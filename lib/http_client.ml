(* Erde.Http_client -- the gateway's outbound path.

   Deliberately tiny: one request, one response, no pool yet, and no host the
   caller did not name.  TLS is on for https, with the system CA store.

   The response is consumed by one Eio.Buf_read parser: status line, headers,
   then a body whose framing (content-length or chunked) decides the reader. *)

(* TLS needs a seeded RNG before the first handshake; the mirage-crypto
   default generator is that seed. *)
let () = Mirage_crypto_rng_unix.use_default ()

let tls_config =
  lazy
    (match Ca_certs_nss.authenticator () with
    | Error (`Msg m) -> failwith ("erde: cannot load the CA store: " ^ m)
    | Ok auth -> (
        match Tls.Config.client ~authenticator:auth () with
        | Ok c -> c
        | Error (`Msg m) -> failwith ("erde: cannot build the tls config: " ^ m)))

let ( let* ) = Eio.Buf_read.bind
let ( let+ ) p f = Eio.Buf_read.map f p

let strip_cr s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

let line = let+ l = Eio.Buf_read.line in strip_cr l

let header_value headers name =
  let lname = String.lowercase_ascii name in
  List.find_map
    (fun (k, v) ->
      if String.lowercase_ascii (String.trim k) = lname then Some (String.trim v) else None)
    headers

let status_of_line l =
  match String.split_on_char ' ' l with
  | _ :: code :: _ -> ( try int_of_string code with _ -> 0)
  | _ -> 0

let headers =
  let rec go acc =
    let* l = line in
    if l = "" then Eio.Buf_read.return (List.rev acc)
    else
      match String.index_opt l ':' with
      | None -> go acc
      | Some i ->
          let k = String.sub l 0 i in
          let v = String.sub l (i + 1) (String.length l - i - 1) in
          go ((k, v) :: acc)
  in
  go []

(* A parser carries state, so these are functions, not values: a top-level
   parser value holding a Buffer would let one response's bytes bleed into the
   next one parsed by the same process. *)
let chunked_body () =
  let out = Buffer.create 1024 in
  let rec go () =
    let* size_line = line in
    let size_field =
      match String.index_opt size_line ';' with
      | Some i -> String.sub size_line 0 i
      | None -> size_line
    in
    let n = try int_of_string ("0x" ^ String.trim size_field) with _ -> 0 in
    if n = 0 then
      (* The terminating chunk is "0" and the trailer section ends with an
         empty line.  Leaving that line unread is what makes the whole reply
         parse as "unexpected data after parsing" on the first chunked (i.e.
         streaming) response.  A server that closes right at the zero chunk
         without the trailer line is tolerated, not failed on. *)
      let* next = Eio.Buf_read.peek_char in
      match next with
      | None -> Eio.Buf_read.return (Buffer.contents out)
      | Some _ ->
          let* _trailer_end = line in
          Eio.Buf_read.return (Buffer.contents out)
    else
      let* chunk = Eio.Buf_read.take n in
      let* _crlf = line in
      Buffer.add_string out chunk;
      go ()
  in
  go ()

let response () =
  let* status_line = line in
  let* hs = headers in
  let* body =
    match header_value hs "transfer-encoding" with
    | Some te when String.lowercase_ascii te = "chunked" -> chunked_body ()
    | _ -> (
        match header_value hs "content-length" with
        | Some cl -> (
            match int_of_string_opt (String.trim cl) with
            | Some n when n >= 0 -> Eio.Buf_read.take n
            | _ -> Eio.Buf_read.take_all)
        | None -> Eio.Buf_read.take_all)
  in
  Eio.Buf_read.return (status_of_line status_line, body)

let render_request ~meth ~path ~headers ~body =
  let b = Buffer.create 1024 in
  Buffer.add_string b (Printf.sprintf "%s %s HTTP/1.1\r\n" meth path);
  List.iter (fun (k, v) -> Buffer.add_string b (Printf.sprintf "%s: %s\r\n" k v)) headers;
  (match body with
  | Some s -> Buffer.add_string b (Printf.sprintf "Content-Length: %d\r\n" (String.length s))
  | None -> ());
  Buffer.add_string b "\r\n";
  (match body with Some s -> Buffer.add_string b s | None -> ());
  Buffer.contents b

let request ~env ~sw ?(headers = []) ?body ~meth ~url () =
  let uri = Uri.of_string url in
  let scheme = match Uri.scheme uri with Some s -> String.lowercase_ascii s | None -> "http" in
  match Uri.host uri with
  | None -> Error (Printf.sprintf "no host in url %S" url)
  | Some host ->
      let port = match Uri.port uri with Some p -> p | None -> if scheme = "https" then 443 else 80 in
      let path = (match Uri.path_and_query uri with "" -> "/" | p -> p) in
      let net = Eio.Stdenv.net env in
      let addrs = Eio.Net.getaddrinfo_stream net host ~service:(string_of_int port) in
      (match addrs with
      | [] -> Error (Printf.sprintf "cannot resolve %s" host)
      | addr :: _ -> (
          try
            let raw = Eio.Net.connect ~sw net addr in
            (* HTTP/1.1 requires Host; Cloudflare answers 400 without it. *)
            let host_header =
              if (scheme = "https" && port = 443) || (scheme = "http" && port = 80) then host
              else Printf.sprintf "%s:%d" host port
            in
            let headers = ("host", host_header) :: ("connection", "close") :: headers in
            let req = render_request ~meth ~path ~headers ~body in
            let exchange
                (flow : [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t)
                ~half_close =
              Eio.Flow.copy_string req flow;
              (* On plain TCP a send-half-close is a genuine half-close and some
                 servers want it.  Over TLS the same call sends close_notify,
                 which relayd (and others) read as "connection over" and answer
                 with EOF: the request is never seen.  So: no half-close on TLS,
                 where "connection: close" already ends the exchange. *)
              if half_close then Eio.Flow.shutdown flow `Send;
              Eio.Buf_read.parse_exn ~max_size:(32 * 1024 * 1024) (response ()) flow
            in
            let status, body =
              if scheme = "https" then
                (match Domain_name.of_string host with
                | Error (`Msg m) -> failwith m
                | Ok d -> (
                    match Domain_name.host d with
                    | Ok h ->
                        exchange
                          (Tls_eio.client_of_flow (Lazy.force tls_config) ~host:h raw)
                          ~half_close:false
                    | Error (`Msg m) -> failwith m))
              else exchange raw ~half_close:true
            in
            Ok (status, body)
          with exn -> Error (Printf.sprintf "%s: %s" host (Printexc.to_string exn))))

(* A testable seam: parse a whole response out of a string, so the framing
   rules (content-length, chunked, and the terminating chunk's trailer line)
   have a regression test that does not need a socket. *)
let parse_response_string (s : string) = Eio.Buf_read.parse_string (response ()) s
