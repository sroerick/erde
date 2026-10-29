(* Erde.Cache_strategy -- cache is a provider property, not a substrate
   constant.

   Anthropic wants explicit cache_control breakpoints placed by the caller;
   OpenAI is implicit-prefix and only wants a byte-stable head.  The plan is
   computed here, in the gateway, next to the keys, because it depends on the
   provider row (mode, TTL, minimum cacheable prefix) rather than on anything
   the turn engine knows.

   The economics that decide it:
     5m write 1.25x, 1h write 2x, read 0.1x
     -> a 5-minute entry pays for itself after one read, a 1-hour entry after
        two.  Below the provider's minimum prefix a write is a pure loss: it
        can never be read. *)

type plan = {
  provider : string;
  mode : Config.cache_mode;
  breakpoints : int list;  (* segment indices, oldest first; [] in implicit mode *)
  head : string;           (* the prefix hash, i.e. the cache key *)
  ttl_s : int;
  prefix_tokens : int;
  reason : string;
}

let shared_kinds = [ Segments.Framework; Segments.Tools; Segments.Claims; Segments.Project ]

let take_last n xs =
  let len = List.length xs in
  if len <= n then xs else List.filteri (fun i _ -> i >= len - n) xs

let shared_indices (segs : Segments.t) =
  segs
  |> List.mapi (fun i (s : Segments.segment) -> (i, s.Segments.kind))
  |> List.filter (fun (_, k) -> List.mem k shared_kinds)
  |> List.map fst

let plan (p : Config.provider) (segs : Segments.t) =
  let cache = p.Config.cache in
  let prefix_tokens = Segments.tokens segs in
  let head = Segments.prefix_hash segs in
  let base mode breakpoints reason =
    {
      provider = p.Config.name;
      mode;
      breakpoints;
      head;
      ttl_s = cache.Config.ttl_s;
      prefix_tokens;
      reason;
    }
  in
  match cache.Config.mode with
  | Config.Implicit ->
      base Config.Implicit []
        "implicit-prefix provider: nothing to place, keep the head byte-stable"
  | Config.Explicit ->
      if prefix_tokens < cache.Config.min_prefix_tokens then
        base Config.Explicit []
          (Printf.sprintf
             "prefix %d tokens is under the %d-token minimum: a write costs 1.25x and can never be read"
             prefix_tokens cache.Config.min_prefix_tokens)
      else
        let bps = take_last 4 (shared_indices segs) in
        base Config.Explicit bps
          (Printf.sprintf "explicit breakpoints after each shared segment (last %d), ttl %ds"
             (List.length bps) cache.Config.ttl_s)
