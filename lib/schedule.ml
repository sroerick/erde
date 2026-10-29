(* Erde.Schedule -- order the work by cache locality, not by readiness.

   A cache entry is (provider, prefix bytes).  A TTL is measured from the
   start of the request that wrote it, so two turns sharing a prefix but
   running 40 minutes apart pay two writes and read nothing.  The ready queue
   is therefore not a queue: it is a set of cache groups, and a group runs its
   members inside one window.  The first member pays the write, the rest read.

   A prefix under the provider's minimum cannot be cached at all, so it gets
   its own lane and is not made to wait for anyone.

   [capacity] is how many calls fit inside one window at the batch size the
   operator is willing to run at once: a group larger than that splits into
   ceil(n / capacity) windows, one write each. *)

type task = {
  id : string;
  prefix_hash : string;
  prefix_tokens : int;
  provider : string;
  ttl_s : int;
  min_prefix_tokens : int;
  priority : int;  (* lower runs first *)
}

type batch = {
  prefix_hash : string;
  provider : string;
  ids : string list;
  opens_at : float;
  closes_at : float;
  cacheable : bool;
  writes : int;
  reads : int;
}

let cacheable t = t.prefix_tokens >= t.min_prefix_tokens

let compare_task a b = compare (a.priority, a.id) (b.priority, b.id)

let chunks n xs =
  let rec go acc cur k = function
    | [] -> List.rev (if cur = [] then acc else List.rev cur :: acc)
    | x :: rest -> if k = 0 then go (List.rev cur :: acc) [ x ] (n - 1) rest else go acc (x :: cur) (k - 1) rest
  in
  if n <= 0 then [ xs ] else go [] [] n xs

let plan ?(capacity = 64) ~now tasks =
  let capacity = if capacity < 1 then 1 else capacity in
  (* group by the cache key: provider + prefix *)
  let key (t : task) = (t.provider, t.prefix_hash) in
  let groups : (string * string, task list) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun (t : task) ->
      let k = key t in
      Hashtbl.replace groups k (t :: (try Hashtbl.find groups k with Not_found -> [])))
    tasks;
  let entries = Hashtbl.fold (fun k v acc -> (k, List.sort compare_task v) :: acc) groups [] in
  (* deterministic order: by the best priority in the group, then the key *)
  let entries =
    List.sort
      (fun ((p1, h1), v1) ((p2, h2), v2) ->
        let best = function t :: _ -> t.priority | [] -> max_int in
        compare (best v1, p1, h1) (best v2, p2, h2))
      entries
  in
  let opens = ref now in
  List.concat_map
    (fun ((provider, prefix_hash), members) ->
      let head = List.hd members in
      let cacheable_group = cacheable head in
      let pieces = if cacheable_group then chunks capacity members else List.map (fun t -> [ t ]) members in
      List.map
        (fun piece ->
          let ids = List.map (fun t -> t.id) piece in
          let b =
            {
              prefix_hash;
              provider;
              ids;
              opens_at = !opens;
              closes_at = !opens +. float_of_int head.ttl_s;
              cacheable = cacheable_group;
              (* nothing is written under the minimum: those calls are full
                 price input reads, not cache writes *)
              writes = if cacheable_group then 1 else 0;
              reads = if cacheable_group then max 0 (List.length piece - 1) else 0;
            }
          in
          if cacheable_group then opens := b.closes_at +. 1.0 else opens := !opens +. 1.0;
          b)
        pieces)
    entries

(* What the ordering bought, as the numbers the operator needs to see. *)
let summary batches =
  let tasks = List.fold_left (fun a b -> a + List.length b.ids) 0 batches in
  let writes = List.fold_left (fun a b -> a + b.writes) 0 batches in
  let reads = List.fold_left (fun a b -> a + b.reads) 0 batches in
  let cacheable_batches = List.length (List.filter (fun b -> b.cacheable) batches) in
  let uncached = List.filter (fun b -> not b.cacheable) batches in
  let uncached_tasks = List.fold_left (fun a b -> a + List.length b.ids) 0 uncached in
  Printf.sprintf
    "tasks=%d batches=%d cacheable_batches=%d writes=%d reads=%d uncacheable_tasks=%d"
    tasks (List.length batches) cacheable_batches writes reads uncached_tasks
