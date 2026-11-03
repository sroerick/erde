# erde

Erde Lang: the substrate around tuna.  One microturn, journaled, cache-aware.

Repo: `git@wyo.town:/srv/git/erde.git` (remote `wyo`).  Clone with
`git clone roerick@wyo.town:/srv/git/erde.git`.  cgit on `git.wyo.town` does
not list it yet, which needs a repo entry in the server's cgitrc.

Design: `tuna/docs/substrate-design.md` on `factory/substrate-design-1029`
(commit `7a5245e`) is canonical.  This tree is the week-1 walking skeleton
from that document.

## What exists

- `lib/config.ml` -- instance config (`erde.json`, no secrets) plus `llm.env`
  (mode 600, one key per provider, referenced by name).  The key value never
  enters the config type, a log line, or the journal; `Config.redact` and
  `Authority.scrub` remove loaded secrets from any string on the way back.
  The routing table orders candidates by cache affinity then cost, drops a
  provider retired for a lane, and names the retirement in its reason.
- `lib/segments.ml` -- the prompt as an append-only list ordered by
  volatility (framework, tools, claims, project, artifacts, task, scratch),
  with the order enforced and the canonical rendering length-prefixed so the
  prefix hash is stable.  `Claim_set` sorts claims canonically, so the same
  claim set always hashes the same.
- `lib/cache_strategy.ml` -- cache is a provider property: explicit
  breakpoints for Anthropic, an implicit stable head for OpenAI, and nothing
  at all below the provider's minimum cacheable prefix.
- `lib/wire.ml` -- both wire shapes and their differing usage accounting
  (Anthropic excludes cache counters from `input_tokens` and charges the
  write; OpenAI includes cached tokens in `prompt_tokens` and does not).
- `lib/authority.ml` -- the tuna client, and the only place a bearer lives.
  The store is HTTP-only: reads and writes cross the grant boundary, never a
  linked library.  `claims` is a prefix list plus a get per entry, where the
  value is the canonical ternary string and the citation hash is the source.
- `lib/grants.ml` -- the gateway's own gate, checked before a tool runs.
  Prefix matching follows tuna's rule verbatim (a raw string prefix; an empty
  prefix is refused because it reads as scope while meaning everything), and a
  revoked row is a fact that is simply not a capability.
- `lib/tools.ml` -- the tool host: `tree.get`, `tree.list`, `tree.put`, plus a
  local `echo` for tests, each gated before anything leaves the process, each
  described to the model in the shape its wire expects.  Arguments are typed
  JSON; nothing builds a shell string.  `Wire.assemble_stream` folds either
  provider's transcript into the same response record, tool-call fragments
  included, and a streaming request asks for usage in the final chunk.
- `lib/journal.ml` -- append-only JSONL rows carrying provider, cache
  counters, cost and route reason, with `tool` rows for each tool decision and
  `verdict` rows that correct an earlier row by reference.  `hit_rates`
  prints the per-provider cache share.
- `lib/turn.ml` -- one microturn as a bounded loop: route, plan, call, run the
  tools it asks for, feed the results back, up to `max_steps` model calls.
  A refusal is a journal row, not a provider round trip.
- `lib/schedule.ml` -- order the work by cache locality rather than readiness:
  group by (provider, prefix), run a group inside one TTL window so the first
  call writes and the rest read, and give an uncacheable prefix its own lane.
- `lib/prompt.ml` -- the one place a prompt is assembled, as a pure function of
  (policy revision, tools revision, claims, project, task).  Change one claim
  and the prefix hash changes, which is exactly the cache boundary.
- `lib/schedule.ml` -- order the work by cache locality rather than readiness:
  group by (provider, prefix), run a group inside one TTL window so the first
  call writes and the rest read, give an uncacheable prefix its own lane, and
  report what the ordering bought.
- `lib/http_client.ml` -- one request, one response, TLS via the system CA
  store.
- `bin/erde.ml` -- `providers`, `plan`, `claims`, `tool`, `turn`, `journal`,
  `verdict`, `get`.

## Use

```
dune build && dune runtest
E=_build/default/bin/erde.exe
$E providers -c erde.json                       # providers, routes, authority, grants
$E plan -c erde.json -m glm-5.3-flash -t "..."  # dry run: segments, prefix hash, cache plan, route
$E plan -c erde.json -t "..." --policy FILE     # same, with the framework segment read from FILE
$E tool -c erde.json tree.list u.marin.         # one gated tool call, no model
$E claims -c erde.json                          # an identity's claims, rendered into segments
$E turn -c erde.json -m claude-sonnet-5.5 -t "..." --max-steps 3
$E journal -c erde.json --n 20                  # rows plus per-provider hit rates
$E verdict -c erde.json --ref 3 --status refuted --note "the citation no longer resolves"
cp llm.env.example llm.env && chmod 600 llm.env
```

The authority's bearer is named by `authority.token_env` and may come from the
env file, the process environment, or a mode-600 file of your own.

## Verified

- 43 alcotest cases, `dune build` green.
- Live HTTPS GET to `api.openai.com/v1/models` returns 401 with the
  provider's JSON error body.
- Live POST to `api.anthropic.com` with a deliberately bad key returns 401 and
  writes a journal row carrying provider, prefix hash, route reason and status.
- Live `whoami` against `https://tuna.town` with an identity bearer.
- A gated `tree.get` on a path outside every grant is refused before any
  request is sent, and the refusal is a journal row; a `tree.put` with no grant
  row is refused the same way.
- Claim density, measured on the live garden: `cited` 343 prefix tokens,
  `trunc60` 467, `full` 17,679 (51x).

## Next (from the design)

Incremental streaming (the transcript is assembled correctly but read to the
end, so latency is not yet improved); prompt caching verified against a real
billing statement rather than a recorded body; the task-claim lease on the tuna
side; the gregor journal-row diff as the acceptance test; the move to vultr
next to the keys.
