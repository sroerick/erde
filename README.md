# erde

Erde Lang: the substrate around tuna.  One microturn, journaled, cache-aware.

Design: `tuna/docs/substrate-design.md` on `factory/substrate-design-1029`
(commit `7a5245e`) is canonical.  This tree is the week-1 walking skeleton
from that document.

## What exists

- `lib/config.ml` -- instance config (`erde.json`, no secrets) plus `llm.env`
  (mode 600, one key per provider, referenced by name).  The key value never
  enters the config type, a log line, or the journal; `Config.redact` scrubs
  loaded values out of any string on the way back.
- `lib/config.ml` routing table -- candidate lists ordered by cache affinity
  then cost, with a provider retired for a lane dropped and named out loud.
- `lib/segments.ml` -- the prompt as an append-only list ordered by
  volatility (framework, tools, claims, project, artifacts, task, scratch),
  with the order enforced and the canonical rendering length-prefixed so the
  prefix hash is stable.  `Claim_set` sorts claims canonically, so the same
  claim set always hashes the same.
- `lib/cache_strategy.ml` -- cache is a provider property: explicit
  breakpoints for Anthropic, implicit stable head for OpenAI, nothing at all
  under the provider's minimum cacheable prefix.
- `lib/wire.ml` -- the two request/response shapes and the per-wire usage
  accounting (Anthropic excludes cache counters from `input_tokens` and
  charges the write; OpenAI includes cached tokens in `prompt_tokens` and
  does not).
- `lib/journal.ml` -- append-only JSONL rows (facts) with room for verdicts
  (corrections), carrying provider, cache counters and cost; `hit_rates`
  prints the per-provider cache share.
- `lib/http_client.ml` -- one request, one response, TLS via the system CA
  store.
- `lib/turn.ml` -- one microturn: route, plan, call, parse, journal.
- `bin/erde.ml` -- `providers`, `plan` (dry run, spends nothing), `turn`,
  `journal`, `get`.

## Use

```
dune build
dune runtest
_build/default/bin/erde.exe providers -c erde.json
_build/default/bin/erde.exe plan -c erde.json -m glm-5.3-flash -t "port the list prims"
cp llm.env.example llm.env && chmod 600 llm.env
```

## Next (from the design)

Tool hosts behind a grant check; the tuna claim fetch (`/api/claims` on the
authority side, with the task-claim lease); SSE streaming with tool-call
deferral; the gregor journal-row diff as the acceptance test; move the box
to vultr next to the keys.
