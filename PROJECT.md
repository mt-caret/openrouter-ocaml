# openrouter-ocaml — project status

A typed Async client for the OpenRouter API. This document captures the state
of the library as of this session — what's implemented, what's tested, what's
deliberately out of scope, and what's left to do.

## Status

The library covers the full surface of `POST /api/v1/chat/completions`,
`POST /api/v1/embeddings`, `GET /api/v1/models`, and `GET /api/v1/generation`,
including every documented request parameter, every response shape we've seen
in the wild, and the OpenRouter-specific extensions (server tools, plugins,
provider routing, prompt caching annotations, app-attribution headers).

`dune build`, `dune runtest`, `dune fmt` are clean. `example/openrouter_api_example.exe`
provides a CLI exercising every feature; the live sweep across 10
provider/model combinations passes 20/20 (each in both streaming and
non-streaming form).

## Library surface

### `Completions.Request`

Phantom-typed so the type system enforces consistency between the request
shape and the HTTP entry point that consumes it:

```ocaml
type 'tag t                            (* abstract, phantom-tagged *)
val create          : ... -> [ `Non_streaming ] t
val create_streaming: ... -> [ `Streaming ] t
```

`Completions.create` requires `[`Non_streaming] Request.t`;
`Completions.create_stream` requires `[`Streaming] Request.t`. Mixing them is
a compile error. Serialization goes through
`[%jsonaf_of: [ `Non_streaming ] Request.t]` — ppx threads a dummy converter
for the phantom tag.

Request fields covered:

- **Sampling**: `temperature`, `top_p`, `top_k`, `min_p`, `top_a`,
  `frequency_penalty`, `presence_penalty`, `repetition_penalty`,
  `logit_bias`, `seed`, `stop`, `max_tokens`, `max_completion_tokens`, `n`
  (skipped — not documented for OpenRouter), `verbosity`, `logprobs`,
  `top_logprobs`.
- **Output shape**: `response_format` (`json_object` | `json_schema`),
  `structured_outputs`, `modalities`, `stream_options`, `service_tier`.
- **Reasoning**: `Reasoning.t` with `effort` (`Effort.t` variant), `max_tokens`,
  `exclude`, `enabled`, `summary`. `Reasoning.create` validates effort/max_tokens
  are not both set (per OpenRouter contract).
- **Tools**: `Tool.t` is a variant — `Function | Web_search | Web_fetch |
  Datetime | Image_generation | Search_models`. Smart constructors
  `Tool.function_`, `Tool.web_search`, etc. OpenRouter server-tool payloads
  use the current `parameters` wrappers.
- **Tool choice**: `Auto | None_ | Required | Specific` (encodes as bare
  string or object — handled manually since it doesn't fit the
  `Json_helper.Make_tagged_union` pattern).
- **Plugins**: `Plugin.t` variants — `Web | File_parser | Response_healing |
  Context_compression | Auto_router | Moderation | Pareto_router`.
- **Routing**: `models` (fallback list), `transforms`, `Provider.t` typed
  record with `order`, `allow_fallbacks`, `require_parameters`,
  `data_collection`, `zdr`, `only`, `ignore`, `quantizations`, `sort`,
  `max_price`, `preferred_min_throughput`, `preferred_max_latency`,
  `enforce_distillable_text`. `Provider.empty` + `Provider.is_empty` via
  derived `equal`.
- **Multimodal content parts**: `Text`, `Image_url`, `File`, `Input_audio`
  (base64 + format), `Video_url`. Each variant carries an optional
  `Cache_control.t` (Anthropic-style ephemeral prompt caching).

### `Completions.Response` / `Stream_chunk`

Every documented response field is parsed:

- `Choice.t` with `Logprobs.t` (full tree: `Token.t` with nested
  `Top_logprob.t` list), `finish_reason`, `native_finish_reason`,
  `Message.t` (with `reasoning_details` covering Anthropic's `text + signature`
  shape, OpenAI's encrypted `data` shape, DeepSeek's `text`-only shape).
- `Usage.t` with `Prompt_tokens_details.t` (cached/cache-write/audio/video
  tokens), `Cost_details.t`, `Completion_tokens_details.t`,
  `Server_tool_use.t`.
- `Stream_chunk.Choice.t` carries an optional `error : Jsonaf.t option` and
  an `is_error : t -> bool` predicate for mid-stream upstream failures.
  Streaming chunks also parse top-level `debug` and `delta.audio`.

All response records carry `[@@jsonaf.allow_extra_fields]` so OpenRouter
adding new fields doesn't break parsing.

### Other endpoints

- `Embeddings.create`
- `Models.list`
- `Generation.get` — polls `GET /api/v1/generation?id=…`, returns
  `Stats.t option` (None on 404 while not yet materialized).
- `Api_error.t` — `code` is `Int of int | String of string`, `metadata` is
  `Jsonaf.t option`.

Each endpoint module exposes `For_testing.{response,stats,...}_of_jsonaf` so
fixtures can round-trip without `[@@deriving of_jsonaf]` leaking the wire
format into the public surface.

### Headers

`Http.App_info.t` covers `HTTP-Referer`, `X-Title`,
`X-OpenRouter-Categories`, `X-OpenRouter-Cache`,
`X-OpenRouter-Experimental-Metadata`, plus an `extra : (string * string) list`
escape hatch. Threaded as `?app_info` through every entry point.

### Capture hooks

Each endpoint takes a `?on_response_body` / `?on_stream_chunk` callback
invoked with the raw bytes before parsing — used by the example's
`-log-request-to`, `-log-response-to`, and `-log-stream-to` flags to capture
request/response fixtures and live-probe artifacts.

## Test infrastructure

```
test/
├── dune
├── test_request_serialization.ml      # JSON-shape snapshots (10 tests)
├── fixture_helpers.ml                  # shared fixture parsing helpers
├── test_*_fixtures.ml                  # fixture-based parsing by shape
└── responses/
    ├── *.json                          # captured response bodies
    └── *.ndjson                        # captured stream chunks (one per line)
```

**Request-side snapshots** (`test_request_serialization.ml`): build a
request via the smart constructors, snapshot the JSON we'd send. Currently
10 tests covering minimal, strict function-tool + tool_choice, multi-message
tool result chains, streaming + web-search plugin, file attachment,
file-parser plugin, audio + video, content-part cache_control, full provider
routing (including distillable/audio-price routing), and current OpenAPI chat
knobs/server tools.

**Response-side fixtures** (`test_*_fixtures.ml`): load real captured
JSON/NDJSON through `fixture_helpers.ml`, parse via endpoint-specific
`For_testing` parsers, snapshot the sexp or a compact summary for large
fixtures. 17 tests including:

- Bedrock with `system_fingerprint: null` (regression guard for the original
  parser bug),
- OpenAI logprobs (full `Logprobs.t` shape),
- Anthropic prompt-cache hit (`cached_tokens > 0`),
- Three reasoning shapes (Anthropic text+signature, DeepSeek text-only,
  OpenAI encrypted-data),
- Web search annotations,
- Image output (Gemini),
- Five streaming variants (simple, reasoning chunks, with usage, debug
  `echo_upstream_body`, audio output),
- Embeddings, models list (367-model summary), generation stats,
  Api_error.

Tagged-union helpers and the variant-stringable functor (see *Internal
abstractions* below) get inline expect-tests in `lib/completions.ml` for
their respective behaviours.

## Internal abstractions

- `Json_helper.Make_string_variant (T)` — derives `Stringable.S` and
  `Jsonaf.Jsonafable.S` for nullary variants whose JSON wire form is a
  bare string. Used by `Reasoning.Effort`, `Verbosity`, `Pdf_engine`,
  `Provider.Sort`, `Provider.Data_collection`. Recovers the constructor name
  from `[%sexp_of: T.t]` (an `Atom`), normalises with
  `lowercase + tr '_' '-' + rstrip '-'` (handles `None_` → `none`,
  `Pdf_text` → `pdf-text`).

- `Json_helper.Make_tagged_union (T)` — derives `Jsonaf.Jsonafable.S` for
  objects with a single tag field (`"type"` or `"id"`) selecting how the
  rest is parsed. Per-variant `codec` says whether the payload is absent,
  spread inline, or nested under a sub-key (`Json_helper.nested`); the wire
  tag comes from `Typed_variant.name` by default (or kebab-case / a custom
  function). Used by `Tool`, `Plugin`, `Content_part`, `Response_format`.

## Recently-completed work

In rough chronological order:

1. **Scalar request fields + Reasoning extension** — `top_k`, `min_p`,
   `top_a`, `logit_bias`, `logprobs`, `top_logprobs`, `max_completion_tokens`,
   `verbosity`, `modalities`, `stream_options`, `service_tier`, `models`,
   `transforms`. Reasoning sub-fields (`effort`, `exclude`, `enabled`).
2. **Headers + error handling + /generation** — `App_info.t`,
   `Api_error.{code,metadata}`, new `Generation` module.
3. **Defensive parsing fixes** — relaxed `Reasoning_detail` required
   fields, fixed `system_fingerprint: null`, added
   `[@@jsonaf.allow_extra_fields]` everywhere, properly modelled `Logprobs.t`,
   relaxed `Image.t.index`.
4. **Fixture-based test directory** — `test/`, `For_testing` submodules,
   capture hooks on every endpoint, 17 fixture-driven tests.
5. **`String_variant.Make` functor** — replaces hand-rolled
   to_string/of_string for nullary variants.
6. **`Tagged_union.{to,of}_jsonaf`** — replaces hand-rolled `match
   Jsonaf.member "type"` boilerplate in 4 sites.
7. **Provider routing** — typed `Provider.t` record, every documented
   sub-field, `empty` + `is_empty`.
8. **Server-side tools** — `Tool.t` refactored to variant covering
   `openrouter:web_search/web_fetch/datetime/image_generation` and
   `openrouter:experimental__search_models`.
9. **Cache control on content parts**, `Plugin.Response_healing`,
   `Plugin.Context_compression`.
10. **Audio + video content parts** — `Input_audio` (base64 + format),
    `Video_url`. Example auto-detects mime from extension.
11. **Mid-stream error helper** — `Stream_chunk.Choice.error : Jsonaf.t
    option` field + `is_error` predicate.
12. **Type-bound logic migration** — `Provider.is_empty` (via derived
    `equal`), `Reasoning.create` validates effort/max_tokens exclusivity.
13. **`Request.create` smart constructor** — collapses 30-line record
    literals to 3–5 line calls.
14. **Phantom-typed `'tag Request.t`** — type system enforces
    streaming/non-streaming consistency. JSON-shape inline tests moved to
    `test/test_request_serialization.ml`.
15. **Current OpenRouter chat extensions** — top-level `cache_control`,
    `debug`, `metadata`, `user`, `session_id`, `route`, `trace`, `audio`,
    `image_config`, reasoning `summary`, strict tools, provider distillation
    routing/audio max price, server-tool `parameters`, and auto-router /
    moderation / pareto-router plugins.
16. **Live response fixtures for new shapes** — cache-hit usage, debug stream
    chunks with `choices=[]`, and streaming audio output with `delta.audio`
    and `audio_tokens`.

## Out of scope (per project decisions)

- **Other endpoints**: `/credits`, `/key`, `/responses` (beta), audio/video
  generation, account management. Skipped per user direction.
- **OAuth, BYOK, management API keys**. Skipped per user direction.

## Remaining work

### OpenAPI audit (2026-05-12)

The live `https://openrouter.ai/openapi.json` surface is broader than this
library's current inference-client scope. Implemented endpoints remain:
`/chat/completions`, `/embeddings`, `/models`, and `/generation`.

The spec also lists management and adjacent-product endpoints that are not
covered here: activity, credits, key/auth/key management, guardrails,
organization/workspace management, provider/endpoints discovery beyond model
listing, rerank, audio speech/transcription, video generation, Anthropic
Messages, and the beta `/responses` API. Treat those as explicit future work,
not accidental omissions in the current chat client.

The chat request shape has been refreshed against the current spec with:
top-level `cache_control`, `debug`, `metadata`, `user`, `session_id`, `route`,
`trace`, `audio`, `image_config`, reasoning `summary`, provider
`enforce_distillable_text`, provider max audio price, current server-tool
`parameters` wrappers, and the `auto-router` / `moderation` / `pareto-router`
plugins. The request snapshot
`"request: current OpenAPI chat knobs + server tools"` is the regression guard
for these wire shapes.

### Documentation

- README.md has been rewritten around `Request.create`, phantom streaming
  tags, server tools/plugins, multimodal content, capture hooks, and the
  current endpoint coverage boundary. Keep it in sync when expanding the
  surface beyond inference endpoints.

### Live verification (2026-05-12)

The example binary now has flags for every chat-field addition above, and live
probes were run through the binary with request logging enabled. Verified
paths include:

- `openrouter:experimental__search_models`, `openrouter:web_search`,
  `openrouter:web_fetch`, `openrouter:datetime`, and
  `openrouter:image_generation` request serialization. Search/web fetch/search
  tools returned successful chat responses; image-generation tool requests were
  accepted but the server tool returned model/tool errors without an image.
- `auto-router`, `moderation`, `pareto-router`, `metadata`, `user`,
  `session_id`, and `trace`.
- Strict function tools (`strict: true`) with a live `tool_calls` response.
- Top-level `cache_control` and content-part cache control against direct
  Anthropic routing. Large repeated probes produced both `cache_write_tokens`
  and `cached_tokens`; `cache_hit.json` captures the cache-hit response.
- `debug.echo_upstream_body` streaming, captured in
  `debug_echo_upstream_body.ndjson`.
- Audio output with `modalities=["text"; "audio"]` and `audio.format="pcm16"`
  on `openai/gpt-audio-mini`; `stream_audio_output.ndjson` captures
  `delta.audio` transcript/data/expires-at chunks and nonzero audio tokens.
- `provider.enforce_distillable_text` on `openrouter/auto`, Qwen, Llama,
  Mistral, and DeepSeek distill models. The same flag correctly rejected an
  Inclusion model with "No endpoints found with models that allow text
  distillation."
- `image_config` with Gemini image output. The pre-existing image-output
  fixture already covers the response shape, so no duplicate model-only
  fixture was added.

### Hard-to-reproduce edge cases

These have lib-side support but no live fixture because the relevant
upstream behaviour isn't reliably triggerable through OpenRouter:

- **Mid-stream error.** `Stream_chunk.Choice.is_error` and the optional
  `error` field exist. Reproducing requires an upstream provider failure
  mid-generation; we'd want a fixture showing `finish_reason: "error"` on
  a stream chunk.
- **Refusal field.** `Message.refusal : string option` is parsed but no
  fixture has it populated. Triggering needs a provider-specific safety
  refusal.
- **`usage.server_tool_use.web_search_requests > 0`.** The structure parses,
  but the live OpenAI/Azure response from `openrouter:web_search` doesn't
  populate this counter — possibly provider-dependent. We have a fixture
  showing the (empty) field; a populated one would be nice.

### Possible future investigations

- **Compile-fail tests.** The phantom-type guarantees are nice but
  un-asserted in the test suite. A `dune` rule that compiles a known-broken
  fixture and asserts it fails would close the loop. (Requires a bit of
  dune machinery; not standard practice in this codebase.)
- **More provider/model coverage in the live sweep.** Currently 10
  providers; expanding would catch shape drift from new providers. Cheap
  to extend `/tmp/probe-models.sh` if it ever falls into version control.
