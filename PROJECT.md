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
  `exclude`, `enabled`. `Reasoning.create` validates effort/max_tokens are
  not both set (per OpenRouter contract).
- **Tools**: `Tool.t` is a variant — `Function | Web_search | Web_fetch |
  Datetime | Image_generation`. Smart constructors `Tool.function_`,
  `Tool.web_search`, etc.
- **Tool choice**: `Auto | None_ | Required | Specific` (encodes as bare
  string or object — handled manually since it doesn't fit the
  `Json_helper.Make_tagged_union` pattern).
- **Plugins**: `Plugin.t` variants — `Web | File_parser | Response_healing |
  Context_compression`.
- **Routing**: `models` (fallback list), `transforms`, `Provider.t` typed
  record with `order`, `allow_fallbacks`, `require_parameters`,
  `data_collection`, `zdr`, `only`, `ignore`, `quantizations`, `sort`,
  `max_price`, `preferred_min_throughput`, `preferred_max_latency`. `Provider.empty`
  + `Provider.is_empty` via derived `equal`.
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
`-log-response-to` / `-log-stream-to` flags to capture fixtures.

## Test infrastructure

```
test/
├── dune
├── test_request_serialization.ml      # JSON-shape snapshots (9 tests)
├── test_response_parsing.ml            # fixture-based parsing (14 tests)
└── responses/
    ├── *.json                          # captured response bodies
    └── *.ndjson                        # captured stream chunks (one per line)
```

**Request-side snapshots** (`test_request_serialization.ml`): build a
request via the smart constructors, snapshot the JSON we'd send. Currently
9 tests covering minimal, function-tool + tool_choice, multi-message tool
result chains, streaming + web-search plugin, file attachment, file-parser
plugin, audio + video, cache_control, full provider routing.

**Response-side fixtures** (`test_response_parsing.ml`): load real captured
JSON, parse via `Completions.For_testing.response_of_jsonaf`, snapshot the
sexp. 14 tests including:

- Bedrock with `system_fingerprint: null` (regression guard for the original
  parser bug),
- OpenAI logprobs (full `Logprobs.t` shape),
- Three reasoning shapes (Anthropic text+signature, DeepSeek text-only,
  OpenAI encrypted-data),
- Web search annotations,
- Image output (Gemini),
- Three streaming variants (simple, reasoning chunks, with usage),
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
   capture hooks on every endpoint, 14 fixture-driven tests.
5. **`String_variant.Make` functor** — replaces hand-rolled
   to_string/of_string for nullary variants.
6. **`Tagged_union.{to,of}_jsonaf`** — replaces hand-rolled `match
   Jsonaf.member "type"` boilerplate in 4 sites.
7. **Provider routing** — typed `Provider.t` record, every documented
   sub-field, `empty` + `is_empty`.
8. **Server-side tools** — `Tool.t` refactored to variant covering
   `openrouter:web_search/web_fetch/datetime/image_generation`.
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

## Out of scope (per project decisions)

- **Other endpoints**: `/credits`, `/key`, `/responses` (beta), audio/video
  generation, account management. Skipped per user direction.
- **OAuth, BYOK, management API keys**. Skipped per user direction.

## Remaining work

### Documentation

- **README.md is stale.** It mentions only chat/embeddings/list-models and
  doesn't reflect the smart constructors, phantom types, plugin/server-tool
  surface, or cache control. Worth a substantial rewrite.
- A short *quickstart* example in the README built around `Request.create` +
  `Completions.create` would be useful (the existing CLI example is more of
  a demo than a usage tutorial).

### Hard-to-reproduce edge cases

These have lib-side support but no live fixture because the relevant
upstream behaviour isn't reliably triggerable through OpenRouter:

- **Prompt cache hits.** `cache_control` ships correctly on content parts;
  the request-side wire format is regression-tested. But OpenRouter doesn't
  appear to pass the directive through to providers reliably — the live
  test never produced `usage.prompt_tokens_details.cached_tokens > 0`, even
  via `provider.only=["Anthropic"]` (which routes around Bedrock). Worth
  re-checking periodically; the test would be a fixture with a populated
  `cached_tokens` field.
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

### Smaller polish

- **`list_or_none` helper duplicated** in `example/openrouter_api_example.ml`
  (defined inside the provider construction block AND the main request
  construction block). Lift to a top-level helper.
- **Type aliases for clarity**: `Request.Non_streaming.t = [`Non_streaming]
  Request.t` and `Request.Streaming.t = [`Streaming] Request.t` would
  let users write `Request.Non_streaming.t` instead of the raw polymorphic
  variant. Trivial addition.
- **`Stream_options.create`** for symmetry with the other smart
  constructors. One-field type so the value is small, but consistency is
  free.

### Possible future investigations

- **Compile-fail tests.** The phantom-type guarantees are nice but
  un-asserted in the test suite. A `dune` rule that compiles a known-broken
  fixture and asserts it fails would close the loop. (Requires a bit of
  dune machinery; not standard practice in this codebase.)
- **Audio fixture.** We confirmed audio input works end-to-end against
  Gemini, but didn't capture a fixture showing the audio-input *response*
  shape. A small WAV → response capture would be nice.
- **More provider/model coverage in the live sweep.** Currently 10
  providers; expanding would catch shape drift from new providers. Cheap
  to extend `/tmp/probe-models.sh` if it ever falls into version control.
