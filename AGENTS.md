# Working notes for agents

This file covers conventions and gotchas that aren't visible from the code
alone. For current status, inspect the worktree, recent commits, and test
suite rather than relying on a separate project-status file. These notes are
project-specific and should be applied alongside any broader agent instructions
configured by the person running the agent.

## Build / test workflow

- `dune build`, `dune runtest`, and `dune fmt` from the project root after
  every substantive edit. None should warn or fail before reporting work done.
  If you use `dune fmt --auto-promote`, inspect the diff before keeping
  promoted changes.
- Expect-test snapshots live in `lib/completions.ml` (3 inline tests for
  validation/parsing-helper logic) and `test/test_*_*.ml` (snapshot
  fixtures). Use `dune runtest --auto-promote` to update snapshots, but
  inspect the diff first — `--auto-promote` is the right tool but blindly
  accepting backtraces or unrelated changes is a foot-gun.
- Tests stay clean even on the slowest machine; nothing currently does
  network or wall-clock work inside `runtest`.
- The README mdx check (root `dune`) is disabled by default: it links a
  bytecode runtime, and the Jane Street v0.18~preview packages reference an
  OxCaml-only runtime symbol (`caml_no_bytecode_impl`) that mainline OCaml's
  bytecode runtime lacks, so it can't link there (native builds are fine). Run
  it with `OPENROUTER_MDX_README=true dune runtest` on a toolchain whose
  bytecode runtime provides the symbol.

## Live testing

- Live calls are part of the workflow. The user provides
  `OPENROUTER_API_KEY` for testing; pass it via env var to the example
  binary:

  ```sh
  OPENROUTER_API_KEY=… dune exec example/openrouter_api_example.exe -- chat \
    -no-stream -model openai/gpt-4o-mini "say 'pong'"
  ```

- A "20-model sweep" (10 providers × {stream, no-stream}) is the default
  smoke test after any parser-side change. If the script isn't lying around
  in `/tmp`, recreate it with the model list in the docstring at the top of
  `test/fixture_helpers.ml`.
- For request-shape additions, exercise the example binary with
  `-log-request-to /tmp/...` and inspect the JSON it actually sends. Prefer
  omitting unset optional list/object fields rather than serializing empty
  lists or objects; OpenRouter rejects some empty values that look harmless,
  such as `modalities: []` and empty provider routing objects.
- Don't commit API keys. If you write a script that uses one, write it to
  `/tmp/` rather than the repo.

## Fixture capture

- The example binary has `-log-response-to PATH` (non-streaming) and
  `-log-stream-to PATH` (streaming, NDJSON). Both work on `chat`,
  `embeddings`, `list-models`, and `generation`.
- New fixtures go under `test/responses/`. Use snake_case names.
- **Dedup principle**: don't add a fixture that just differs by model id —
  these tests are about JSON shapes, not provider coverage. Pick combinations
  that produce a structurally distinct response (different fields populated,
  different sub-record shapes, etc.). The doc comment at the top of
  `test/fixture_helpers.ml` reiterates this.
- Large fixtures (>50KB) — don't snapshot the full sexp; snapshot a summary
  (`count` + `List.hd_exn` of the relevant list) so the test catches
  shape regressions without the snapshot becoming unwieldy. See the
  `models_list` test for the pattern.

## OpenRouter quirks (learned the hard way)

- **`system_fingerprint: null`** on Bedrock-routed responses.
  `[@jsonaf.option]` does NOT tolerate explicit nulls — only handles "field
  absent". Use `[@default None]` for any response-side optional field that
  could come back null. The original parser bug here is captured in
  `bedrock_null_system_fingerprint.json`.
- **SSE keepalive whitespace** leaks into non-streaming response bodies for
  slow upstream calls (`web search`, etc.). `Jsonaf.parse` tolerates leading
  whitespace, so this is a non-issue for parsing, but it makes raw fixtures
  look weird.
- **Reasoning shapes vary by provider**: Anthropic uses `text + signature`,
  OpenAI o-series uses encrypted `data`, DeepSeek uses `text` only and
  `Reasoning_detail.{format,index,type_}` are not always present. All three
  are in the test fixtures.
- **Image output (Gemini)** omits `index` on the image object — keep
  `Image.t.index` as `int option`.
- **Cache hits don't reliably materialize** through OpenRouter's proxy,
  even with `provider.only=["Anthropic"]`. Request-side `cache_control` is
  regression-tested via the inline JSON snapshot; live `cached_tokens > 0`
  is a known TODO.
- **`/api/v1/generation` has a 5–20-second delay** after the originating
  completion finishes. The example's `generation` subcommand polls; tests
  use a captured fixture.
- **xAI Grok was flapping with 503s** during this session — when a live
  test fails for an exotic provider, retry once before assuming a parser
  regression.

## Code patterns specific to this project

### Request-surface changes

Every new OpenRouter request feature should be wired through the full surface:
library types/smart constructors, example CLI flags, request-serialization
expect tests, and a manual example-binary invocation. Use `-log-request-to
/tmp/...` to confirm the outgoing JSON. Add response fixtures only when the
live response has a novel JSON shape; don't add fixtures that only differ by
model id.

Don't translate meaningful explicit values back to `None` just because they
match a default — if a caller supplied a value, serializing it is usually the
least surprising behavior. Empty optional lists/objects are different: omit
them when unset, because OpenRouter rejects some empty values that appear
innocuous.

### API invariants belong in types

Make invalid states unconstructable when the OpenRouter API has mode-specific
rules. Use phantom tags, private records, variants, and smart constructors
rather than documenting "don't set these fields together" in prose. In
particular, streaming-only and non-streaming-only options should be separated
in the type shape instead of sharing one permissive record.

Expose record definitions in `.mli` files only when callers need to construct
or pattern-match them directly. If a record has field-population contracts,
prefer `type t = private { ... }` plus `create`.

### Ppx-derived surfaces

Use ppx derivations in both `.ml` and `.mli` whenever they express the public
surface. In particular, keep `[@@deriving sexp]` exposed for public request and
response types unless there is a deliberate reason to hide parsing or
serialization. Before manually writing a converter signature that used to be
derived, check the relevant ppx docs; `ppx_jsonaf_conv` supports narrower
derivations such as `jsonaf_of` and `of_jsonaf`.

### Example CLI structure

Keep parsing and request construction close to `Command.Param.t` values instead
of collecting raw strings and doing a second parsing pass later. Custom helper
types/values for the example binary should live under `example/`, and if a
custom type earns a module, name the primary type `t`.

Keep request construction separate from execution concerns such as app headers,
logging paths, and fixture capture. Values related to a type should live in
that type's module; avoid accumulating top-level helpers that only make sense
for one module's `t`.

### Forward-compatible response parsing

Every record that's parsed from an API response carries
`[@@jsonaf.allow_extra_fields.log]`. New fields appearing in OpenRouter's
responses don't break parsing; instead the field is logged at `Error` level
(`<module>.t_of_jsonaf: extra fields: <name>`) via `ppx_log`, so unmodeled
fields are observable rather than silently dropped. This needs
`Async_log_kernel.Ppx_log_syntax` in scope, which `open! Async` provides — so
any module defining a response record must `open! Async` even if it otherwise
wouldn't. When adding a new response record, do the same.

In fixture tests these log lines surface in the expect snapshots:
`Fixture_helpers` pins the global log's time source to the epoch (stable
timestamp), captures the lines, and `flush_log` drains them — deduped — after
the parsed value is printed. Call `flush_log` at the end of any test that
parses a fixture outside the `parse_*` helpers.

### `[@jsonaf.option]` vs `[@default None]`

- `[@jsonaf.option]` is for **request-side** option fields (encoded as
  "omit when None").
- `[@default None]` is for **response-side** option fields (parsed via
  `option_of_jsonaf`, which tolerates `null`).
- Mixing them up is the most common parsing-bug shape in this codebase.

### Phantom types + ppx_jsonaf_conv

`Completions.Request.t` is `'tag t` where the tag distinguishes streaming
from non-streaming. ppx_jsonaf_conv generates `jsonaf_of_t : ('tag ->
Jsonaf.t) -> 'tag t -> Jsonaf.t` — the converter argument is unused but
required. At call sites, use the extension form so ppx auto-derives the
dummy converter for the polymorphic-variant tag:

```ocaml
[%jsonaf_of: [ `Non_streaming ] Request.t] request
```

Don't try to call `Request.jsonaf_of_t` directly without a converter.

### Reusable JSON shape helpers

- `Json_helper.Make_string_variant (T)` — for nullary variants whose JSON wire
  form is a bare string. Recovers the constructor name via `[%sexp_of: T.t]`
  and normalises with `lowercase + tr '_' '-' + rstrip '-'`. Use it for any new
  enum-shaped variant. The `module T = struct ... end include T include
  Json_helper.Make_string_variant (T)` pattern is the canonical shape. The
  functor also exposes `arg_type` for example CLI flags.
- `Json_helper.Make_tagged_union` — for `{type: …, …}` discriminated unions.
  Use `Json_helper.Tagged_union_codec` and `Json_helper.nested` to describe
  whether each variant is tag-only, inlined, or nested under a payload key.
  This centralises dispatch and missing/non-string-tag error reporting.

### `For_testing` submodules

When tests need a parser that shouldn't be part of the public API, expose
it under `Module.For_testing` (e.g.
`Completions.For_testing.response_of_jsonaf`). Don't promote internal
parsers to the top level just to make a test compile.

### Type-bound logic lives in the type's module

The user enforces this: validation, predicates, and "is this empty" checks
go on the type itself, not in the example. Two recent migrations:
`Reasoning.create` validates `effort xor max_tokens`; `Provider.is_empty`
is `equal t empty` via derived `equal`. Smart constructors are preferred
over inline record literals in user code.

### Interface files

Every library module should have an `.mli`, except the module whose name
matches the library name. That wrapper module just re-exports the same public
surface and should not duplicate it in a second interface file.

## Common rookie mistakes

- **Sync I/O under `open! Async`**: don't use `In_channel.read_all`,
  `Sys.file_exists`, etc. Use `Reader.file_contents`, `Async.Sys.file_exists_exn`.
  Caught me once when capturing fixtures.
- **`try/with` in a deferred context**: the user pushed back on this.
  Reach for `Or_error.try_with` outside the deferred chain instead, and
  thread `Or_error.t` through `Deferred.return` only when crossing the
  boundary.
- **Field wildcards in record patterns**: `let { a; b; _ } = …` silently
  absorbs new fields. Enumerate every field and ignore the rest with
  `= _`.
- **Don't snapshot a backtrace**: `--auto-promote` happily captures
  `[@@expect.uncaught_exn …]` blocks containing exception stack traces.
  Fix the underlying issue (usually a missing fixture file or wrong
  fixture path) and re-promote, don't accept the backtrace.

## Things to push back on

- **"Just add it as a quick helper in the example"** — if the logic
  belongs to a type, it lives in that type's module. The user has flagged
  this twice (Provider.is_empty, Reasoning.create).
- **"Mock the network"** — deterministic fixture tests stay in `runtest`, but
  live calls are still the confidence signal for request/response surface
  changes. Capture realistic fixtures from live responses when the JSON shape
  is novel.
- **"Fixtures should cover every model"** — they shouldn't. Each fixture
  must exercise a distinct JSON shape. Adding `another_openai_model.json`
  that's structurally identical to an existing fixture is anti-coverage.

## A few specific files worth reading first

- `lib/completions.mli` — the public surface. Skim this end-to-end before
  making any cross-cutting change.
- `lib/json_helper.{ml,mli}` — reusable JSON codecs for string variants and
  tagged unions; understand them before introducing another codec abstraction.
- `example/chat_options.{ml,mli}` — the example CLI's request construction
  surface. Read these before adding or changing completion flags.
- `test/fixture_helpers.ml` — the fixture workflow doc-comment is the
  canonical "how to add a regression test" guide. Fixture tests are split by
  shape across `test/test_*_fixtures.ml`.
- `test/test_request_serialization.ml` — the JSON-shape guard for the
  smart constructors. Add a snapshot here when adding a new
  request-side field.
