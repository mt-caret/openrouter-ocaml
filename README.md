# openrouter-ocaml

An Async-based OCaml client for the [OpenRouter](https://openrouter.ai) API.
Full API docs: <https://mt-caret.github.io/openrouter-ocaml/>.

<!-- $MDX skip -->
```sh
opam install openrouter_api
```

## Quickstart

```ocaml
open! Core
open! Async
open Openrouter_api

let run_once ~api_key =
  let request =
    Completions.Request.create
      ~model:"openai/gpt-4o-mini"
      ~messages:[ Completions.Request.Message.user "Say pong." ]
      ()
  in
  Completions.create ~api_key request
  >>| function
  | Error error -> eprintf "%s\n" (Error.to_string_hum error)
  | Ok (response : Completions.Response.t) ->
    response.choices
    |> List.iter ~f:(fun (choice : Completions.Response.Choice.t) ->
      Option.iter choice.message.content ~f:print_endline)
;;
```

## Feature Coverage

| OpenRouter functionality | Supported |
| --- | --- |
| Chat completions | Yes |
| Streaming chat completions | Yes |
| Model listing | Yes |
| Generation stats | Yes |
| Embeddings | Yes |
| Sampling and output controls | Yes |
| Provider routing | Yes |
| Prompt caching | Yes |
| Reasoning controls and responses | Yes |
| Structured outputs | Yes |
| Tool calling | Yes |
| OpenRouter server tools | Yes |
| Plugins | Yes |
| Multimodal inputs | Yes |
| Image outputs | Yes |
| Audio outputs | Yes |
| Credits endpoint | No |
| API key management | No |
| Organization/workspace management | No |
| Guardrails | No |
| Reranking | No |
| Audio transcription/speech endpoints | No |
| Video generation endpoints | No |
| Anthropic Messages API | No |
| Responses API | No |

## Example CLI

<!-- $MDX skip -->
```sh
export OPENROUTER_API_KEY=sk-or-v1-...
dune exec example/openrouter_api_example.exe -- help -recursive -flags

dune exec example/openrouter_api_example.exe -- chat \
  -no-stream \
  -model openai/gpt-4o-mini \
  "say 'pong'"
```

The full flag reference is generated in
[`example/help.txt`](example/help.txt).

## Development

After substantive edits, run:

<!-- $MDX skip -->
```sh
dune build
dune runtest
dune fmt
```

Response fixtures live in `test/responses/`; request-wire snapshots live in
`test/test_request_serialization.ml`. Use `dune runtest --auto-promote` to
refresh expect tests, then inspect the diff before keeping promoted snapshots.
