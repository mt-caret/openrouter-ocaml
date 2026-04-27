# openrouter-ocaml

An Async-based OCaml client for the [OpenRouter](https://openrouter.ai) API — a
unified gateway to LLMs from Anthropic, OpenAI, Google, and many other
providers.

```sh
opam install openrouter_api
```

API documentation: <https://mt-caret.github.io/openrouter-ocaml/>

## Usage

See [`example/openrouter_api_example.ml`](example/openrouter_api_example.ml)
for a complete CLI exercising implemented endpoints (chat, embeddings,
list-models). Run it with:

```sh
export OPENROUTER_API_KEY=sk-or-v1-...
dune exec example/openrouter_api_example.exe -- help -recursive -flags
```

The full flag reference is also dumped to
[`example/help.txt`](example/help.txt).
