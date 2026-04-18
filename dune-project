(lang dune 3.22)

(name openrouter_api)

(generate_opam_files true)

(source
 (github mt-caret/openrouter-ocaml))

(authors "Masayuki Takeda <mtakeda@oeconomia.jp>")

(maintainers "Masayuki Takeda <mtakeda@oeconomia.jp>")

(license MIT)

(documentation https://github.com/mt-caret/openrouter-ocaml)

(package
 (name openrouter_api)
 (synopsis "OCaml client for the OpenRouter API")
 (description
  "An Async-based OCaml client for OpenRouter (https://openrouter.ai), a unified API for LLMs from multiple providers (Anthropic, OpenAI, Google, and others). Covers chat completions (with streaming, tools, plugins, and structured outputs), text embeddings, and model listing.")
 (depends
  (ocaml
   (>= 5.4.1))
  ppx_jsonaf_conv
  ppx_jane
  async
  cohttp-async
  async_ssl
  core
  jsonaf
  (ocamlformat :dev)
  (ocaml-lsp-server :dev))
 (tags
  (openrouter llm ai api-client async)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
