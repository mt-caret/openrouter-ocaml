(lang dune 3.22)

(using mdx 0.4)

(name openrouter_api)

(generate_opam_files true)

(source
 (github mt-caret/openrouter-ocaml))

(authors "Masayuki Takeda <mtakeda@oeconomia.jp>")

(maintainers "Masayuki Takeda <mtakeda@oeconomia.jp>")

(license MIT)

(documentation https://mt-caret.github.io/openrouter-ocaml/)

(package
 (name openrouter_api)
 (synopsis "OCaml client for the OpenRouter API")
 (description
  "An Async-based OCaml client for OpenRouter (https://openrouter.ai), a unified API for LLMs from multiple providers (Anthropic, OpenAI, Google, and others). Covers chat completions (with streaming, tools, plugins, multimodal content, provider routing, reasoning, and structured outputs), text embeddings, model listing, and generation stats.")
 (depends
  ; Pinned to Jane Street's v0.18~preview packages (see dune-workspace) because
  ; [@@jsonaf.allow_extra_fields.log] only exists from ppx_jsonaf_conv v0.18
  ; onward. Those packages currently require OCaml < 5.3.
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3)))
  (mdx
   (and
    :with-test
    (>= 2.3.0)))
  (ppx_jsonaf_conv
   (and
    (>= v0.18~)
    (< v0.19)))
  (ppx_jane
   (and
    (>= v0.18~)
    (< v0.19)))
  (ppx_typed_fields
   (and
    (>= v0.18~)
    (< v0.19)))
  (async
   (and
    (>= v0.18~)
    (< v0.19)))
  cohttp-async
  (async_ssl
   (and
    (>= v0.18~)
    (< v0.19)))
  (core
   (and
    (>= v0.18~)
    (< v0.19)))
  (jsonaf
   (and
    (>= v0.18~)
    (< v0.19))))
 (tags
  (openrouter llm ai api-client async)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
