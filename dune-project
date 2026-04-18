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
 (synopsis "A short synopsis")
 (description "A longer description")
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
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
