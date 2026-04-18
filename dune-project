(lang dune 3.22)

(name openrouter-ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name openrouter-ocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends (ocaml (>= 5.4.1)) (ocamlformat :dev) (ocaml-lsp-server :dev))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
