(lang dune 3.16)

(name gamedata)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name gamedata)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  re
  atd
  atdgen
  atdgen-runtime
  dream
  lwt
  lwt_ppx
  digestif
  ocaml-lsp-server
  ocamlformat
  base
  ppx_deriving
  ppx_jane
  ; Testing dependencies
  alcotest
  qcheck
  qcheck-alcotest
  bisect_ppx)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
