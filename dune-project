(lang dune 2.7)

(name dream_middleware_ext)

(generate_opam_files true)

(source
 (github axons-talent/dream_middleware_ext))

(authors "Geoffrey Borough <geoffrey.borough@outlook.com>")

(maintainers "Geoffrey Borough <geoffrey.borough@outlook.com>")

(license MIT)

(documentation https://axons-talent.github.io/dream_middleware_ext/dream_middleware_ext)

(package
 (name dream_middleware_ext)
 (synopsis "A collection of middleware utilities for Dream framework")
 (description "A collection of middleware utilities for Dream framework")
 (depends
  (ocaml
   (>= 4.14.0))
  (dream (>= 1.0.0~alpha8))
  lwt_ppx
  uri
  (ppx_expect :with-test)
  ptime
  ipaddr)
 (tags
  (http web framework middleware)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
