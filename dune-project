(lang dune 2.9)
(name xmelly)
(version 1.0.1)

(cram enable)
(generate_opam_files true)

(source (github m4c0/xmelly))
(license MIT)
(authors "Eduardo Costa")
(maintainers m4c0@users.noreply.github.com)

(package
 (name xmelly)
 (synopsis "Simplest way to do simple parsing of simple XML files in OCaml")
 (description
  "This is a simple library to parse XML files in OCaml. It does not do
  validation or output. Call `Xmelly.parse` with an `in_channel` and pattern
  match you way into the data you want.

  The output will strip extra spaces - therefore this library might not be
  suitable for HTML/SGML parsing."))
