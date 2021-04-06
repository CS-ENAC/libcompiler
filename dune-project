(lang dune 2.8)
(name compiler_enac)

(generate_opam_files true)

(package
 (name compiler_enac)
 (version 2021)
 (synopsis "Library for the compiler workshop")
 (authors "Cyril Allignol <cyril.allignol@enac.fr>")
 (maintainers "Cyril Allignol <cyril.allignol@enac.fr>")
 (description "\| This is the library for the compiler workshop at ENAC."
              "\| It provides modules for building an AST for the source"
              "\| language, implementing instructions for the target"
              "\| architecture, and executing the compiled programs on a"
              "\| simulated processor."
 )
 (depends "menhir" "graphics")
)
