(lang dune 2.8)
(name libcompiler)

(generate_opam_files true)

(package
 (name libcompiler)
 (version 2023)
 (synopsis "Library for the compiler workshop")
 (authors "Cyril Allignol <cyril.allignol@enac.fr>")
 (maintainers "Cyril Allignol <cyril.allignol@enac.fr>")
 (homepage "https://github.com/CS-ENAC/libcompiler")
 (bug_reports "https://github.com/CS-ENAC/libcompiler/issues")
 (source (github CS-ENAC/libcompiler))
 (license GPL-3.0-or-later)
 (description "\| This is the library for the compiler workshop at ENAC.
              "\| It provides modules for building an AST for the source
              "\| language, implementing instructions for the target
              "\| architecture, and executing the compiled programs on a
              "\| simulated processor.
 )
 (depends menhir graphics)
)
