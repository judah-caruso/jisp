# Jisp

Simple example showing how you can create a DSL
by compiling a custom file on #import, emit Jai
AST nodes, and pass that to a workspace for Jai
to compile.

# Instructions

Just run `jai compile.jai`

# Notes

The Lisp is just a proof of concept so it's
missing almost all features. I wouldn't use
this as a reference for implementing a
compiler/interpreter.

As of now, we can't give Jai a Code_Nodes
directly so we have to use Program_Print and
add_build_string. Ideally we'd be able to
compile our module and pass that directly
without converting to a string first.
