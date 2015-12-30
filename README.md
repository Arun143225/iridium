Attention: This library is under active development.  The method of
generating LLVM is being switched from an FFI binding library to the
llvm-general library.

Iridium: Common High-level IR Library
================

A library whose goal is to produce several common high-level
intermediate languages, and facilities for compiling them.  The
current compilation target is LLVM, though others (JVM, CIL, etc)
may follow.

This package is designed to assist in compiling languages to produce
executable files.  At the moment, it contains one IR language, though
others are planned.

 * FlatIR: A simply-typed language with flat scopes, whose
   instructions correspond to LLVM's.  However, FlatIR has mutable
   variables, supports variant types (not implemented), and allows
   variables of any type.  Also, it makes instruction selection
   decisions based on  the types of arguments.

   FlatIR also contains rather elaborate information pertaining to
   garbage collection.

 * SimpleIR (not implemented): A simply-typed IR language with nested
   scopes and patterns.  Subtyping is based on 

 * PolyIR (not implemented): A polymorphically-typed IR language with
   nested scopes and patterns.


Compilation steps are as follows:

 * FlatIR to LLVM: convert types and generate metadata to capture
   garbage collection and other information, SSA construction,
   generate LLVM instructions.

 * SimpleIR to FlatIR: compile pattern matches to branching code,
   implement static linking to get rid of nested scopes.  Possibly
   implement dispatch as well.

 * PolyIR to SimpleIR (not implemented): monomorphize and boxing to
   eliminate polymorphic types, scope analysis and static linking.


Specific things to be done:

 * General:

   1) Better error messages (needs a redesign of Format from
      proglang-utils)

 * FlatIR:

   1) Add variant types and support for compiling them.

   2) Design general-purpose Alloc to replace GCAlloc, add support for
      compiling it.

   3) Redesign ConstValue to fall back to generating initializer code.

      a) Have global variables generate static initializers.

      b) Track global variables that are made non-constant by the need
         for static initializers, add llvm.invariant intrinsics to the
         preludes of all functions that use them.

      c) Attempt to generate select instructions wherever branch
         expressions can be turned into constants.

   4) Add exceptions and continuations.

   5) Output TBAA data.

   6) Link GC headers and GC read/write/addr functions into metadata directly

   7) Rework metadata to correspond to llvm plugin.

   8) Build table of GC accessor functions, implement read/write,
      possibly address of GC fields.

   9) Output debugging information, have meaningful variable names
