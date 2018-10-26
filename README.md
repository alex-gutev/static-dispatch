# STATIC-DISPATCH

Static dispatch is a library, inspired by
[inlined-generic-function](https://github.com/guicho271828/inlined-generic-function),
which allows standard Common Lisp generic function dispatch to be
performed statically (at compile time) rather than dynamically
(runtime). This is similar to what is known as "overloading" in
languages such as C++ and Java.

The purpose of static dispatch is to provide an optimization in cases
where the usual dynamic dispatch is too slow, and the dynamic features
of generic functions, such as adding/removing methods at runtime are
not required. An example of such a case is a generic equality
comparison function. Currently generic functions are considered far
too slow to implement generic arithmetic and comparison operations
when used heavily in numeric code.

## How it works

The shadowed `DEFMETHOD` macro expands to both a method definition
(using `DEFMETHOD`) and an ordinary function definition `DEFUN` which
implements the method function. The body of the method is also stored
in a global variable which contains a hash-table mapping generic
functions to their methods.

A compiler macro function is added to the generic function which
replaces calls to the generic function, if it is declared inline or
its `DISPATCH` is `STATIC`, with a `MATCH` form (using the
[trivia](https://github.com/guicho271828/trivia) pattern matching
library) which matches the arguments of the generic function to the
correct method. The form contains a match clause for each method in
order of specificity. It is up to the Common Lisp compiler to remove
clauses for which it is known that the arguments will not match. This
can only be done if the types of the arguments have been declared,
either using `DECLARE` or `THE`.

Each match clause can either contain a call to the method function, if
the generic function's `DISPATCH` is declared `STATIC` or the method's
body can be directly inlined in the clause if the generic function is
declared `INLINE`.


## Usage

The package `STATIC-DISPATCH-CL` is provided which contains all
symbols in the `COMMON-LISP` package and the shadowed `DEFMETHOD`
macro. This package should be used/imported instead of `COMMON-LISP`
as besides exporting the shadowed `DEFMETHOD` symbol, it also exports
all symbols shadowed by the `CLOSER-COMMON-LISP` package (from
[closer-mop](https://github.com/pcostanza/closer-mop)) which are
necessary for closer-mop to function correctly.

Generic functions and methods are defined as usual, using `DEFGENERIC`
and `DEFMETHOD`. Generic functions are dispatched dynamically, and are
identical to standard Common Lisp generic function, until either of
the following declarations is in place:

   1. `(DISPATCH STATIC <generic function name>)`
   2. `(INLINE <generic function name>)`


When the first declaration is in place, the generic function is
replaced with a `MATCH` form that invokes a non-generic function which
implements the method. When the second declaration is in place the
generic function is replaced with a `MATCH` form that contains bodies
of the methods inline.


## Differences from INLINED-GENERIC-FUNCTION

Inlined-Generic-Function uses a custom generic function metaclass
`INLINED-GENERIC-FUNCTION` which stores the method's body in order for
it to be inlined by the compiler-macro. Whilst this approach is more
robust than shadowing `DEFMETHOD` in order to store the method body in
a hash-table in a global variable, as it will be able to inline
methods added by other means besides `DEFMETHOD`, the metaclass is
changed from the standard generic function metaclass which in turn
prevents certain optimizations, of the dynamic generic function
dispatch, from being performed by the compiler. This results in slower
execution speed, as shown in
[https://github.com/guicho271828/inlined-generic-function#user-content-result].

Static-Dispatch does not use a custom generic function metaclass thus
generic functions are identical to standard common lisp generic
function, and the usual optimizations are performed, unless an
`INLINE` or `DISPATCH STATIC` declaration is in place. This only
matters when generic functions are not inlined, however the goal of
this library is to provide generic function inlining as an
optimization for cases where it is known that standard dynamic
dispatch is too slow, not to provide inlining by default.

Static-Dispatch provides functionality for performing static-dispatch
without inlining the method bodies, similar to "overloading" in other
languages. Inlined-Generic-Function always only provides functionality
for inlining the full method bodies or default dynamic dispatch.

Static-Dispatch can handle full lambda-lists with all lambda-list
keywords. Inlined-Generic-Function cannot, as of yet (October 2018),
handle lambda-lists containing anything but required arguments.

Static-Dispatch doesn't yet support `CALL-NEXT-METHOD`,
`NEXT-METHOD-P`, before after and around methods and user-defined
method combinations. All are supported by Inlined-Generic-Function.

## Dependencies

### Main Dependencies

[closer-mop](https://github.com/pcostanza/closer-mop) - Required to
obtain information about generic-function methods, their specializer
lists etc.

[trivia](https://github.com/guicho271828/trivia) - Pattern Matching.

[cl-environments](https://github.com/alex-gutev/cl-environments) -
Used to extract declaration information from environments, on any
implementation. Requires that the `ENABLE-HOOK` function (exported
from the `STATIC-DISPATCH-CL` package) is called.


### Other Dependencies

[agutil](https://github.com/alex-gutev/agutil), [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria), [anaphora](https://github.com/tokenrove/anaphora), [iterate](https://gitlab.common-lisp.net/iterate/iterate), [cl-arrows](https://github.com/nightfly19/cl-arrows).


## Status

Supports class and EQL specializers.

Does not yet support `CALL-NEXT-METHOD`/`NEXT-METHOD-P`, method
qualifiers and user-defined method combinations.

Methods may only be defined using `DEFMETHOD`, not (yet) using the
`:METHOD` option of `DEFGENERIC`.

Tested on CCL.
