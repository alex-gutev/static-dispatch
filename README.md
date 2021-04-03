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

The shadowed `DEFMETHOD` macro stores the body of the method in a
global variable which contains a hash-table mapping generic functions
to their methods.

A compiler macro function is added to the generic function which
determines the types of the arguments and replaces the function call
form with the body of the most specific applicable method. If the
types cannot be determined, or there isn't enough type information the
generic function call is left as is. Thus in order to choose the
appropriate method at compile-time rather than runtime, the types of
the arguments either have to be declared using `DECLARE` or surrounded
in a `THE` form.


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
identical to standard Common Lisp generic function, until the
following declaration is in place:

   1. `(INLINE <generic function name>)`

In order for the appropriate method to be chosen directly the
arguments to the generic function call form should be one of the
following:

   + Variables for which there is a `TYPE` declaration.
   + Functions for which there is an `FTYPE` declaration.
   + `THE` forms.
   + Constant literals.
   + Constants defined using `DEFCONSTANT`.
   + Macros/Symbol-macros which expand to one of the above.

Otherwise no method is chosen and the generic function call form is
left as is.

Both `EQL` and class specializers are supported. `EQL` methods will
only be chosen statically if the argument is one of the following and
is `EQL` to the specializer's value.

   + Constant literal.
   + Constant defined using `DEFCONSTANT`.
   + Variable with a `TYPE (EQL ...)` declaration.
   + Macro/Symbol-macro which expands to one of the above.

**Note:** Even if a variable is bound (by `LET`) to a value that is
`EQL` to the specializer's value, it will not be chosen unless it has
a `TYPE (EQL ...)` declaration.

`CALL-NEXT-METHOD` and `NEXT-METHOD-P` are supported
fully.

The standard method combination is supported, along with `:BEFORE`,
`:AFTER` and `:AROUND` methods, however user-defined method
combinations are not supported. You can still use a generic function
with a user-defined method combination in your code however it will
not be statically dispatched.

**Note:** In order for type and inline declarations to be made
available, to the compiler macro, consistently across implementations
the `ENABLE-HOOK` function has to be called at some point, see
https://github.com/alex-gutev/cl-environments#documentation for more
information.

### Interaction with other Compiler Macros

By default Static-Dispatch does not add a compiler macro to a generic
function which already has a compiler macro function. To statically
dispatch such functions the `STATIC-DISPATCH` function has to be
called manually from the compiler macro.

Function: `STATIC-DISPATCH FORM &OPTIONAL ENV`

Determines whether to statically dispatch the call to the generic
function `FORM`. If so returns the body of the selected method
otherwise returns `FORM` as is.

`ENV` is the lexical environment of the generic function call.

## Differences from INLINED-GENERIC-FUNCTION

Inlined-Generic-Function uses a custom generic function metaclass
`INLINED-GENERIC-FUNCTION` which stores the method's body in order for
it to be inlined by the compiler-macro. Whilst this approach is more
robust than shadowing `DEFMETHOD` in order to store the method body in
a global variable, as it will be able to inline methods added by other
means besides `DEFMETHOD`, the metaclass is changed from the standard
generic function metaclass which in turn prevents certain
optimizations, of the dynamic generic function dispatch, from being
performed by the compiler. This results in slower execution speed, as
shown in
[https://github.com/guicho271828/inlined-generic-function#user-content-result].

Static-Dispatch does not use a custom generic function metaclass thus
generic functions are identical to standard common lisp generic
functions, and hence the usual optimizations are performed, unless an
`INLINE` declaration is in place. This only matters when generic
functions are not inlined, however the goal of this library is to
provide generic function inlining as an optimization for cases where
it is known that standard dynamic dispatch is too slow, not to provide
inlining by default.

In Static-Dispatch the generic function call form is directly replaced
with the body of the most-specific applicable method, whereas in
Inlined-Generic-Function the form is replaced with a `MATCH` form
which contains a pattern-matching clause for each method that checks
whether the types of the arguments match the method's specializer list
and evaluates the body of the method. An advantage of the approach
taken by Inlined-Generic-Function is that the method bodies can be
inlined even if the arguments are more complicated expressions than
variables and `THE` forms. However this relies on the compiler to
remove the clauses corresponding to the non-applicable methods
otherwise the result is that dynamic dispatch is still performed,
however is performed inline and is potentially slower than the
built-in dynamic dispatch of CLOS. SBCL is capable of removing
branches, corresponding to non-applicable methods, however most other
compilers (including CCL) are not.

Static-Dispatch can handle full lambda-lists with all lambda-list
keywords. Inlined-Generic-Function cannot, as of yet (November 2018),
handle lambda-lists containing anything but required arguments.

Static-Dispatch does not yet support user-defined method combinations,
whereas Inlined-Generic-Function does.

## Dependencies

### Main Dependencies

[closer-mop](https://github.com/pcostanza/closer-mop) - Required to
obtain information about generic-function methods, namely the argument
precedence order and class precedence list.

[cl-environments](https://github.com/alex-gutev/cl-environments) -
Used to extract declaration information from environments, on any
implementation. Requires that the `ENABLE-HOOK` function (exported
from the `STATIC-DISPATCH-CL` package) is called.


### Other Dependencies

[agutil](https://github.com/alex-gutev/agutil),
[alexandria](https://gitlab.common-lisp.net/alexandria/alexandria),
[anaphora](https://github.com/tokenrove/anaphora),
[trivia](https://github.com/guicho271828/trivia),
[iterate](https://gitlab.common-lisp.net/iterate/iterate),
[arrows](https://gitlab.com/Harleqin/arrows).


## Status

Supports class and EQL specializers.

Does not support user-defined method combinations.

Tested on: CCL, SBCL, CLISP, ECL, CMUCL and ABCL.

Currently generic functions can be statically dispatched successfully
on SBCL and CCL without any workarounds. On the remaining listed
implementations, generic functions can only be statically dispatched
if the
[`CL-ENVIRONMENTS:ENABLE-HOOK`](https://github.com/alex-gutev/cl-environments#enable-hook)
function is called prior to compiling the source file, where static
dispatch should be performed.

### Known Issues:

 * On ABCL: methods are not inlined, if the types of the arguments are
   declared, due to the the lexical environment not being passed to
   compiler-macros. Methods can only be inlined on ABCL by surrounding
   the arguments in THE forms and if the generic function is declared
   inline globally. See
   https://github.com/alex-gutev/cl-environments#issues for more
   information.
