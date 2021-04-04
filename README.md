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

### Optimize Declarations

The optimize `speed` and `safety` declarations affect the code that is
inserted in place of the generic function call
expression. Specifically, the definition of the lexical
`CALL-NEXT-METHOD` functions.

The types of the arguments which may be passed to `CALL-NEXT-METHOD`
are unknown. An optimize declaration with a `SAFETY` level greater
than or equal to the level for `SPEED` will result in type checks,
using `CHECK-TYPE`, being inserted in the `CALL-NEXT-METHOD`
definition to check that the arguments are of the expected type given
in the method's specializer list. This will result in a condition
being raised if arguments of an incorrect type are passed to
`CALL-NEXT-METHOD`. However, if the `SPEED` optimize level is greater
than the `SAFETY` level the type checks are not inserted. It is then
the programmer's responsibility to ensure that the arguments passed to
`CALL-NEXT-METHOD` are of the types specified in specializer list of
the next most specific method. If `CALL-NEXT-METHOD` is called with no
arguments, which means it is called with the same arguments passed to
the current method, the arguments are guaranteed to be of the correct
type.

#### Examples

Given a generic function with the following methods:

```lisp
(defmethod foo ((a number))
  (list :number a))

(defmethod foo ((a integer))
  (list :integer (call-next-method (1+ a))))
```

An optimize declaration such as the following will result in type
checks being inserted:

```lisp
(locally
  (declare (inline foo)
           (optimize (speed 2) (safety 2)))

  (declare (type integer x))

  (foo x))
```

This results in the expression `(foo x)` being replaced with something
similar to the following (simplified to remove the case of
`CALL-NEXT-METHOD` being called with no arguments:

```lisp
(flet
  ((call-next-method (x)
     (check-type x number)
     (list :number x)))

 (list :integer (call-next-method (1+ x))))
```

If the optimize declaration were changed to `(optimize (speed 3)
(safety 0))`, the type checks are omitted which results in the
following:

```lisp
(flet
  ((call-next-method (x)
     (list :number x)))

 (list :integer (call-next-method (1+ x))))
```


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

### Common Pitfalls

The semantics of generic functions are changed when statically
dispatched, that is when they are declared `inline` in the environment
in which they are called.

Statically dispatched generic functions are dispatched based on the
declared types of the arguments at compile-time. The declared type may
differ from the actual runtime type of the argument, which may result
in a different method being called when statically dispatched from
when the generic function is dynamically dispatched. A useful analogy
is that dynamically dispatched generic functions have the semantics of
C++ virtual methods whereas statically dispatched functions have the
semantics of overloaded functions/methods in C++ and Java.

Consider the following methods:

```lisp
(defmethod foo ((a number))
  (list :number a))

(defmethod foo ((a integer))
  (list :integer a))
```

And consider the following code:

```lisp
(let ((x 1))
  (declare (type number x)
           (inline foo))

  (foo x))
```

When statically dispatched, as in the above example with the `(INLINE
FOO)` declaration, the method specialized on `NUMBER` will be called
and hence `(FOO X)` will evaluate to `(:NUMBER 1)`. However since `X`
is actually bound to an integer value, with dynamic dispatch, when the
`(INLINE FOO)` declaration is removed, the method specialized on
`INTEGER` will be called and hence `(FOO X)` will evaluate to
`(:INTEGER 1)`.

It's also possible that a particular implementation may change the
declared type of a variable to a subtype if it can be deduced to be of
that subtype, so in this example the declaration `(TYPE NUMBER X)`
maybe changed to `(TYPE INTEGER X)` by the implementation. As a result
it's not even possible to rely on the declaration forcing the method
specialized on `NUMBER` to be called. Therefore you should only use
statically dispatched functions for optimization where each method has
the same behaviour just implemented for different types.

**NOTE:** If the type of an argument cannot be determined, or is
declared `T`, the generic function will not be statically dispatched even
if an `INLINE` declaration is in place.

Another aspect in which the semantics of generic functions are changed
is that when dynamically dispatched the list of methods can be changed
at runtime, that is new methods can be added while the program is
running and existing methods can be removed. Obviously,
static-dispatch cannot predict what methods will be added or removed,
therefore adding or removing methods will not have the desired effect
on statically dispatched generic function calls.


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

### Planned Future Additions

* Increased type inference of arguments allowing for more complex
  expressions.
* Removal of `CALL-NEXT-METHOD` and `NEXT-METHOD-P` if not used by the
  method.
* Optimization of `CALL-NEXT-METHOD`, where it is inlined in some cases.
* Proper static dispatch, where the generic function call is replaced
  with a call to the actual method function rather than with the body
  of the method inline.
* Method return type declarations.
* Support for other method combinations.
* Enhance generic functions to allow for specialization on all types
  rather than just classes.
* Debugging options for emitting a warning when a generic function
  could not be statically dispatched.
