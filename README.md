# STATIC-DISPATCH

Static dispatch is a library, inspired by
[inlined-generic-function](https://github.com/guicho271828/inlined-generic-function),
which allows standard Common Lisp generic function dispatch to be
performed statically (at compile time) rather than dynamically
(at runtime). This is similar to what is known as "overloading" in
languages such as C++ and Java.

The purpose of static dispatch is to provide an optimization in cases
where the usual dynamic dispatch is too slow, and the dynamic features
of generic functions, such as adding/removing methods at runtime are
not required. An example of such a case is a generic equality
comparison function. Currently generic functions are considered far
too slow to implement generic arithmetic and comparison operations
when used heavily in numeric code.

## Usage

Generic functions and methods are defined as usual, however using
`DEFGENERIC` and `DEFMETHOD` from the `STATIC-DISPATCH-CL` package,
rather than the `CL` package. By default, generic functions are
dispatched dynamically, and are identical to standard Common Lisp
generic functions.

The standard method combination is supported, including `:BEFORE`,
`:AFTER` and `:AROUND` methods, as well as `CALL-NEXT-METHOD` and
`NEXT-METHOD-P`. User-defined method combinations are not
supported. You can still define a generic function with a user-defined
method combination in your code, however it will not be statically
dispatched.

**NOTE:** The package `STATIC-DISPATCH-CL` exports all symbols from the `CL`
package as well as the symbols in the `STATIC-DISPATCH` package,
including the shadowed `DEFMETHOD` and `DEFGENERIC` macros. Use this
package instead of `CL`.

### Enable Static Dispatch

To enable static dispatching for a generic function the following
macro form has to be inserted after the definition of the generic
function and its methods:

```lisp
(enable-static-dispatch &rest names)
```

where each item in `names` is the name of a generic function, for
which to enable static dispatching. The name of a function may also be
of the following forms which control how functions are statically
dispatched:

* `(:INLINE name)` - Calls to `name` are replaced with the body of the
  most applicable method(s) inline.
  
* `(:FUNCTION name)` - Calls to `name` are replaced with a call to an
  ordinary function which implements the most applicable method.
  
If only the name is given it's equivalent to `(:INLINE name)`. The
only difference between this form and `(:INLINE name)` is that if
static dispatching is already enabled for the function, the default
form leaves the static dispatching mode as is whereas `(:INLINE name)`
changes it to `:INLINE`.

**NOTE:** The `:FUNCTION` static dispatching mode is most useful, when
you have large methods and you want true overloading, as is found in
languages such as Java and C++. The `:INLINE` mode will likely result
in faster, however also larger code.

**NOTE:** Information about the generic function's methods must be
available at the time the `ENABLE-STATIC-DISPATCH` macro is
expanded. Furthermore only the methods defined till the point of its
expansion will be considered for static dispatching. Thus it's best to
place an `ENABLE-STATIC-DISPATCH` form in a separate file which is
loaded after the file(s) containing the definition of the generic
function and it's methods.

### Static Dispatching

To statically dispatch a generic function, an `OPTIMIZE` declaration,
with a `SPEED` level greater than the `SAFETY` level, and an `INLINE`
declaration for the generic function need to be in place in the
lexical environment of the generic function call.

**NOTE:** Static dispatching must have been enabled for the generic
function, using `ENABLE-STATIC-DISPATCH`, prior to the generic
function call, in order for the declarations to have any effect.

**Example:**

```
(locally (declare (optimize (speed 3) (safety 2))
                  (inline foo)) ; Static dispatch FOO

    (FOO X Y)) ; Call to FOO statically dispatched if type of X and Y known
```


In order for the applicable method to be chosen at compile-time, the
types of the arguments to the generic function call must be known.

On SBCL this is dependent on whether the compiler can determine the
types of the arguments.

On the remaining implementations, the types of the arguments can be
determined if they are one of the following:

   + Variables for which there is a `TYPE` declaration.
   + Functions for which there is an `FTYPE` declaration.
   + `THE` forms.
   + Constant literals.
   + Constants defined using `DEFCONSTANT`.
   + Macros/Symbol-macros which expand to one of the above.

The types of the following special forms can also be determined:

   + `PROGN`

If the types of the arguments cannot be determined, no method is
chosen and the generic function call form is left as is, which
falls back to the standard dynamic dispatch.

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

### Prevent Static Dispatching

**NOTE:** On SBCL all generic functions, for which static dispatch is
enabled, using `ENABLE-STATIC-DISPATCH`, will be statically dispatched,
regardless of whether the function is declared inline or not, if the
`SPEED` optimization level exceeds the `SAFETY` optimization level. To
prevent a function from being statically dispatched in such cases,
declare it `NOTINLINE`.

**Example:**

```lisp
(locally (declare (optimize speed)
                  (notinline foo))
    (FOO X Y)) ; FOO not statically dispatched
```


### Optimize Declarations

As stated earlier the `SPEED` level, in an `OPTIMIZE` declaration,
must be greater than the `SAFETY` level for static dispatch to be
performed.

Additionally the values of the `SPEED` and `SAFETY` levels affect the
code that is inserted in place of the generic function call
expression. Specifically, the definition of the lexical
`CALL-NEXT-METHOD` function.

The types of the arguments which may be passed to `CALL-NEXT-METHOD`
are unknown. By default type checks, using `CHECK-TYPE`,
for the arguments are added. A condition is raised if the types of the
arguments passed to `CALL-NEXT-METHOD` are not compatible with the
types in the next applicable method's specializer list.

When a `SPEED` level of 3 or a `SAFETY` level of 0 is given, the type
checks are omitted and it is the programmer's responsibility to ensure
that the arguments passed to the next method are compatible with the
types in the method's specializer list. When `CALL-NEXT-METHOD` is
called with no arguments, which is equivalent to being called with the
same arguments as the current method, the argument types are
guaranteed to be compatible.

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
           (optimize (speed 2) (safety 1)))

  (declare (type integer x))

  (foo x))
```

This results in the expression `(foo x)` being replaced with something
similar to the following (simplified to remove the case of
`CALL-NEXT-METHOD` being called with no arguments):

```lisp
(flet
  ((call-next-method (x)
     (check-type x number)
     (list :number x)))

 (list :integer (call-next-method (1+ x))))
```

If the optimize declaration is changed to `(optimize (speed 3) (safety
0))`, the type checks are omitted which results in the following:

```lisp
(flet
  ((call-next-method (x)
     (list :number x)))

 (list :integer (call-next-method (1+ x))))
```


### Interaction with other Compiler Macros

By default static-dispatch does not add a compiler macro to a generic
function which already has a compiler macro function. To statically
dispatch such functions the `STATIC-DISPATCH` function has to be
called manually from the compiler macro.

Function: `STATIC-DISPATCH FORM &OPTIONAL ENV`

Determines whether to statically dispatch the call to the generic
function `FORM`. If so returns the body of the selected method
otherwise returns `FORM` as is.

`ENV` is the lexical environment of the generic function call.

**NOTE:** On SBCL this is a no-op since IR1 transforms, using
`SB-C:DEFTRANSFORM`, specified directly on the argument types, are
used rather than compiler macros.

### Common Pitfalls

The semantics of generic functions are changed when statically
dispatched.

Statically dispatched generic functions are dispatched based on the
declared types of the arguments at compile-time. The declared type may
differ from the actual runtime type of the argument, which may result
in a different method being called when statically dispatched from
when the generic function is dynamically dispatched. An intuitive
analogy is that dynamically dispatched generic functions have the
semantics of C++ virtual methods whereas statically dispatched
functions have the semantics of overloaded functions/methods in C++
and Java.

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
           (optimize speed)
           (inline foo))

  (foo x))
```

Since `FOO` is statically dispatched, the method specialized on
`NUMBER` will be called, due to the `(TYPE NUMBER X)` declaration, and
hence `(FOO X)` will evaluate to `(:NUMBER 1)`. However since `X` is
actually bound to an integer value, with dynamic dispatch, when the
`(INLINE FOO)` declaration is removed, the method specialized on
`INTEGER` will be called and hence `(FOO X)` will evaluate to
`(:INTEGER 1)`.

It's also possible that a particular implementation may change the
declared type of a variable to a subtype if it can be deduced to be of
that subtype, so in this example the declaration `(TYPE NUMBER X)`
may be changed to `(TYPE INTEGER X)` by the implementation. As a result
it's not even possible to rely on the declaration forcing the method
specialized on `NUMBER` to be called. Therefore you should only use
statically dispatched functions for optimization where each method has
the same behaviour just implemented for different types.

**NOTE:** If the type of an argument cannot be determined, or is
declared `T`, the generic function will not be statically dispatched
even if an `INLINE` and `OPTIMIZE` declaration is in place.

Another aspect in which the semantics of generic functions are changed
is that when dynamically dispatched the list of methods can be changed
at runtime, that is new methods can be added while the program is
running and existing methods can be removed. Obviously,
static-dispatch cannot predict what methods will be added or removed,
therefore adding or removing methods will not have the desired effect
on statically dispatched generic function calls.

## Dependencies

### Main Dependencies

* [closer-mop](https://github.com/pcostanza/closer-mop) - Required to
  obtain information about generic-function methods, namely the
  argument precedence order and class precedence list.

* [cl-environments](https://github.com/alex-gutev/cl-environments) -
  Used to extract declaration information from environments, on any
  implementation.


### Other Dependencies

* [agutil](https://github.com/alex-gutev/agutil)
* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [anaphora](https://github.com/tokenrove/anaphora)
* [trivia](https://github.com/guicho271828/trivia)
* [iterate](https://gitlab.common-lisp.net/iterate/iterate)
* [arrows](https://gitlab.com/Harleqin/arrows)


## Status

Supports class and EQL specializers, CALL-NEXT-METHOD, NEXT-METHOD-P
and auxiliary methods.

Does not support user-defined method combinations.

Tested on: CCL, SBCL, Clisp, ECL, CMUCL and ABCL.

Tests pass (static dispatch is successful) on: SBCL, CCL, Clisp and
ECL.

Tests fail (falls back to dynamic dispatch) on: CMUCL and ABCL.

Whether static-dispatching is successful depends on whether the
cl-environments library is able to extract the type information from
the environment. Where the CLTL2 API is natively supported (SBCL, CCL,
CMUCL, LispWorks and Allegro) the type information can be accessed
without any workarounds. On the remaining implementations the type
information can be extracted in most cases however workarounds are
required for some edge cases, see [CL-ENVIRONMENTS: Ensuring Code
Walking](https://alex-gutev.github.io/cl-environments/#ensuring_code_walking).

### Known Issues:

 * Static dispatch does not work on ABCL. Code still runs correctly
   but falls back to dynamic dispatch. This is due to ABCL not passing
   the lexical environment to compiler-macros. See
   https://github.com/alex-gutev/cl-environments#issues for more
   information.

### Planned Future Additions

* Increased type inference of arguments allowing for more complex
  expressions.
* Removal of `CALL-NEXT-METHOD` and `NEXT-METHOD-P` if not used by the
  method.
* Optimization of `CALL-NEXT-METHOD`, where it is inlined in some cases.
* Method return type declarations.
* Support for other method combinations.
* Enhance generic functions to allow for specialization on all types
  rather than just classes.
* Debugging options for emitting a warning when a generic function
  could not be statically dispatched.
