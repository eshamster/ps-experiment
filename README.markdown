[![Build Status](https://travis-ci.org/eshamster/ps-experiment.svg?branch=master)](https://travis-ci.org/eshamster/ps-experiment)
[![Coverage Status](https://coveralls.io/repos/github/eshamster/ps-experiment/badge.svg?branch=master)](https://coveralls.io/github/eshamster/ps-experiment?branch=master)

# Ps-Experiment

This is experimental extentions of [Parenscript](https://common-lisp.net/project/parenscript).

----

## Usage

### Some extended notations

#### Dot notation

Example: 

```lisp
(ps. (setf pnt.x 100))
;; => (ps:ps (setf (@ pnt x) 100))
```

Limitation:

this conversion is done after reading. So the following example is not supported. 

```lisp
;; this can't be supported
(ps. `(setf ,pnt.x 100))
```

#### Camel case (read macro)

In \#j. (sharp j dot) read macro, camel cases are supported. To enable this syntax, call `(enable-ps-experiment-syntax)` macro.

```lisp
> (enable-ps-experiment-syntax)
> '#j.div.innerHTML#
DIV.INNER-H-T-M-L
> (ps. (setf #j.div.innerHTML# "sample text"))
"div.innerHTML = 'sample text';"
```

### defstruct.ps[+]

The ps-experiment provides the subset of defstruct. When you define a struct 'abc' with slots 'slot1' and 'slot2', followings are defined automatically. These macros and functions can be used in ps:ps environment.

- Macros
  - abc-slot1
  - abc-slot2
- Functions
  - make-abc
  - abc-p

Example:

```lisp
;; If you use defstruct.ps+ instead of defstruct.ps,
;; the struct is defined also in Common Lisp's environment.
> (defstruct.ps test1 a (b 10))
TEST1
> (defstruct.ps (test2 (:include test1 (a 20)) c))
TEST2
> (ps. (setf (test2-a x) 100))
"x.a = 100;"
```

Syntax:

```text
name-and-options::= structure-name | (structure-name (:include included-structure-name {inherit-slot-description}*))
slot-description::= slot-name | (slot-name slot-init-form)
inherit-slot-description::= (slot-name slot-init-form)

included-structure-name---a symbol.
structure-name---a symbol.
slot-name---a symbol.
slot-init-form---a form.
```

### defgeneric.ps[+], defmethod.ps[+]

The ps-experiment provides the (tiny) subset of defstruct. The main purpose of the implementation is to provide multiple dispatch according to types of arguments.

Syntax of `defgeneric.ps[+]`:

```text
defgeneric function-name gf-lambda-list [[option]]

=> new-generic

option::= (:documentation gf-documentation)

Arguments and Values:

function-name---a function name.
gf-documentation---a string; not evaluated.
gf-lambda-list---a generic function lambda list.
new-generic---the generic function object.
```

Syntax of `defmethod.ps[+]`:

```text
defmethod function-name specialized-lambda-list [[declaration* | documentation]] form*

=> new-method

function-name::= symbol

;; Note: & paramters are not considered and tested well.
specialized-lambda-list::= ({var | (var parameter-specializer-name)}*
                            [&optional {var | (var [initform [supplied-p-parameter] ])}*]
                            [&rest var]
                            [&key{var | ({var | (keywordvar)} [initform [supplied-p-parameter] ])}*
                                 [&allow-other-keys] ]
                            [&aux {var | (var [initform] )}*] )
parameter-specializer-name::= symbol

Arguments and Values:

declaration---a declare expression; not evaluated.
documentation---a string; not evaluated.
var---a variable name.
Form---a form.
Initform---a form.
Supplied-p-parameter---variable name.
new-method---the new method object.
```

### Some top-level definitions and Easy package system

The ps-experiment provides some top-level definitions, `defvar.ps(+)`, `defun.ps(+)`, `defstruct.ps(+).

- `.ps` (Ex. `defvar.ps`): defines only for Parenscript
- `.ps+` (Ex. `defvar.ps+`): defines both for Parenscript and for Common Lisp

Then, you can output these definitions as JavaScript by `with-use-ps-pack` macro per package.

The sample [Roswell script](https://github.com/snmsts/roswell):

```lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(ql:quickload :ps-experiment)

(defpackage pack-a
  (:use :cl :ps-experiment)
  (:export :inc-num :negate))
(defpackage pack-b
  (:use :cl :ps-experiment)
  (:import-from :pack-a
                :inc-num))

;; ----- Package A ----- ;;
(in-package :pack-a)

(defvar.ps *num* 0)

(defun.ps inc-num (x)
  (incf *num* x))

(defun.ps negate (x)
  (* x -1))

;; ----- Package B ----- ;;
(in-package :pack-b)

(defun.ps dec-num (x) 
  (inc-num (pack-a:negate x)))

;; :this = :pack-b
(defun main (&rest argv)
  (declare (ignorable argv))
  (print
   (with-use-ps-pack (:this)
     pack-a::*num*)))
```

The output is as below.

```javascript
var packA = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  var NUM = 0;
  function incNum(x) {
      return NUM += x;
  };
  function negate(x) {
      return x * -1;
  };
  /* --- extern symbols --- */
  return {
    'incNum': incNum,
    'negate': negate,
    '_internal': {
      'NUM': NUM,
    }
  };
})();

var packB = (function() {
  /* --- import symbols --- */
  var incNum = packA.incNum;
  /* --- define objects --- */
  function decNum(x) {
      return incNum(packA.negate(x));
  };
  function __psMainFunc__() {
      return packA._internal.NUM;
  };
  /* --- extern symbols --- */
  return {
    '_internal': {
      'decNum': decNum,
      '__psMainFunc__': __psMainFunc__,
    }
  };
})();

packB._internal.__psMainFunc__();
```

### Other functionalities

#### `--` macro

This is only the alias of `ps:chain`

```lisp
> (ps. (-- a (b c) d))
;; => "a.b(c).d"
```

----

## Installation

This library is "not" submitted to quicklisp repository. So please do "git clone" this to a proper directory. Then,

```lisp
(ql:quickload :ps-experiment)
```

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
