![Build Status](https://circleci.com/gh/eshamster/ps-experiment.png?style=shield)

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

### Some top-level definitions and Easy package system

The ps-experiment provides some top-level definitions, `defvar.ps(+)`, `defun.ps(+)`, `defstruct.ps(+).

- `.ps` (Ex. `defvar.ps`): defines only for Parenscript
- `.ps+` (Ex. `defvar.ps+`): defines both for Parenscript and for Common Lisp

Then, you can output these definitions as JavaScript by with-use-ps-pack macro per package. 

The sample [Roswell script](https://github.com/snmsts/roswell):

```lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(ql:quickload :ps-experiment)

(defpackage pack-a
  (:use :cl
        :ps-experiment))
(defpackage pack-b
  (:use :cl
        :ps-experiment))

;; ----- Package A ----- ;;
(in-package :pack-a)

(defvar.ps *num* 0)

(defun.ps inc-num (x)
  (incf *num* x))

(defun.ps add (x y)
  (+ x y))

;; ----- Package B ----- ;;
(in-package :pack-b)

;; *num* in pack-a is not guarded
(defun.ps dec-num (x) 
  (incf *num* x))

;; :this = :pack-b
(defun main (&rest argv)
  (declare (ignorable argv))
  (print
   (with-use-ps-pack (:pack-a :this)
     (inc-num (dec-num 10)))))
```

The output is as below.

```javascript
var NUM = 0;
function incNum(x) {
    return NUM += x;
};
function add(x, y) {
    return x + y;
};
function decNum(x) {
    return NUM -= x;
};
incNum(decNum(10));
```

***Note: If you "use" pack-a in pack-b, you need not write ":pack-a" in the with-use-ps-pack macro.***

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

***Note: The test of this project depends on the fixed version of cl-javascript. Thus, if you want to pass the test, you need to clone the master branch of <https://github.com/akapav/js>.***

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
