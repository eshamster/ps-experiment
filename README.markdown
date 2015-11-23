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
;; this can't be support
(ps. `(setf ,pnt.x 100))
```

#### Camel case (read macro)

In \#j. (sharp j dot) read macro, camel cases are supported.   

```lisp
#j.THREE.WebGLRenderer#
;; => -t-h-r-e-e.-web-g-l-renderer
```

### Some definitions and Easy package system

The ps-experiment provides some top-level definitions, defvar.ps, defun.ps, defstruct.ps.

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
     (inc-num (dec-num)))))
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
incNum(decNum());
```

----

## Installation

This library is not submitted to quicklisp repository. So please do "git clone" this to a proper directory. Then,

```lisp
(ql:quickload :ps-experiment)
```

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
