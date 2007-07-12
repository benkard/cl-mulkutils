;;; -*- mode: lisp -*-
;;; Copyright 2007, Matthias Andreas Benkard.

#| Basic usage
 | ===========
 |
 | (fn #'+ _ 10)
 | (fn (+ _ 10))
 | (fn #'+ _0 _1)
 | (fn #'+ _ _1)
 | (fn #'+ _ (/ _1 2))
 | (mapcar (fn (cons _ _)) '(1 2 3))       ;=> ((1 . 1) (2 . 2) (3 . 3))
 | (funcall (fn (+ _ 10 _3)) 20 30 40 50)  ;=> 80
 |
 |
 | Simple variant FN1
 | ==================
 |
 | (funcall (fn () _) 42)                ;=> 42
 | (funcall (fn _) 42)                   ;=> error (usually)
 | (funcall (fn1 _) 42)                  ;=> 42
 | (funcall (fn +))                      ;=> error (usually)
 | (funcall (fn1 +))                     ;=> value of +
 | (funcall (fn #'+))                    ;=> 0
 | (funcall (fn1 #'+))                   ;=> #<FUNCTION +>
 |
 |
 | Argument-number-safe variant EFN
 | ================================
 |
 | (funcall (fn (fn () _)))              ;=> #<LAMBDA ...>
 | (funcall (efn (efn () _)))            ;=> #<LAMBDA ...>  ; new!
 | (funcall (fn1 (fn1 _)))               ;=> #<LAMBDA ...>
 | (funcall (efn1 (efn1 _)))             ;=> #<LAMBDA ...>  ; new!
 | (funcall (fn1 _3) 1 2 3 4 5)          ;=> 4
 | (funcall (efn1 _3) 1 2 3 4 5)         ;=> error
 |#


(defpackage #:mulk.lambda
  (:use #:cl)
  (:export #:efn #:fn #:efn1 #:fn1))
(in-package #:mulk.lambda)


(defun collect-lambda-args (form)
  (let ((lambda-args (list)))
    (handler-bind
        ((arnesi:undefined-variable-reference
          #'(lambda (c)
              (with-accessors ((symbol arnesi:name)) c
                (let ((name (symbol-name symbol)))
                  (when (or (string= "_" name)
                            (and (char= #\_ (char name 0))
                                 (ignore-errors
                                   (parse-integer (subseq name 1)))))
                    (push symbol lambda-args))))
              (invoke-restart 'muffle-warning))))
      (let ((arnesi:*warn-undefined* t))
        (arnesi:walk-form form)
        (nreverse lambda-args)))))


(defun find-lambda-args (clause)
  (let* ((lambda-args (collect-lambda-args clause))
         (max-arg-no  (loop for lambda-arg in lambda-args
                            maximizing
                              (let ((name (symbol-name lambda-arg)))
                                (if (string= "_" name)
                                    0
                                    (parse-integer (subseq name 1))))))
         (arglist     (loop for i from 0 to (if (not (null lambda-args))
                                                max-arg-no
                                                -1)
                            collecting
                              (let ((symbol (find (format nil "_~D" i)
                                                  lambda-args
                                                  :key #'symbol-name
                                                  :test #'string=)))
                                ;; If the user does not use a particular
                                ;; positional argument, we shall not
                                ;; introduce its name, either.  Use a
                                ;; GENSYM instead.
                                (or symbol (gensym (format nil "_~D_" i)))))))
    (cond ((null lambda-args)           nil)
          ((position "_" lambda-args
                     :key #'symbol-name
                     :test #'string=)   (cons (intern "_" *package*) arglist))
          (t                            arglist))))


(defun normalise-lambda-args (lambda-args)
  "Normalise a MULK.LAMBDA argument list.

Aim: Remove the symbol _ from LAMBDA-ARGS if it exists, as its purpose
is identical to _0.  Return both the normalised LAMBDA-ARGS and the
removed symbol (if found)."
  (values (remove "_" lambda-args :key #'symbol-name :test #'string=)
          (find   "_" lambda-args :key #'symbol-name :test #'string=)))


(defmacro efn (function-or-form &rest args)
  "A convenience wrapper for LAMBDA.

Positional arguments are numbered from 0 and follow the pattern given by the
format string \"_~D\".  _ can be used as an abbreviation for _0."
  (cond ((or (not (listp function-or-form))
             (eq 'function (first function-or-form)))
         `(efn (funcall ,function-or-form ,@args)))
        (t
         (let ((lambda-args      (find-lambda-args `(progn
                                                      ,function-or-form
                                                      ,@args))))
           (multiple-value-bind (real-lambda-args _)
               (normalise-lambda-args lambda-args)
             `(symbol-macrolet ,(if _
                                    `((,_ ,(first real-lambda-args)))
                                    nil)
                (function (lambda ,real-lambda-args
                  (declare (ignorable ,@real-lambda-args))
                  ,function-or-form
                  ,@args))))))))


(defmacro fn (function-or-form &rest args)
  "A less safe but recursively callable variant of EFN.

This macro is like EFN save the fact that the anonymous functions it
produces do not check the number of their arguments."
  (cond ((or (not (listp function-or-form))
             (eq 'function (first function-or-form)))
         `(fn (funcall ,function-or-form ,@args)))
        (t
         (let ((lambda-args      (find-lambda-args `(progn
                                                      ,function-or-form
                                                      ,@args)))
               (args-sym         (gensym "FN-ARGS")))
           (multiple-value-bind (real-lambda-args _)
               (normalise-lambda-args lambda-args)
             `(symbol-macrolet (,@(if _
                                      `((,_ ,(first real-lambda-args)))
                                      nil)
                                ,@(loop for i from 0
                                        for lambda-arg in real-lambda-args
                                        collect `(,lambda-arg
                                                  (nth ,i ,args-sym))))
                (function (lambda (&rest ,args-sym)
                  (declare (ignorable ,args-sym))
                  ,function-or-form
                  ,@args))))))))


(defmacro efn1 (value-or-form &rest forms)
  "A variant of EFN that does not try to interpret its first argument as
  a function name.

  Useful for stuff like (EFN1 _3).

  (EFN1 a b c ...) is semantically equivalent to (EFN () a b c ...)."
  (cond ((or (not (listp value-or-form))
             (eq 'function (first value-or-form)))
         `(efn () ,value-or-form ,@forms))
        (t                           `(efn ,value-or-form ,@forms))))


(defmacro fn1 (value-or-form &rest forms)
  "A variant of FN that does not try to interpret its first argument as
  a function name.

  Useful for stuff like (FN1 _3).

  (FN1 a b c ...) is semantically equivalent to (FN () a b c ...)."
  (cond ((or (not (listp value-or-form))
             (eq 'function (first value-or-form)))
         `(fn () ,value-or-form ,@forms))
        (t                           `(fn ,value-or-form ,@forms))))
