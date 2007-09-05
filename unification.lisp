(in-package #:mulk.utils)


(defun split-off-declarations (clauses)
  (let* ((docstring-found-p nil)
         (decl-end (mismatch clauses clauses
                             :test #'(lambda (x y)
                                       (declare (ignore y))
                                       (if (stringp x)
                                           (and (not docstring-found-p)
                                                (setq docstring-found-p t))
                                           (and (listp x)
                                                (eq 'declare (car x))))))))
    (if (null decl-end)
        (values clauses nil)
        (values (subseq clauses 0 decl-end)
                (subseq clauses decl-end)))))



(defmacro matching-defun (function-name lambda-list &body clauses)
  "Define a pattern-matching function.

clauses ::= [[declaration\\* | documentation]] pattern-clause\\*

pattern-clause ::= (pattern forms\\*)


## Arguments and Values:

*function-name* --- a **function name**.

*lambda-list* --- an **ordinary lambda list**.

*declaration* --- a __declare__ **expression**; not evaluated.

*documentation* --- a **string**; not evaluated.

*pattern* --- a **list**; not evaluated.

*form\\** --- an **implicit progn**.

Returns: *function-name* --- a **function name**.


## Description:

If *lambda-list* is __nil__, all arguments are used for matching.
Otherwise, it is expected to contain the **symbol** __*__ as if it were
the name of a **positional argument**.  In this case, the appropriate
number of pattern arguments are inserted at the position of __*__ in
*lambda-list*.

All patterns must be congruent -- that is, they must match a fixed
number of arguments, unless all of the following conditions are met:

1. __*__ appears as the very last **positional argument** (including
`&optional` **argument**s) in *lambda-list*.

2. None of `&key`, `&rest` or `&allow-other-keys` is found in
*lambda-list*.

In this case, __*__ is treated as a `&rest` **argument** to be matched
by *clauses*.

__*__ may appear anywhere after `&optional`, but not after any other
**lambda-list keyword**.

The results of supplying the **symbol** __*__ in a position not
indicating a **positional argument** or supplying the same **symbol**
more than once are undefined, even if the second occurrence is in a
position not indicating a **positional argument** (that is, an invalid
position).

Note that the following definitions are all equivalent:

    (defun f ())
    (defun f (*))
    (defun f (&optional *))


## Examples:

    (matching-defun fac ()
      ((0) 1)
      ((?n) (* n (fac (1- n)))))
     ;=> FAC

    (matching-defun fac-iter (* &optional (accumulator 1))
      \"An iterative version of FAC.\"
      ((0) accumulator)
      ((?n) (fac-iter (1- n) (* accumulator n))))
     ;=> FAC-ITER

    (matching-defun direction (* &key numericp)
      ((:up) (if numericp 0 \"Up!\"))
      ((:down) (if numericp 1 \"Down!\"))
      ((:left) (if numericp 2 \"Left!\"))
      ((:right) (if numericp 3 \"Right!\")))
     ;=> DIRECTION

    (fac 10)  ;=> 3628800
    (fac-iter 10)  ;=> 3628800
    (fac-iter 10 11)  ;=> 39916800
    (direction :left)  ;=> \"Left!\"
    (direction :left :numericp t)  ;=> 2


## See Also:

  __defun__, __matching-labels__, __matching-flet__"

  (when (null lambda-list)
    (setq lambda-list '(*)))

  (if (not (member '* lambda-list))
      `(defun ,function-name ,lambda-list
         (unify:match-case (list ,@(mapcar #'(lambda (arg)
                                               (if (atom arg)
                                                   arg
                                                   (first arg)))
                                           lambda-list))
           ,@clauses))
      (let* ((star-position (position '* lambda-list))
             (star-wildp (and (or (endp (nthcdr (1+ star-position) lambda-list))
                                  (member (nth (1+ star-position) lambda-list)
                                          '(&aux)))
                              (null (intersection '(&key &rest &allow-other-keys)
                                                  lambda-list)))))
          (multiple-value-bind (declarations pattern-clauses)
              (split-off-declarations clauses)
            (let* ((star-args-num (if (or star-wildp (null pattern-clauses))
                                      0
                                      (length (first (first pattern-clauses)))))
                   (star-args-syms
                    (mapcar #'gensym (make-list star-args-num
                                                :initial-element "PATTERN-ARG")))
                   (wild-star-sym (gensym)))
              (assert (or star-wildp
                          (every #'(lambda (c)
                                     (= (length (first c)) star-args-num))
                                 pattern-clauses))
                      (clauses lambda-list)
                      "Patterns must be congruent if * is non-wild")
              `(defun ,function-name ,(mapcan #'(lambda (x)
                                                  (if (eq x '*)
                                                      (if star-wildp
                                                          (list '&rest
                                                                wild-star-sym)
                                                          (copy-list star-args-syms))
                                                      (list x)))
                                              lambda-list)
                 ,@declarations
                 (unify:match-case (,(if star-wildp
                                         wild-star-sym
                                         `(list ,@star-args-syms)))
                   ,@(mapcar #'(lambda (clause)
                                 (cons (list 'quote (car clause))
                                       (cdr clause)))
                             pattern-clauses))))))))
