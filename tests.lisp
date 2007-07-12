(defpackage #:mulk.tests
  (:use #:cl #:lift #:mulk.lambda))
(in-package #:mulk.tests)


(deftestsuite mulk-lambda ()
  ())


(deftestsuite mulk-lambda-basic (mulk-lambda)
  ()
  (:equality-test #'equal)
  (:tests
   ((ensure (functionp (fn #'+ _ 10))))
   ((ensure (functionp (fn (+ _ 10)))))
   ((ensure (functionp (fn #'+ _0 _1))))
   ((ensure (functionp (fn #'+ _ _1))))
   ((ensure (functionp (fn #'+ _ (/ _1 2)))))
   ((ensure (functionp (fn #'+ _ (/ _1 2)))))
   ((ensure-same (mapcar (fn (cons _ _)) '(1 2 3))
                 '((1 . 1) (2 . 2) (3 . 3))))
   ((ensure-same (funcall (fn (+ _ 10 _3)) 20 30 40 50)
                 80))
   ((ensure-same (funcall (fn (if (zerop _) t nil)) 0)
                 t))))


(deftestsuite mulk-lambda-fn1 (mulk-lambda)
  ()
  (:tests
   ((ensure-same (funcall (fn () _) 42)
                 42))
   ((ensure-error (funcall (fn _) 42)))
   ((ensure-same (funcall (fn1 _) 42)
                 42))
   ((ensure-error (funcall (fn +))))
   ((ensure-same (funcall (fn1 +))
                 +))
   ((ensure-same (funcall (fn #'+))
                 0))
   ((ensure (functionp (funcall (fn1 #'+)))))))


(deftestsuite mulk-lambda-efn (mulk-lambda)
  ()
  (:tests
   ((ensure (functionp (funcall (fn (fn () _))))))
   ((ensure (functionp (funcall (efn (efn () _))))))
   ((ensure (functionp (funcall (fn1 (fn1 _))))))
   ((ensure (functionp (funcall (efn1 (efn1 _))))))
   ((ensure-same (funcall (fn1 _3) 1 2 3 4 5)
                 4))
   ((ensure-error (funcall (efn1 _3) 1 2 3 4 5)))))


(deftestsuite mulk-lambda-fn* (mulk-lambda)
  ()
  #+nil
  (:tests
   ((ensure-same (funcall (fn* #'+ 1) 2)
                 3))
   ((ensure-same (funcall (fn* #'+) 1 2)
                 3))
   ((ensure-same (funcall (fn* (/ (* _ 4))) 3 6)
                 2))))

