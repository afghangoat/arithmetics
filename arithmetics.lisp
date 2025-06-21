;;;; All is linked
;;;Contains Euclidean GCD and Euler totient functions

;;;Macros
(defmacro spacing()
(princ " "))

(defmacro while (condition &body body)
  `(loop while ,condition do (progn ,@body)))
; usage:
; (while (condition) (what to do in while))
;

;;;Other definitions
(defconstant *PI* 3.141592)
(defvar *n1* 0)

;;;Functions
(defun egcd(a b)
  "Using the Euclidean algorithm"
  (check-type a integer)
  (check-type b integer)
  (
    let ((Euclideant 0)
    (ta a)
    (tb b))
    
    (while (> ta 0)
      (setq Euclideant ta)
      (setq ta (mod tb ta))
      (setq tb Euclideant)
    )
    tb
  )
)

(defun fi (num)
  "The Euler totient function"
  (check-type num integer)
  (cond ((< num 1)
    (
      error "FI was given a negative or 0,1 argument." num)
    )
  )
  (
    
    let ((amm 0))
    (loop for x from 1 to num by 1 do
      (when (= (egcd x num) 1)
        (
          setq amm (+ amm 1)
        )
      )
    )
    amm
  )
)

;(
;  print (mapcar #'fi' (
;    5 10 20
;    )
;  )
;)

(format t "Please input a number, the totient will be calculated!:")
(setq *n1* (read))
(princ "fi(N) is ")
(format t "fi(N) is ~A~%" (fi *n1*))