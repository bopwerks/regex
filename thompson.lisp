;; Thompson NDFA Implementation
;;
;; Example:
;;
;; (let ((m (make-machine '(cat a (or a b)))))
;;   (run-machine m 'a)
;;   (run-machine m 'a)
;;   (run-machine m 'x))

(defun compile-expr (expr code &optional (pc 0))
  (cond ((symbolp expr)
         (compile-symbol expr code pc))
        ((eq (car expr) 'cat)
         (compile-cat expr code pc))
        ((eq (car expr) '*)
         (compile-closure expr code pc))
        ((eq (car expr) 'or)
         (compile-or expr code pc))
        (t (error "Unexpected expression: ~a" expr))))

(defun compile-symbol (expr code pc)
  (vector-push-extend '(nop) code)
  (vector-push-extend `(match ,expr) code)
  (vector-push-extend '(save-pc) code)
  (+ pc 3))

(defun compile-cat (expr code pc)
  (compile-expr (caddr expr)
                code
                (compile-expr (cadr expr)
                              code
                              pc)))

(defun compile-closure (expr code subexpr-pc)
  (let ((closure-pc (compile-expr (cadr expr) code subexpr-pc)))
    (setf (aref code subexpr-pc) `(jump ,closure-pc))
    (vector-push-extend '(fork) code)
    (vector-push-extend `(jump ,(1+ subexpr-pc)) code)
    (+ closure-pc 2)))

(defun compile-or (expr code first-subexpr-pc)
  (let* ((second-subexpr-pc (compile-expr (cadr expr) code first-subexpr-pc))
         (or-expr-pc (compile-expr (caddr expr) code second-subexpr-pc)))
    (setf (aref code first-subexpr-pc) `(jump ,(1+ or-expr-pc)))
    (setf (aref code second-subexpr-pc) `(jump ,(+ 4 or-expr-pc)))
    (vector-push-extend `(jump ,(+ 4 or-expr-pc)) code)
    (vector-push-extend '(fork) code)
    (vector-push-extend `(jump ,(1+ first-subexpr-pc)) code)
    (vector-push-extend `(jump ,(1+ second-subexpr-pc)) code)
    (+ 4 or-expr-pc)))

(defun print-code (code)
  (dotimes (i (length code))
    (format t "~2d ~a~%" i (aref code i))))

(defun make-machine (pattern)
  (let ((code (make-array '(0) :adjustable t :fill-pointer t))
        (current-pcs (list 0))
        (matchp nil))
    (compile-expr pattern code 0)
    ;; (vector-push-extend '(finish) code)
    ;; Replace final 'save-pc opcode with 'finish so the final input signals a match.
    (if (eq (car (aref code (1- (length code)))) 'save-pc)
        (setf (aref code (1- (length code))) '(finish))
        (vector-push-extend '(finish) code))
    (print-code code)
    (labels ((run (input pc current-pcs next-pcs)
               (let* ((inst (aref code pc))
                      (operator (car inst)))
                 (format t "run: ~a~%" inst)
                 (cond ((eq operator 'nop)
                        (run input
                             (1+ pc)
                             current-pcs
                             next-pcs))
                       ((eq operator 'jump)
                        (run input
                             (cadr inst)
                             current-pcs
                             next-pcs))
                       ((eq operator 'save-pc)
                        (if (null current-pcs)
                            (cons (1+ pc) next-pcs)
                            (run input
                                 (car current-pcs)
                                 (cdr current-pcs)
                                 (cons (1+ pc) next-pcs))))
                       ((eq operator 'fork)
                        (run input
                             (+ 2 pc)
                             (cons (+ 1 pc) current-pcs)
                             next-pcs))
                       ((eq operator 'match)
                        (if (eq input (cadr inst))
                            (run input (1+ pc) current-pcs next-pcs)
                            (if (null current-pcs)
                                next-pcs
                                (run input
                                     (car current-pcs)
                                     (cdr current-pcs)
                                     next-pcs))))
                       ((eq operator 'finish)
                        (setf matchp t)
                        ())
                       (t (error "Unknown instruction: ~a" inst)))))
             (handle-input (input)
               (when (not (null current-pcs))
                 (format t "Running~%")
                 (setf current-pcs (run input (car current-pcs) (cdr current-pcs) ())))
               (if (null current-pcs)
                   matchp
                   'pending)))
      #'handle-input)))

(defun run-machine (machine input)
  (funcall machine input))
