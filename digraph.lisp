;; NDFA simulation as described in: https://swtch.com/~rsc/regexp/regexp1.html
;; Try matching number sequences: (matchp '(cat oddp (cat evenp evenp)) 3 2 2 1)

(setf *print-circle* t)

(defun compile-expr (expr)
  (cond ((symbolp expr)
         (compile-symbol expr))
        ((eq (car expr) 'cat)
         (compile-cat expr))
        ((eq (car expr) '*)
         (compile-closure expr))
        ((eq (car expr) 'or)
         (compile-or expr))
        (t (error "Unexpected expression: ~a" expr))))

(defun make-fragment (state outs)
  (list state outs))

(defun fragment-state (fragment)
  (car fragment))

(defun fragment-outs (fragment)
  (cadr fragment))

(defun make-state (&key label out1 out2 (list-id 0))
  (list label list-id out1 out2))

(defun state-label (state)
  (car state))

(defun state-out1 (state)
  (cddr state))

(defun state-out2 (state)
  (cdddr state))

(defun state-list-id (state)
  (cdr state))

(defun state-outs (state)
  (list (state-out1 state)
        (state-out2 state)))

(defun compile-symbol (expr)
  (let ((state (make-state :label expr)))
    (make-fragment state (list (state-out1 state)))))

;; Set each from-fragment.out = to-state
(defun patch (from-fragment to-state)
  (dolist (out (fragment-outs from-fragment))
    (setf (car out) to-state))
  from-fragment)

(defun compile-cat (expr)
  (let ((frag1 (compile-expr (cadr expr)))
        (frag2 (compile-expr (caddr expr))))
    (patch frag1 (fragment-state frag2))
    (make-fragment (fragment-state frag1)
                   (fragment-outs frag2))))

(defun compile-closure (expr)
  (let* ((child-frag (compile-expr (cadr expr)))
         (closure-state (make-state :out1 (fragment-state child-frag))))
    (patch child-frag closure-state)
    (make-fragment closure-state (list (state-out2 closure-state)))))
                   
(defun compile-or (expr)
  (let* ((frag1 (compile-expr (cadr expr)))
         (frag2 (compile-expr (caddr expr)))
         (or-state (make-state :out1 (fragment-state frag1)
                               :out2 (fragment-state frag2))))
    (make-fragment or-state
                   (append (fragment-outs frag1)
                           (fragment-outs frag2)))))

(defun reachable-states (state list-id)
  (cond ((null state) ())
        ((= (car (state-list-id state)) list-id) ())
        (t (setf (car (state-list-id state)) list-id)
           (if (null (state-label state))
               (append (reachable-states (car (state-out1 state)) list-id)
                       (reachable-states (car (state-out2 state)) list-id))
               (list state)))))

(defun state-matches-input (state input)
  (funcall (state-label state) input))

(defun flatmap (fn lst)
  (apply #'append (mapcar fn lst)))

(defun next-states (input clist step-id)
  (flatmap (lambda (from-state)
             (reachable-states (car (state-out1 from-state)) step-id))
           (remove-if-not (lambda (state)
                            (state-matches-input state input))
                          clist)))

(defun matchp (expr &rest inputs)
  (let ((fragment (compile-expr expr))
        (match-state (make-state :label (lambda (input) (declare (ignore input)) t))))
    (patch fragment match-state)
    (let ((start-state (fragment-state fragment)))
      (labels ((iter (inputs possible-next-states step-id)
                 (if (null inputs)
                     (not (null (member match-state possible-next-states)))
                     (iter (cdr inputs)
                           (next-states (car inputs)
                                        possible-next-states
                                        step-id)
                           (1+ step-id)))))
        (iter inputs (reachable-states start-state 1) 2)))))

(defun cat-sequence (seq)
  (cond ((null seq) ())
        ((null (cdr seq)) (car seq))
        (t `(cat ,(car seq) ,(cat-sequence (cdr seq))))))

