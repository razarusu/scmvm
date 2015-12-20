(define (puts . things)
  (for-each display things)
  (newline))

(define-syntax thunk
  (syntax-rules ()
    ((thunk body ...)
     (lambda () body ...))))

(define-syntax define-macro
  (syntax-rules ()
    ((define-macro (name . param) body ...)
     (define-syntax name
       (er-macro-transformer
	 (lambda (exp rename compare)
	   (apply (lambda param body ...)
		  (cdr exp))))))))
