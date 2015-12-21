;(declare (unit vm)) ; For some reason this forbids (require 'vm)
(declare (export vm:run vm:step vm:dump-registers
		 vm:reset vm:load-program vm:assemble-from-file))

(define (str . items)
  (apply string-append (map ->string items)))

(define (build-symbol . name-parts)
  (string->symbol (apply str name-parts)))

;; (define-stack foo) will generate:
;;  - variable `foo-stack';
;;  - function `push-foo!';
;;  - function `pop-foo!'.

(define-macro (define-stack name)
  (let ((stack-name (build-symbol name '-stack))
	(push-name (build-symbol 'push- name '!))
	(pop-name (build-symbol 'pop- name '!)))
    `(begin
       (define ,stack-name '())
       (define (,push-name obj)
	 (set! ,stack-name (cons obj ,stack-name)))
       (define (,pop-name)
	 (let ((obj (car ,stack-name)))
	   (set! ,stack-name (cdr ,stack-name))
	   obj)))))

(define-stack addr)

(define-stack data)

(define program (make-vector 64 'halt))

(define (program-set! addr val)
  (vector-set! program addr val))

(define (vm:load-program prg)
  (set! program prg))

(define (make-register)
  (list 'register 0))

(define registers
  `((pc . ,(make-register))
    (r1 . ,(make-register))
    (r2 . ,(make-register))
    (r3 . ,(make-register))
    (r4 . ,(make-register))
    (r5 . ,(make-register))
    (r6 . ,(make-register))
    (r7 . ,(make-register))
    (r8 . ,(make-register))))

(define (vm:dump-registers)
  (for-each (lambda (pair)
	      (printf "~A: ~A~%"
		      (car pair)
		      (regvalue (cdr pair))))
	    registers))

(define (register sym)
  (let ((reg (assq sym registers)))
    (if reg
	(cdr reg)
	(error "Does not denote a register" sym))))

(define (set-register! reg val)
  (set-car! (cdr reg) val))

(define (vm:reset)
  (set-register! (register 'pc) 0)
  (set! code-stack 0))

(define (regvalue reg)
  (second reg))

(define (fetch!)
  (let ((program-counter (regvalue (register 'pc))))
    (if (< program-counter (vector-length program))
	(let ((fetched (vector-ref program program-counter)))
	  (set-register! (register 'pc) (+ 1 program-counter))
	  fetched)
	'halt)))

(define (jump addr)
  (set-register! (register 'pc) addr))

;; (define-instruction load ((register dst)
;; 			  (immediate val))
;;   (set-register! dst val))

;; (let ((hdlr (lambda (src dst)
;; 	      (let* ((dst (register (fetch!)))
;; 		     (val (immediate (fetch!))))
;; 		(set-register! dst (regvalue src))))))
;;   (cons (cons 'copy hdlr) instruction-handlers))

(define (vm:step)
  (let ((instruction (fetch!)))
    (case instruction
      
      ((load)
       (let* ((dest (register (fetch!)))
	      (value (fetch!)))
	 (set-register! dest value)))

      ((copy)
       (let* ((source (register (fetch!)))
	      (dest (register (fetch!))))
	 (set-register! dest (regvalue source))))

      ((push)
       (let ((src (register (fetch!))))
	 (push-data! (regvalue src))))

      ((pop)
       (let ((dst (register (fetch!))))
	 (set-register! dst (pop-data!))))

      ((jump)
       (jump (fetch!)))

      ((jump-if-zero)
       (let* ((reg (register (fetch!)))
	      (address (fetch!)))
	 (if (zero? (regvalue reg))
	     (jump address))))

      ((jump-if-bigger)
       (let* ((reg1 (register (fetch!)))
	      (reg2 (register (fetch!)))
	      (addr (fetch!)))
	 (if (> (regvalue reg1)
		(regvalue reg2))
	     (jump addr))))

      ((travel) ; Kinda CALL
       (let* ((proc-addr (fetch!)))
	 (push-addr! (regvalue (register 'pc)))
	 (jump proc-addr)))

      ((return)
       (let* ((return-addr (pop-addr!)))
	 (set-register! (register 'pc)
			return-addr)))

      ((add)
       (let* ((src1 (register (fetch!)))
	      (src2 (register (fetch!)))
	      (dest (register (fetch!))))
	 (set-register! dest (+ (regvalue src1)
				(regvalue src2)))))

      ((sub)
       (let* ((src1 (register (fetch!)))
	      (src2 (register (fetch!)))
	      (dest (register (fetch!))))
	 (set-register! dest (- (regvalue src1)
				(regvalue src2)))))

      ((mul)
       (let* ((src1 (register (fetch!)))
	      (src2 (register (fetch!)))
	      (dest (register (fetch!))))
	 (set-register! dest (* (regvalue src1)
				(regvalue src2)))))

      ((div)
       (let* ((src1 (register (fetch!)))
	      (src2 (register (fetch!)))
	      (dst1 (register (fetch!)))
	      (dst2 (register (fetch!))))
	 (set-register! dst1 (quotient (regvalue src1)
				       (regvalue src2)))
	 (set-register! dst2 (modulo (regvalue src1)
				     (regvalue src2)))))

      ((neg)
       (let* ((reg (register (fetch!))))
	 (set-register! reg (- (regvalue reg)))))

      ((out)
       (print (regvalue (register (fetch!)))))

      ((in)
       (set-register! (register (fetch!))
		      (read)))

      ((halt) 'halt)

      ((dec)
       (let* ((reg (register (fetch!))))
	 (set-register! reg (- (regvalue reg) 1))))

      ((inc)
       (let* ((reg (register (fetch!))))
	 (set-register! reg (+ (regvalue reg) 1))))

      (else
       (error "Not a correct instruction" instruction))

      )))

(define (vm:run)
  (unless (eq? (vm:step) 'halt)
    (vm:run)))

(define (symbol->keyword sym)
  (string->keyword (symbol->string sym)))

(define (vm:assemble-from-file source origin)
  (with-input-from-file source
    (thunk
      (let loop ((thing (read))
		 (program-counter origin)
		 (labels '())		  ; alist
		 (label-references '()))  ; alist
	(cond ((eq? thing '#!eof)	  ; Stop assembling on EOF
	       (fill-holes! label-references labels))
	      ((vector? thing)		; Vectors denote comments
	       (loop (read) program-counter labels label-references))
	      ((keyword? thing)		  ; Keywords denote labels
	       (loop (read) program-counter
		     (cons (cons thing program-counter)
			   labels)
		     label-references))
	      ((list? thing)		; Lists denote label references
	       (program-set! program-counter '????)
	       (loop (read) (+ 1 program-counter)
		     labels (cons (cons program-counter thing)
				  label-references)))
	      (else
	       (program-set! program-counter thing)
	       (loop (read) (+ 1 program-counter)
		     labels label-references)))))))


(define (fill-holes! label-references labels)
  (unless (null? label-references)
    (let* ((ref (car label-references))
	   (address (car ref))
	   (label-name (symbol->keyword (cadr ref)))
	   (offset (if (null? (cddr ref)) #f (caddr ref)))
	   (label (assq label-name labels)))
      (if label
	  (begin
	    (program-set! (+ address (if offset offset 0))
			  (cdr label))
	    (fill-holes! (cdr label-references)
			 labels))
	  (printf "WARNING: Unresolved reference: ~A~%"
		  label-name)))))
