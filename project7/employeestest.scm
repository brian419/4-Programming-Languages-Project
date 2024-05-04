
(define (print-file filename) 
    (with-input-from-file filename
    (lambda ()
    (let loop ((line (read-line)))
        (unless (eof-object? line)
        (display line)
        (newline)
        (loop (read-line)))))))


(define (make-salaried-employee firstName lastName salary)
(lambda (action)
    (case action
        ((get-firstName) firstName)
        ((get-lastName) lastName)
        ((get-salary) salary)
        )))


(define (make-hourly-employee firstName2 lastName2 hours-worked hourly-rate)

    (lambda (action)
        (case action
            ((get-firstName) firstName2)
            ((get-lastName) lastName2)
            ((get-hoursWorked) hours-worked)
            ((get-hourlyRate) hourly-rate)
            ((get-salary)
                (cond
                    ((<= hours-worked 40) (* hours-worked hourly-rate))
                    ((<= hours-worked 50) (+ (* 40 hourly-rate) (* (- hours-worked 40) (* hourly-rate 1.5))))
                    (else (+ (* 40 hourly-rate) (* 10 (* hourly-rate 1.5)) (* (- hours-worked 50) (* hourly-rate 2))))
                )
            )

        )
    )
)

(define (make-commission-employee firstName3 lastName3 base-salary sales commission-rate)
    (lambda (action)
        (case action
            ((get-firstName) firstName3)
            ((get-lastName) lastName3)
            ((get-baseSalary) base-salary)
            ((get-sales) sales)
            ((get-commissionRate) commission-rate)
            ((get-salary) (if (> (* sales commission-rate) base-salary) (* sales commission-rate) base-salary))
        )
    )
) 

(define (string-contains? str substr)
    (if (string=? substr "")
        #t
        (let loop ((str str)
            (substr substr))
            (cond ((string=? (substring str 0 (string-length substr)) substr)
                #t)
                ((string=? str "")
                #f)
            (else
                (loop (substring str 1) substr))))))


(define (str-split str ch)
    (let ((len (string-length str)))
        (letrec
            ((split
                (lambda (a b)
                    (cond
                        ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
                        ((char=? ch (string-ref str b)) (if (= a b)
                        (split (+ 1 a) (+ 1 b))
                        (cons (substring str a b) (split b b))))
                    (else (split a (+ 1 b)))))))
                        (split 0 0))))


(define (perform filename . args)

    (if (file-exists? filename)
        (begin ;if the file does exist
            (if (null? args)
                (begin
                    (newline)
                    (display "Usage: (perform employee_file action)\n")
                    (display "or\n")
                    (display "Usage: (perform employee_file action operator threshold)\n")
                    (newline)
                    (display "Valid actions: count print min max total avg\n")
                    (display "Valid operators: eq ne gt ge lt\n")
                    (newline)
                )
                (let ((action (car args)))
                    (cond ((string=? action "count") ; eq ne gt ge lt le
                        (cond
                            ((null? (cdr args))
                                (let ((file (open-input-file filename)))
                                    (let loop ((count 0)
                                        (line (read-line file))) 
                                        (if (eof-object? line) 
                                            (begin
                                                (close-input-port file) 
                                                (display "There are ") 
                                                (display count) 
                                                (display " employees") ;
                                                (newline)
                                            )
                                            (loop (if (string=? line "") count (+ count 1)) (read-line file))
                                        )
                                    )
                                )
                            )
                            ((string=? (car (cdr args)) "eq")
                                
                                ;print the number of employees that have a salary equal to the threshold
                                (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display count)                                                                                            
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                        (set! count (+ count 1))
                                                                                                                                        
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            
                                                            
                                                            )    

                                                        )
                                                        (loop threshold count)
                                                    )
                                                    (else (loop threshold count))                                            
                                                )

                                            )
                                        )
                                        
                                    )
                                
                            )
                            ((string=? (car (cdr args)) "ne")
                                
                                ;print the number of employees that have a salary equal to the threshold
                                (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display count)                                                                                            
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))                                                        
                                                                    (if (not (= salary threshold))
                                                                        (set! count ( + count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (not (= salary threshold))
                                                                        (set! count ( + count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (not (= salary threshold))
                                                                        (set! count ( + count 1))
                                                                    )
                                                                )
                                                            )
                                                            
                                                            
                                                            )    

                                                        )
                                                        (loop threshold count)
                                                    )
                                                    (else (loop threshold count))                                            
                                                )

                                            )
                                        )
                                        
                                    )
                                
                            )
                            ((string=? (car (cdr args)) "gt")
                                
                                ;print the number of employees that have a salary equal to the threshold
                                (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display count)                                                                                            
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))                                                        
                                                                    (if (> salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (> salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (> salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            
                                                            
                                                            )    

                                                        )
                                                        (loop threshold count)
                                                    )
                                                    (else (loop threshold count))                                            
                                                )

                                            )
                                        )
                                        
                                    )
                                
                            )
                            ((string=? (car (cdr args)) "ge")
                                
                                ;print the number of employees that have a salary equal to the threshold
                                (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display count)                                                                                            
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))                                                        
                                                                    (if (>= salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (>= salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (>= salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            
                                                            
                                                            )    

                                                        )
                                                        (loop threshold count)
                                                    )
                                                    (else (loop threshold count))                                            
                                                )

                                            )
                                        )
                                        
                                    )
                                
                            )
                            ((string=? (car (cdr args)) "lt")
                                
                                ;print the number of employees that have a salary equal to the threshold
                                (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display count)                                                                                            
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))                                                        
                                                                    (if (< salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (< salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (< salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            
                                                            
                                                            )    

                                                        )
                                                        (loop threshold count)
                                                    )
                                                    (else (loop threshold count))                                            
                                                )

                                            )
                                        )
                                        
                                    )
                                
                            )
                            ((string=? (car (cdr args)) "le")
                                
                                ;print the number of employees that have a salary equal to the threshold
                                (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display count)                                                                                            
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))                                                        
                                                                    (if (<= salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (<= salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (<= salary threshold)
                                                                        (set! count (+ count 1))
                                                                    )
                                                                )
                                                            )                                                    
                                                            )    

                                                        )
                                                        (loop threshold count)
                                                    )
                                                    (else (loop threshold count))                                            
                                                )

                                            )
                                        )
                                        
                                    )
                                
                            )
                        )
                        
                        (newline)
                        )
                        ((string=? action "total")
                            (cond
                                ((null? (cdr args))
                                    (let ((file (open-input-file filename)))
                                        (let loop ((total 0))
                                            (let ((line (read-line file)))
                                                (cond ((eof-object? line)
                                                    (close-input-port file)
                                                    (display "Total payment is $")
                                                    (display total)
                                                    (newline)
                                                    (newline))
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                (set! total (+ total ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                )
                                                                ((string=? (car words) "hourly")
                                                                    (set! total (+ total ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                )
                                                                ((string=? (car words) "commission")
                                                                    (set! total (+ total ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                )
                                                            )                                            
                                                        )
                                                        (loop total)                                    
                                                    ) 
                                                    (else (loop total))      
                                                )                                
                                            )                            
                                        )                        
                                    )
                                )
                                ((string=? (car (cdr args)) "eq")
                                    ;prints the total earnings of all the employee[s] whose earned salary is equal to the threshold
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)  
                                                        (display total)                                                                                                                                    
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                        (set! total (+ total ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                        (set! total (+ total ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                        (set! total (+ total ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                        
                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold total)
                                                    )
                                                    (else (loop threshold total))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "ne")
                                    ;prints the total earnings of all the employee[s] whose earned salary is not equal to the threshold
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)  
                                                        (display total)                                                                                                                                    
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    (if (not (= salary threshold))
                                                                        (set! total (+ total ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))

                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    
                                                                    (if (not (= salary threshold))
                                                                        (set! total (+ total ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))

                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    
                                                                    (if (not (= salary threshold))
                                                                        (set! total (+ total ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                        

                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold total)
                                                    )
                                                    (else (loop threshold total))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "gt")
                                    ;prints the total earnings of all the employee[s] whose earned salary is greather than the threshold
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)  
                                                        (display total)                                                                                                                                    
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    (if (> salary threshold)
                                                                        (set! total (+ total ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    
                                                                    (if (> salary threshold)
                                                                        (set! total (+ total ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    
                                                                    (if (> salary threshold)
                                                                        (set! total (+ total ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                        
                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold total)
                                                    )
                                                    (else (loop threshold total))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "ge")
                                    ;prints the total earnings of all the employee[s] whose earned salary is greather than the threshold
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)  
                                                        (display total)                                                                                                                                    
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    (if (>= salary threshold)
                                                                        (set! total (+ total ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    
                                                                    (if (>= salary threshold)
                                                                        (set! total (+ total ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    
                                                                    (if (>= salary threshold)
                                                                        (set! total (+ total ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                        
                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold total)
                                                    )
                                                    (else (loop threshold total))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "lt")
                                    ;prints the total earnings of all the employee[s] whose earned salary is greather than the threshold
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)  
                                                        (display total)                                                                                                                                    
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    (if (< salary threshold)
                                                                        (set! total (+ total ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    
                                                                    (if (< salary threshold)
                                                                        (set! total (+ total ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    
                                                                    (if (< salary threshold)
                                                                        (set! total (+ total ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                        
                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold total)
                                                    )
                                                    (else (loop threshold total))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "le")
                                    ;prints the total earnings of all the employee[s] whose earned salary is greather than the threshold
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)  
                                                        (display total)                                                                                                                                    
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    (if (<= salary threshold)
                                                                        (set! total (+ total ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    
                                                                    (if (<= salary threshold)
                                                                        (set! total (+ total ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    
                                                                    (if (<= salary threshold)
                                                                        (set! total (+ total ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                        
                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold total)
                                                    )
                                                    (else (loop threshold total))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                            )
                            
                        )
                        ((string=? action "print")
                            "conditional to check if the next two arguments are not there, then print 'proceed as normal'"
                            (cond
                                ((null? (cdr args))
                                    (let ((file (open-input-file filename))) 
                                        (let loop ()
                                            (let ((line (read-line file)))
                                                (cond ((eof-object? line) 
                                                    (close-input-port file) 
                                                    (newline))
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                (display "Salaried employee: ")
                                                                (display ((make-salaried-employee (car (cdr words) ) (car (cdr (cdr words))) (string->number(car (cdr (cdr (cdr words)))))) 'get-firstName))
                                                                (display " ")  
                                                                (display ((make-salaried-employee (car (cdr words) ) (car (cdr (cdr words))) (string->number(car (cdr (cdr (cdr words)))))) 'get-lastName))
                                                                (newline)
                                                                (display "weekly salary: ")
                                                                (display ((make-salaried-employee (car (cdr words) ) (car (cdr (cdr words))) (string->number(car (cdr (cdr (cdr words)))))) 'get-salary))
                                                                (display " ")
                                                                (newline)
                                                                (display "earned $")
                                                                (display ((make-salaried-employee (car (cdr words) ) (car (cdr (cdr words))) (string->number(car (cdr (cdr (cdr words)))))) 'get-salary))
                                                                (newline)
                                                                (newline)
                                                                )
                                                                ((string=? (car words) "hourly")
                                                                (display "Hourly employe: ")
                                                                (display ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-firstName))
                                                                (display " ")
                                                                (display ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-lastName))
                                                                (newline)
                                                                (display "hours worked: " )
                                                                (display ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-hoursWorked))
                                                                (display ", hourly rate: ")
                                                                (display ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-hourlyRate))
                                                                (newline)
                                                                (display "earned $")
                                                                (display ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary))
                                                                (newline)
                                                                (newline)
                                                                )
                                                                ((string=? (car words) "commission")
                                                                (display "Commission employee: ")
                                                                (display ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (car (cdr (cdr (cdr words)))) (car (cdr (cdr (cdr (cdr words))))) (car (cdr (cdr (cdr (cdr (cdr words))))))) 'get-firstName))
                                                                (display " ")
                                                                (display ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (car (cdr (cdr (cdr words)))) (car (cdr (cdr (cdr (cdr words))))) (car (cdr (cdr (cdr (cdr (cdr words))))))) 'get-lastName))
                                                                (newline)
                                                                (display "minimum salary: ")
                                                                (display ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (car (cdr (cdr (cdr words)))) (car (cdr (cdr (cdr (cdr words))))) (car (cdr (cdr (cdr (cdr (cdr words))))))) 'get-baseSalary))
                                                                (display ", sales amount: ")
                                                                (display ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (car (cdr (cdr (cdr words)))) (car (cdr (cdr (cdr (cdr words))))) (car (cdr (cdr (cdr (cdr (cdr words))))))) 'get-sales))
                                                                (display ", commission rate: ")
                                                                (display ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (car (cdr (cdr (cdr words)))) (car (cdr (cdr (cdr (cdr words))))) (car (cdr (cdr (cdr (cdr (cdr words))))))) 'get-commissionRate))
                                                                (newline)
                                                                (display "earned $")
                                                                (display ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number(car (cdr (cdr (cdr words))))) (string->number(car (cdr (cdr (cdr (cdr words)))))) (string->number(car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary))
                                                                (newline)
                                                                (newline)                                                
                                                                )
                                                            )   
                                                        )
                                                        (loop) 
                                                    )
                                                    (else (loop)) 
                                                )
                                            )
                                        )    
                                    )
                                )
                                ((string=? (car (cdr args)) "ge")
                                    ;(display (car (cdr (cdr args))))        

                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (ge-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)

                                                        "look through the max-employees list and if the employee is 'salaried' then print the word 'salaried to the terminal"
                                                        (let loop ((ge-employees ge-employees))

                                                            (for-each
                                                                (lambda (employee)                                                            
                                                                    (display employee)
                                                                    (newline)                                                          
                                                                )

                                                                ge-employees
                                                            )                                                
                                                        )                                                                                        
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (if (>= salary threshold)
                                                                        ;(display "Salaried employee: ")
                                                                        (set! ge-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") ge-employees))  
                                                                            
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (>= salary threshold)
                                                                        (set! ge-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words)))) ", hourly rate: " (car (cdr (cdr (cdr (cdr words))))) " \n" "earned $" (number->string salary)  ) ge-employees))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (>= salary threshold)
                                                                        (set! ge-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words)))) ", sales amount: " (car (cdr (cdr (cdr (cdr words))))) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) "%" " \n" "earned $" (number->string salary) ) ge-employees))
                                                        
                                                                    )
                                                                )
                                                            )
                                                            
                                                            
                                                            )    

                                                        )
                                                        (loop threshold ge-employees)
                                                    )
                                                    (else (loop threshold ge-employees))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )    
                                ((string=? (car (cdr args)) "eq")
                                    ;(display (car (cdr (cdr args))))        

                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (equal-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)  
                                                        (for-each
                                                                (lambda (employee)                                                            
                                                                    (display employee)
                                                                    (newline)                                                          
                                                                )

                                                                equal-employees
                                                            )                                                                                                                                         
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                        (set! equal-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") equal-employees))  
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                        
                                                                        (set! equal-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words)))) ", hourly rate: " (car (cdr (cdr (cdr (cdr words))))) " \n" "earned $" (number->string salary)  ) equal-employees))

                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                        
                                                                        (set! equal-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words)))) ", sales amount: " (car (cdr (cdr (cdr (cdr words))))) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) "%" " \n" "earned $" (number->string salary) ) equal-employees))
                                                
                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold equal-employees)
                                                    )
                                                    (else (loop threshold equal-employees))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "ne") 
                                    ;(display (car (cdr (cdr args))))        

                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (not-equal-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)  
                                                        (for-each
                                                                (lambda (employee)                                                            
                                                                    (display employee)
                                                                    (newline)                                                          
                                                                )

                                                                not-equal-employees
                                                            )                                                                                                                                                                                                                                                                         
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))                                                            
                                                                    (if (not (= salary threshold))
                                                                        (set! not-equal-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") not-equal-employees))  
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (not (= salary threshold))
                                                                        (set! not-equal-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words)))) ", hourly rate: " (car (cdr (cdr (cdr (cdr words))))) " \n" "earned $" (number->string salary)  ) not-equal-employees))

                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (not (= salary threshold))
                                                                        (set! not-equal-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words)))) ", sales amount: " (car (cdr (cdr (cdr (cdr words))))) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) "%" " \n" "earned $" (number->string salary) ) not-equal-employees))

                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold not-equal-employees)
                                                    )
                                                    (else (loop threshold not-equal-employees))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "gt") 
                                    ;(display (car (cdr (cdr args))))        

                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (gt-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)     
                                                        (for-each
                                                                (lambda (employee)                                                            
                                                                    (display employee)
                                                                    (newline)                                                          
                                                                )

                                                                gt-employees
                                                            )                                                                                                                                                                                                                                                                         
                                                                                                                                                                                    
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (if (> salary threshold)
                                                                        (set! gt-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") gt-employees))  
                                                                        
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (> salary threshold)
                                                                        (set! gt-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words)))) ", hourly rate: " (car (cdr (cdr (cdr (cdr words))))) " \n" "earned $" (number->string salary)  ) gt-employees))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (> salary threshold)
                                                                        (set! gt-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words)))) ", sales amount: " (car (cdr (cdr (cdr (cdr words))))) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) "%" " \n" "earned $" (number->string salary) ) gt-employees))
                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold gt-employees)
                                                    )
                                                    (else (loop threshold gt-employees))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "lt") 
                                    ;(display (car (cdr (cdr args))))        

                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (lt-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                                (lambda (employee)                                                            
                                                                    (display employee)
                                                                    (newline)                                                          
                                                                )

                                                                lt-employees
                                                            )                                                                                                                                    
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (if (< salary threshold)
                                                                        (set! lt-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") lt-employees))  
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (< salary threshold)
                                                                        (set! lt-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words)))) ", hourly rate: " (car (cdr (cdr (cdr (cdr words))))) " \n" "earned $" (number->string salary)  ) lt-employees))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (< salary threshold)
                                                                        (set! lt-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words)))) ", sales amount: " (car (cdr (cdr (cdr (cdr words))))) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) "%" " \n" "earned $" (number->string salary) ) lt-employees))
                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold lt-employees)
                                                    )
                                                    (else (loop threshold lt-employees))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "le") 
                                    ;(display (car (cdr (cdr args))))        

                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (le-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file) 
                                                        (for-each
                                                            (lambda (employee)                                                            
                                                                (display employee)
                                                                (newline)                                                          
                                                            )
                                                            le-employees
                                                        )                                                                                                                                   
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")                                                        
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (if (<= salary threshold)
                                                                        (set! le-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") le-employees))  
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (<= salary threshold)
                                                                        (set! le-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words)))) ", hourly rate: " (car (cdr (cdr (cdr (cdr words))))) " \n" "earned $" (number->string salary)  ) le-employees))
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (<= salary threshold)
                                                                        (set! le-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words)))) ", sales amount: " (car (cdr (cdr (cdr (cdr words))))) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) "%" " \n" "earned $" (number->string salary) ) le-employees))
                                                                    )
                                                                )
                                                            )                                                                                                
                                                            )    

                                                        )
                                                        (loop threshold le-employees)
                                                    )
                                                    (else (loop threshold le-employees))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                            )
                            
                        )
                        ((string=? action "avg")
                            (cond
                                ((null? (cdr args))
                                    (let ((file (open-input-file filename)))
                                        (let loop ((total 0) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display "Average payment per employee is $")
                                                        (display (/ total count))
                                                        (newline)
                                                        (newline)
                                                    )

                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                (set! total (+ total ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                )
                                                                ((string=? (car words) "hourly")
                                                                    (set! total (+ total ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                )
                                                                ((string=? (car words) "commission")
                                                                    (set! total (+ total ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                )
                                                        
                                                            ) 
                                                            (set! count (+ count 1))
                                                        )
                                                        (loop total count)
                                                    )
                                                    (else (loop total count))
                                                )
                                            )
                                        )
                                    )
                                )
                                ((string=? (car (cdr args)) "eq")
                                    ;prints the average of all the employees whose earned salary is equal to the threshold
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display "Average earnings of all employees equal to the threshold: $")
                                                        (display (/ total count))                                                                                                                                                                                      
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")   
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    (if (= salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (= salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )   
                                                                                                                                                        
                                                            ) 
                                                            ;(set! count (+ count 1))                                           

                                                        )
                                                        
                                                        (loop threshold total count)
                                                    )
                                                    (else (loop threshold total count))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )

                                )
                                ((string=? (car (cdr args)) "ne")
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display "Average earnings of all employees equal to the threshold: $")
                                                        (display (/ total count))                                                                                                                                                                                      
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")   
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    
                                                                    
                                                                    (if (not (= salary threshold))
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (not (= salary threshold))
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (not (= salary threshold))
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    )
                                                                )
                                                            )   
                                                                                                                                                        
                                                            ) 
                                                            ;(set! count (+ count 1))                                           

                                                        )
                                                        
                                                        (loop threshold total count)
                                                    )
                                                    (else (loop threshold total count))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "gt")
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display "Average earnings of all employees equal to the threshold: $")
                                                        (display (/ total count))                                                                                                                                                                                      
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")   
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    (if (> salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (> salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (> salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )   
                                                                                                                                                        
                                                            ) 
                                                            ;(set! count (+ count 1))                                           

                                                        )
                                                        
                                                        (loop threshold total count)
                                                    )
                                                    (else (loop threshold total count))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "ge")
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display "Average earnings of all employees equal to the threshold: $")
                                                        (display (/ total count))                                                                                                                                                                                      
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")   
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    (if (>= salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (>= salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (>= salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )   
                                                                                                                                                        
                                                            ) 
                                                            ;(set! count (+ count 1))                                           

                                                        )
                                                        
                                                        (loop threshold total count)
                                                    )
                                                    (else (loop threshold total count))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "lt")
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display "Average earnings of all employees equal to the threshold: $")
                                                        (display (/ total count))                                                                                                                                                                                      
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")   
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    (if (< salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (< salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (< salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )   
                                                                                                                                                        
                                                            ) 
                                                            ;(set! count (+ count 1))                                           

                                                        )
                                                        
                                                        (loop threshold total count)
                                                    )
                                                    (else (loop threshold total count))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                                ((string=? (car (cdr args)) "le")
                                    (let ((file (open-input-file filename)))
                                        (let loop ((threshold (car (cdr (cdr args)))) (total 0) (count 0))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (display "Average earnings of all employees equal to the threshold: $")
                                                        (display (/ total count))                                                                                                                                                                                      
                                                        (newline)                                        
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")   
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    
                                                                    (if (<= salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                    (if (<= salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                    (if (<= salary threshold)
                                                                    
                                                                        ;(set! count (+ count 1))
                                                                        ;(set! total (+ total salary)) 
                                                                        (begin
                                                                            (set! count (+ count 1))
                                                                            (set! total (+ total salary))
                                                                        )
                                                                    
                                                                    )
                                                                )
                                                            )   
                                                                                                                                                        
                                                            ) 
                                                            ;(set! count (+ count 1))                                           

                                                        )
                                                        
                                                        (loop threshold total count)
                                                    )
                                                    (else (loop threshold total count))
                                                    
                                                )

                                            )
                                        )
                                        
                                    )
                                )
                            )
                            
                        )
                        ((string=? action "min")
                            (cond
                                ((null? (cdr args))
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (min-salary 100000) (min-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            min-employees
                                                        )
                                                        (newline)
                                                
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                            
                                                                    (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                        (cond
                                                                            ((< salary min-salary)
                                                                                (begin
                                                                                    (set! min-salary salary)
                                                                                    (set! min-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))
                                                                                )
                                                                            )
                                                                            ((= salary min-salary)
                                                                                (set! min-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") min-employees))
                                                                            )
                                                                        )
                                                                
                                                                    )
                                                                )
                                                                ((string=? (car words) "hourly")
                                                                    (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                        (cond
                                                                            ((< salary min-salary)
                                                                                (begin
                                                                                    (set! min-salary salary)
                                                                                    (set! min-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ))) 

                                                                                )
                                                                                
                                                                            )
                                                                            ((= salary min-salary)
                                                                                (set! min-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) min-employees))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                ((string=? (car words) "commission")
                                                                    (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                        (cond
                                                                            ((< salary min-salary)
                                                                                (begin
                                                                                    (set! min-salary salary)
                                                                                    (set! min-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                                )
                                                                                
                                                                            )
                                                                            ((= salary min-salary)
                                                                                (set! min-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) min-employees)))
                                                                            )
                                                                
                                                                        )
                                                                    )
                                                                )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop min-salary min-employees)
                                                    )
                                                    (else (loop min-salary min-employees))                                                                            
                                                )
                                            )                                                    
                                        )
                                
                                    )

                                )
                                ((string=? (car (cdr args)) "eq")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 100000) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    

                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                         
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                                                                                
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    )

                                )
                                
                                ((string=? (car (cdr args)) "ne")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 100000) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    

                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (not (= salary threshold)))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                         
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (not (= salary threshold)))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                                                                                
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (not (= salary threshold)))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    )
                                )
                                ((string=? (car (cdr args)) "gt")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 100000) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    

                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (> salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                         
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (> salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                                                                                
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (> salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    ) 
            
                                )
                                ((string=? (car (cdr args)) "ge")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 100000) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    

                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (>= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                         
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (>= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                                                                                
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (>= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    ) 
                                    
                                )
                                ((string=? (car (cdr args)) "lt")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 100000) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    

                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (< salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                         
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (< salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                                                                                
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (< salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    ) 
                                    
                                )
                                ((string=? (car (cdr args)) "le")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 100000) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    

                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (<= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                         
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (<= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                                                                                
                                                                    (cond                                                                                                                            
                                                                        ((and (> max-salary salary) (<= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    )
                                    
                                )
                            )
                            
                        )
                        ((string=? action "max")
                            (cond
                                ((null? (cdr args))
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (max-salary -10000000) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)
                                                
                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                            
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (cond
                                                                        ((< max-salary salary)
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))
                                                                            )
                                                                        )
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        )                                                        
                                                                    )                                                                                                                
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                            
                                                                    (cond
                                                                        ((< max-salary salary)
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ))) 
                                                                            )
                                                                            
                                                                        )
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        )
                                                                
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                
                                                                    (cond
                                                                        ((< max-salary salary)
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )

                                                                        
                                                                        )
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        )
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop max-salary max-employees)
                                                    )
                                                    (else (loop max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    )                    
                                )
            
                                ((string=? (car (cdr args)) "lt")

                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 0) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    

                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (< salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                         
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (< salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                                                                                
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (< salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    )                                                    
                                )
                                ((string=? (car (cdr args)) "eq")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 0) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    

                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                         
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    )
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))                                                                                                
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    )

                                )
                                ((string=? (car (cdr args)) "gt")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 0) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (> salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                        
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                            
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (> salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    ) 
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                
                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (> salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    )
                                    
                                )
                                ((string=? (car (cdr args)) "ge")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 0) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (>= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                        
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                            
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (>= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    ) 
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                
                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (>= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    )
                                    
                                )
                                ((string=? (car (cdr args)) "ne")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 0) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (not (= salary threshold)))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                        
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                            
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary)  (not (= salary threshold)))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    ) 
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                
                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary)  (not (= salary threshold)))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    )
                                
                                    
                                )
                                ((string=? (car (cdr args)) "le")
                                    (let ((file (open-input-file filename)))
                                        (let loop ( (threshold (car (cdr (cdr args)))) (max-salary 0) (max-employees '()))
                                            (let ((line (read-line file)))
                                                (cond
                                                    ((eof-object? line)
                                                        (close-input-port file)
                                                        (for-each
                                                            (lambda (employee)
                                                                (display employee)
                                                                (newline)
                                                            )
                                                            max-employees
                                                        )
                                                        (newline)

                                                    )
                                                    ((not (string=? line ""))
                                                        (let ((words (str-split line #\space)))
                                                            (cond ((string=? (car words) "salaried")
                                                                
                                                                (let ((salary ((make-salaried-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words)))))) 'get-salary)))
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (<= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n")))                                                                    
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Salaried employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "weekly salary: " (car (cdr (cdr (cdr words)))) " \n" "earned $" (car (cdr (cdr (cdr words)))) " \n") max-employees))
                                                                        ) 
            
                                                                    )                                                                                                                                                                                                                                                                                        
                                                                )
                                                            )
                                                            ((string=? (car words) "hourly")
                                                                (let ((salary ((make-hourly-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words))))))) 'get-salary)))
                                                                                                            
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (<= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) )))                                                                                                                                     
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Hourly employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "hours worked: " (car (cdr (cdr (cdr words))) ) ", hourly rate: " (car (cdr (cdr (cdr (cdr words)))) ) " \n" "earned $" (number->string salary) ) max-employees))
                                                                        ) 
            
                                                                    ) 
                                                                )
                                                            )
                                                            ((string=? (car words) "commission")
                                                                (let ((salary ((make-commission-employee (car (cdr words)) (car (cdr (cdr words))) (string->number (car (cdr (cdr (cdr words))))) (string->number (car (cdr (cdr (cdr (cdr words)))))) (string->number (car (cdr (cdr (cdr (cdr (cdr words)))))))) 'get-salary)))
                                                                
                                                                    
                                                                    (cond                                                                                                                            
                                                                        ((and (< max-salary salary) (<= salary threshold))
                                                                            (begin
                                                                                (set! max-salary salary)
                                                                                (set! max-employees (list (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))))  ", sales amount: " (car (cdr (cdr (cdr (cdr words)))))  ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))))) " \n" "earned $" (number->string salary)  )))
                                                                            )
                                                                        )
                                                            
                                                                        ((= salary max-salary)
                                                                            (set! max-employees (cons (string-append " \n" "Commission employee: " (car (cdr words)) " " (car (cdr (cdr words))) " \n" "minimum salary: " (car (cdr (cdr (cdr words))) ) ", sales amount: " (car (cdr (cdr (cdr (cdr words)))) ) ", commission rate: " (car (cdr (cdr (cdr (cdr (cdr words)))) ) "%" " \n" "earned $" (number->string salary) ) max-employees)))
                                                                        ) 
            
                                                                    )
                                                                                                                                                                        
                                                                )
                                                            )                                                                                                
                                                            ) 
                                                        )                                                                                    
                                                        (loop threshold max-salary max-employees)
                                                    )
                                                    (else (loop threshold max-salary max-employees))                                                                            
                                                )
                                            )                                                    
                                        )                    
                                    )
                                    
                                )
                            )                    
                        )
                    )
                )
            )
        )
        (begin
            (if (null? args)
                (begin
                    (display "Unable to open ")
                    (display filename)
                    (display ".dat")
                    (newline)
                    (newline)
                )
                (begin
                    (display "Unable to open ")
                    (display filename)
                    (display ".dat")
                    (newline)
                    (newline)
                
                )
            )
        )
    )

    
)

