; Define a procedure to read and print the contents of a file


(define (print-file filename) ;test file, use (print-file "employees.dat" in test.scm)
  (with-input-from-file filename
    (lambda ()
      (let loop ((line (read-line)))
        (unless (eof-object? line)
          (display line)
          (newline)
          (loop (read-line)))))))
          


(define (perform filename . args)
  (if (null? args)
      (begin
        (display "Usage: (perform employee_file action)\n")
        (display "or\n")
        (display "Usage: (perform employee_file action operator threshold)\n")
        (display "Valid actions: count print min max total avg\n")
        (display "Valid operators: eq ne gt ge lt\n")
        (newline))
        (if (string=? (car args) "count")
          (let ((file (open-input-file filename))) ; Open the file for reading
            (let loop ((count 0)
                       (line (read-line file))) ; Read each line of the file
              (if (eof-object? line) ; Check if end of file is reached
                  (begin
                    (close-input-port file) ; Close the file
                    (display count) ; Display the count
                    (newline))
                    (loop (if (string=? line "") count (+ count 1)) (read-line file))))))
  ))




"
(define (calculate-hourly-earnings hours rate)
    (cond ((<= hours 40) (* hours rate))
        ((<= hours 50) (+ (* 40 rate) (* (- hours 40) rate 1.5)))
        (else (+ (* 40 rate) (* 10 rate 1.5) (* (- hours 50) rate 2)))))

(define (calculate-commission-earnings min-salary sales commission-rate)
    (max min-salary (* sales commission-rate)))
"
