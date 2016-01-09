(define-library (scheme process-context)
  (export
   exit)

  (begin
    (--define-native exit process_exit)
  ))
