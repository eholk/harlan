(library (harlan middle remove-transactions)
  (export remove-transactions)
  (import (rnrs)
          (nanopass)
          (only (elegant-weapons compat) make-parameter parameterize)
          (only (elegant-weapons helpers) gensym)
          (only (harlan middle languages M7)
                M7.0.2 M7.0.3
                unparse-M7.0.2 unparse-M7.0.3))

  ;; The list of regions in the current transaction
  (define tx-regions   (make-parameter '()))
  ;; The list of variable names for each saved allocation pointer
  (define tx-snapshots (make-parameter '()))
  ;; The label name that marks the begining of the transaction
  (define tx-start     (make-parameter #f))
  ;; The label name that marks the code where transaction rollback happens
  (define tx-rollback  (make-parameter #f))
  ;; The label name that you jump to when the transaction completes
  (define tx-commit    (make-parameter #f))
  
  (define-pass remove-transactions : M7.0.2 (m) -> M7.0.3 ()
    (Stmt
     : Stmt (stmt) -> Stmt ()

     ((retry-transaction)
      (if (tx-rollback)
          `(goto ,(tx-rollback))
          `(error non-existent-retry)))
     
     ((transaction (,r ...) ,stmt)
      (parameterize ((tx-regions r)
                     (tx-snapshots (map gensym r))
                     (tx-start (gensym 'txn_start))
                     (tx-rollback (gensym 'txn_rollback))
                     (tx-commit (gensym 'txn_commit)))
        (let ((stmt (Stmt stmt)))
          `(begin (label ,(tx-start))
                  (let (,(map (lambda (rx)
                                `(,rx region_ptr))
                              (tx-snapshots)) ...)
                    (begin
                      ,(map (lambda (rx r)
                               `(set! (var region_ptr ,rx)
                                      (call (c-expr
                                             (fn ((ptr region)) -> region_ptr)
                                             get_alloc_ptr)
                                            (var (ptr region) ,r))))
                            (tx-snapshots)
                            (tx-regions)) ...
                      ,stmt
                      (goto ,(tx-commit))
                      (label ,(tx-rollback))
                      ,(map (lambda (rx r)
                               `(begin
                                  (print (str "Replaying transaction\n"))
                                  (do (call (c-expr
                                             (fn ((ptr (ptr region)) int) -> void)
                                             reserve_at_least)
                                            (addressof (var (ptr region) ,r))
                                            (call (c-expr
                                                   (fn ((ptr region)) -> region_ptr)
                                                   get_alloc_ptr)
                                                  (var (ptr region) ,r))))
                                  (do (call (c-expr
                                             (fn ((ptr region) region_ptr) -> void)
                                             set_alloc_ptr)
                                            (var (ptr region) ,r)
                                            (var region_ptr ,rx)))))
                             (tx-snapshots)
                             (tx-regions)) ...
                             (label ,(tx-commit)))))))))))
