(define content-header "Content-type: text/xml\r\n\r\n")
(define xml-header "<?xml version =\"1.0\" encoding=\"utf-8\"?>")

(define epsilon "")

(define (search-columns column-names query)
  (lset-intersection string-ci=?
                     (map (compose symbol->string car) query)
                     column-names))

(define (search-values search-columns query)
  (map (lambda (search-column)
         (format "%~A%" (alist-ref (string->symbol search-column) query)))
       search-columns))

(define where
  (case-lambda
   ((search-columns)
    (where search-columns "LIKE"))
   ((search-columns operator)
    (where search-columns operator "WHERE" "AND"))
   ((search-columns operator prefix conjunction)
    (if (null? search-columns)
      epsilon
      (string-join
       (list
        prefix
        (string-join
         (map (lambda (search-column)
                (format "~A ~A :~A" search-column operator search-column))
              search-columns)
         (format " ~A " conjunction))))))))

(define (set set-columns)
  (where set-columns "=" "SET" ","))

(define (collate sort-order)
  (if (member sort-order '("ASC" "DESC") string-ci=?)
      (string-upcase sort-order)
      epsilon))

(define (order-by sort-index sort-order column-names)
  (if sort-index
      (if (member sort-index column-names string-ci=?)
          (let ((collate (collate sort-order)))
            (format "ORDER BY ~A ~A"
                    sort-index
                    collate))
          epsilon)
      epsilon))

(define (read-grid-from-table connection table)
  (let ((select
         "SELECT * FROM ~A ~A ~A LIMIT ? OFFSET ?;")
        (count-rows
         "SELECT COUNT(*) FROM ~A ~A;"))
    (fcgi-dynamic-server-accept-loop
     (lambda (in out err env)
       (let ((query (form-urldecode (env "QUERY_STRING"))))
         (let* ((column-names (column-names connection table))
                (search-columns (search-columns column-names query))
                (where (where search-columns)))
           (let ((rows (string->number (alist-ref 'rows query)))
                 (page (string->number (alist-ref 'page query)))
                 (sort-index (alist-ref 'sidx query))
                 (sort-order (alist-ref 'sord query)))
             (let ((order-by (order-by sort-index sort-order column-names))
                   (search-values (search-values search-columns query)))
               (sqlite3:call-with-temporary-statements
                (lambda (select count-rows)
                  (let* ((total-rows
                          (apply sqlite3:first-result (cons count-rows search-values)))
                         (total-pages
                          (+ (inexact->exact
                              (floor (/ (- total-rows 1)
                                        rows)))
                             1)))
                    (let ((results
                           (apply sqlite3:map-row
                                  (append
                                   (list
                                    (lambda x
                                      `(row (@ (id ,(->string (car x))))
                                            ,(map
                                              (lambda (result)
                                                `(cell ,(->string result)))
                                              x)))
                                    select)
                                   search-values
                                   (list
                                    rows
                                    (* rows (- page 1)))))))
                      (out content-header)
                      (out xml-header)
                      (out (shtml->html `(rows
                                          (page ,(->string page))
                                          (total ,(->string total-pages))
                                          (records ,(->string total-rows))
                                          ,results))))))
                connection
                (format select table where order-by)
                (format count-rows table where))))))))))

(define (set-columns column-names query)
  (lset-difference string-ci=?
                   (search-columns column-names query)
                   '("id")))

(define (set-values set-columns query)
  (map (lambda (set-column)
         (alist-ref (string->symbol set-column) query))
       set-columns))

;;; jqGrid requires 2xx or 3xx, apparently; we should really be able
;;; to set this from the client: and at least give the impression of
;;; orthogonality.
(define default-error-code 400)

(define jqgrid-error-code 399)

(define default-error-status "Bad request")

(define error-header
  (case-lambda
   (()
    (error-header default-error-status
                  default-error-code))
   ((error-status)
    (error-header error-status
                  default-error-code))
   ((error-status error-code)
    (format "Status: ~A ~A\r\n" error-code error-status))))

(define error-header/exn
  (case-lambda
   ((exn)
    (error-header/exn exn default-error-code))
   ((exn error-code)
    (error-header ((condition-property-accessor 'exn 'message) exn)
                  error-code))))

;;; may need poly-ary set-clause for add; also: respect oper?
(define (write-grid-to-table connection
                             table
                             modify-query
                             update?
                             insert?
                             delete?)
  (let ((update "UPDATE ~A ~A ~A;")
        (insert "INSERT INTO ~A (~A) VALUES(~A);")
        (delete "DELETE FROM ~A WHERE id = ?;"))
    (fcgi-dynamic-server-accept-loop
     (lambda (in out err env)
       (let* ((query (modify-query (form-urldecode (fcgi-get-post-data in env))
                                   env))
              (operation (alist-ref 'oper query)))
         (let* ((column-names (column-names connection table))
                (set-columns (set-columns column-names query)))
           (cond ((string-ci=? operation "edit")
                  (let ((where (where '("id") "="))
                        (set (set set-columns)))
                    (let ((set-values (set-values set-columns query))
                          (id (set-values '("id") query))
                          (update (format update table set where)))
                      (let ((data (map cons set-columns set-values)))
                        (condition-case
                         ;; assuming the first id for simplicity
                         (if (update? connection (car id) data env)
                             (sqlite3:call-with-temporary-statements
                              (lambda (update)
                                (apply sqlite3:execute
                                       (append (list update) set-values id)))
                              connection
                              update))
                         (exn (exn)
                              (out (error-header/exn exn jqgrid-error-code))))))))
                 ((string-ci=? operation "add")
                  (let ((columns (string-join set-columns ", "))
                        (values (string-join
                                 (map (lambda (set-column)
                                        (format ":~A" set-column))
                                      set-columns)
                                 ", ")))
                    (let* ((set-values (set-values set-columns query))
                           (data (map cons set-columns set-values)))
                      (condition-case
                       (if (insert? connection data env)
                           (let ((insert (format insert table columns values))) 
                             (sqlite3:call-with-temporary-statements
                              (lambda (insert)
                                (apply sqlite3:execute (cons insert set-values)))
                              connection
                              insert)))
                       (exn (exn)
                            (out (error-header/exn exn jqgrid-error-code)))))))
                 ((string-ci=? operation "del")
                  (let* ((id (set-values '("id") query))
                         (delete (format delete table))) 
                    (condition-case
                     ;; assuming the first id for simplicity
                     (if (delete? connection (car id) env)
                         (sqlite3:call-with-temporary-statements
                          (lambda (delete)
                            (apply sqlite3:execute (cons delete id)))
                          connection
                          delete))
                     (exn (exn)
                          (out (error-header/exn exn jqgrid-error-code)))))))))
       (out content-header)
       (out xml-header)
       (out (shtml->html '(ok)))))))
