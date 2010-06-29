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

(define (read-grid-from-table db table)
  (let ((select
         "SELECT * FROM ~A ~A ~A LIMIT ? OFFSET ?;")
        (count-rows
         "SELECT COUNT(*) FROM ~A ~A;"))
    (fcgi-dynamic-server-accept-loop
     (lambda (in out err env)
       (out content-header)
       (out xml-header)
       (call-with-connection
        db
        (lambda (db)
          (let ((query (form-urldecode (env "QUERY_STRING"))))
            (let* ((column-names (column-names db table))
                   (search-columns (search-columns column-names query))
                   (where (where search-columns)))
              (let ((rows (string->number (alist-ref 'rows query)))
                    (page (string->number (alist-ref 'page query)))
                    (sort-index (alist-ref 'sidx query))
                    (sort-order (alist-ref 'sord query)))
                (let ((order-by (order-by sort-index sort-order column-names))
                      (search-values (search-values search-columns query)))
                  (let ((select
                         (sqlite3:prepare db (format select table where order-by)))
                        (count-rows
                         (sqlite3:prepare db (format count-rows table where))))
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
                        (out (shtml->html `(rows
                                            (page ,(->string page))
                                            (total ,(->string total-pages))
                                            (records ,(->string total-rows))
                                            ,results))))))))))))))))

(define (set-columns column-names query)
  (lset-difference string-ci=?
                   (search-columns column-names query)
                   '("id")))

(define (set-values set-columns query)
  (map (lambda (set-column)
         (alist-ref (string->symbol set-column) query))
       set-columns))

;;; may need poly-ary set-clause for add; also: respect oper?
(define (write-grid-to-table db table)
  (let ((update "UPDATE ~A ~A ~A;")
        (insert "INSERT INTO ~A (~A) VALUES(~A);")
        (delete "DELETE FROM ~A WHERE id = ?;"))
    (fcgi-dynamic-server-accept-loop
     (lambda (in out err env)
       (out content-header)
       (out xml-header)
       (let* ((query (form-urldecode (fcgi-get-post-data in env)))
              (operation (alist-ref 'oper query)))
         (call-with-connection
          db
          (lambda (db)
            (let* ((column-names (column-names db table))
                   (set-columns (set-columns column-names query)))
              (cond ((string-ci=? operation "edit")
                     (let ((where (where '("id") "="))
                           (set (set set-columns)))
                       (let ((set-values (set-values set-columns query))
                             (id (set-values '("id") query))
                             (update (sqlite3:prepare db (format update table set where))))
                         (apply sqlite3:execute (append (list update) set-values id)))))
                    ((string-ci=? operation "add")
                     (let ((columns (string-join set-columns ", "))
                           (values (string-join
                                    (map (lambda (set-column)
                                           (format ":~A" set-column))
                                         set-columns)
                                    ", ")))
                       (let ((insert (sqlite3:prepare db (format insert table columns values)))) 
                         (apply sqlite3:execute (cons insert (set-values set-columns query))))))
                    ((string-ci=? operation "del")
                     (let* ((id (set-values '("id") query))
                            (sqlite3:delete (sqlite3:prepare db (format delete table)))) 
                       (apply sqlite3:execute (cons delete id)))))))))
       (out (shtml->html '(ok)))))))
