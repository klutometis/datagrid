(define template
  '#((grid . #((colModel . (%data column-model))
               (pager . (%data pager))
               (height . "auto")
               (caption . (%data caption))
               (url . (%data url))
               (editurl . (%data edit-url))
               (mtype: . "GET")
               (datatype . "xml")
               (sortname . (%data sort-name))
               (sortorder . (%data sort-order))))))

(define type-parsers
  `(("BOOLEAN" . ,(lambda (metadata) '((formatter . "checkbox")
                                       (edittype . "checkbox"))))))

(define (type-parser type)
  (alist-ref type type-parsers string-ci=? (lambda x '())))

(define (parse-name connection name type metadatum)
  (list (cons "name" name)))

(define (parse-editable connection name type metadatum)
  (list (cons "editable" (and (not (metadatum-primary-key metadatum))
                              (metadatum-editable metadatum)))))

(define (parse-type connection name type metadatum)
  (cond ((metadatum-foreign-key metadatum)
         (parse-foreign-key connection (metadatum-foreign-key metadatum)))
        ((null? (metadatum-enumeration metadatum))
         ((type-parser type) metadatum))
        ((metadatum-default metadatum)
         (parse-default metadatum))
        (else
         (parse-enumeration
          (metadatum-enumeration metadatum)))))

(define (parse-default metadatum)
  `(("editoptions" . #(("value" . ,(metadatum-default metadatum))))))

(define (parse-foreign-key connection foreign-key)
  (parse-enumeration
   (map number->string
        (sqlite3:map-row values
                         connection
                         (format "SELECT ~a FROM ~a;"
                                 (foreign-key-column foreign-key)
                                 (foreign-key-table foreign-key))))))

(define (parse-enumeration enumeration)
  `(("edittype" . "select")
    ("editoptions" . #(("value"
                        . #(,@(map (lambda (value) (cons value value))
                                   enumeration)))))))

(define column-parsers
  (list parse-name
        parse-editable
        parse-type))

(define (parse-column connection name type metadatum)
  `#(,@(append-map (lambda (parser) (parser connection name type metadatum))
                      column-parsers)))

(define (metadata-ref column table database)
  (let ((table-metadata (alist-ref table
                                   (database-metadata database)
                                   string-ci=?
                                   '())))
    (alist-ref column
               table-metadata
               string-ci=?
               (make-metadatum))))

(define (table->json database
                     table
                     caption-format
                     pager
                     url
                     edit-url
                     sort-name
                     sort-order)
  (call-with-connection
   (database-file database)
   (lambda (connection)
     (substitute-tokens
      template
       `((column-model . ,(map (lambda (column-type)
                                 (let ((column (car column-type))
                                       (type (cdr column-type)))
                                   (let ((metadatum (metadata-ref column table database)))
                                     (parse-column connection column type metadatum))))
                               (column-name-types connection table)))
         (caption . ,(format caption-format table))
         (pager . ,pager)
         (url . ,url)
         (edit-url . ,edit-url)
         (sort-name . ,sort-name)
         (sort-order . ,sort-order))))))

(define configure-grid
  (case-lambda
   ((database table url edit-url)
    (configure-grid database table "~a" "#pager" url edit-url "id" "desc"))
   ((database table caption-format pager url edit-url sort-name sort-order)
      (fcgi-dynamic-server-accept-loop
       (lambda (in out err env)
         (out "Content-type: application/json\r\n\r\n")
         (out (with-output-to-string
                (lambda ()
                  (json-write (table->json database
                                           table
                                           caption-format
                                           pager
                                           url
                                           edit-url
                                           sort-name
                                           sort-order))))))))))
