(define template
  '#((grid . #((colModel . (%data column-model))))))

(define type-parsers
  '(("BOOLEAN" . (lambda (metadata) '((formatter . "checkbox")
                                      (edittype . "checkbox"))))))

(define (type-parser type)
  (alist-ref type type-parsers string-ci=? (lambda x '())))

(define (parse-name name type metadatum)
  (list (cons "name" name)))

(define (parse-index name type metadatum)
  (list (cons "index" (or (metadatum-index metadatum) name))))

(define (parse-type name type metadatum)
  (if (null? (metadatum-enumeration metadatum))
      ((type-parser type) metadatum)
      (parse-enumeration metadatum)))

(define (parse-enumeration metadatum)
  `(("edittype" . "select")
    ("editoptions" . #(("value"
                        . #(,@(map (lambda (value) (cons value value))
                                   (metadatum-enumeration metadatum))))))))

(define column-parsers
  (list parse-name
        parse-index
        parse-type))

(define (parse-column name type metadatum)
  `#(,@(append-map (lambda (parser) (parser name type metadatum))
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

(define (table->json database table)
  (call-with-connection
   (database-file database)
   (lambda (connection)
     (substitute-tokens
      template
       `((column-model . ,(map (lambda (column-type)
                                 (let ((column (car column-type))
                                       (type (cdr column-type)))
                                   (let ((metadatum (metadata-ref column table database)))
                                     (parse-column column type metadatum))))
                               (column-name-types connection table))))))))

(define (configure-grid database table)
  (fcgi-dynamic-server-accept-loop
   (lambda (in out err env)
     (out "Content-type: application/json\r\n\r\n")
     (out (with-output-to-string
            (lambda ()
              (json-write (table->json database table))))))))
