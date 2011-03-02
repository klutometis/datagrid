(defstruct database file (metadata #f))

;;; index name, too? such that `ercc1_status' becomes `ERCC1 status',
;;; etc.
(defstruct metadatum
  (primary-key #f)
  (editable #t)
  (enumeration '())
  (foreign-key #f)
  (index #f)
  (default #f))

;;; we can obey this constraint in the ui by enumerating the column;
;;; yet another metadatum: allow null?
(defstruct foreign-key table column)

(define (column-name-types db table)
  (let* ((select
          (sqlite3:prepare db (format "SELECT * FROM ~A LIMIT 1;" table)))
         (columns (sqlite3:column-count select)))
    (loop ((for i (up-from 0 (to columns)))
           (for name-types
                (listing
                 (cons (sqlite3:column-name select i)
                       (sqlite3:column-declared-type select i)))))
          => name-types)))

(define (column-names db table)
  (map car (column-name-types db table)))
