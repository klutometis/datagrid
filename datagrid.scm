(module
 datagrid

 (read-grid-from-table
  write-grid-to-table
  configure-grid
  make-database
  make-metadatum
  make-foreign-key
  table->json)

 (import scheme
         chicken
         data-structures
         extras
         posix
         ports)

 (use fastcgi
      uri-common
      htmlprag
      foof-loop
      srfi-1
      srfi-13
      defstruct
      json
      token-substitution
      call-with-sqlite3-connection
      debug)

 (require-library sqlite3)              ; thanks, sjamaan
 (import (prefix sqlite3 sqlite3:))

 (include "database.scm")
 (include "ajax.scm")
 (include "configure.scm"))
