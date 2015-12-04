;;;
;;; Example program from Figure 3 in Clarke's "Ownership Types For Flexible Alias Protection"
;;;

;;;;;;;;;;;;;;;
;;; CLASSES ;;;
;;;;;;;;;;;;;;;
(
  ( class Link (n)
          ; fields
          (
              (next (Link owner (n)))
              (data (X n ()))
          )
          ; methods
          (
              (constructor Unit
                           ( (inData (X n ())) )
                           ()
                           ( seq
                               (= (this next) null)
                               (= (this data) inData)
                           )
              )
          )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCAL VARS FOR MAIN ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
()

;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN EXPRESSION ;;;
;;;;;;;;;;;;;;;;;;;;;;;
;(invoc (new (Main norep ())) main ())
end