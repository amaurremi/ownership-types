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

  ( class XStack (m)
          ; fields
          (
              (top (Link rep (m)))
          )
          ;methods
          (
              (XStack Unit () () (= (this top) null))

              (push Unit ((data (X m ())))
                    (
                        (newTop (Link rep (m)))
                        (dataLink (Link rep (m)))
                    )
                    ( seq
                        ; dataLink = new Link(data)
                        (= dataLink (new (Link rep (m))))
                        (invoc dataLink constructor (data))

                        (= newTop dataLink)
                        (= (newTop next) (this top))
                        (= (this top) newTop)
                    )
              )

              (
                  pop (X m ()) ()
                      (
                          (oldTop (Link rep (m)))
                          (top (Link rep (m)))
                      )
                      ( seq
                          (= oldTop (this top))
                          (= top (oldTop next))
                          ((this top) data)
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