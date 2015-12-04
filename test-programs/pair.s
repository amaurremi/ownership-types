;;;
;;; Example program from Figure 2 in Clarke's "Ownership Types For Flexible Alias Protection"
;;;

;;;;;;;;;;;;;;;
;;; CLASSES ;;;
;;;;;;;;;;;;;;;
(
  ( class X () () () )

  ( class Y () () () )

  ( class Pair
          ; context parameters
          (m n)
          ; fields
          (
                (fst (X m ()))
                (snd (Y n ()))
          )
          ; methods
          ()
  )

  ( class Intermediate ()
          ; fields
          (
              (pair1 (Pair rep (rep norep)))
              (pair2 (Pair norep (rep norep)))
          )
          ; methods
          (
              ( a (Pair rep (rep norep))    () () (this pair1) )
              ( b (Pair norep (rep norep )) () () (this pair2) )
              ( x (X rep ())                () () ((this pair1) fst) )
              ( y (Y norep ())              () () ((this pair1) snd) )
              ( updateX Unit                () () (= ((this pair1) fst) (new (X rep ()))) )
          )
  )

  ( class Main ()
          ; fields
          ( (safe (Intermediate norep ())) )
          ; methods
          (
              ( main Unit ()
                     ; local vars
                     (
                         (a (Pair rep (rep norep)))
                         (b (Pair norep (rep norep)))
                         (x (X rep ()))
                         (y (Y norep ()))
                     )
                     ; body
                     (
                         seq
                         ; the following three lines should fail if uncommented
                         ;    (= a (invoc safe a ()))
                         ;    (= b (invoc safe b ()))
                         ;    (= x (invoc safe x ()))
                             (= y (invoc (this safe) y ()))

                         (invoc (this safe) updateX ())
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
(invoc (new (Main norep ())) main ())