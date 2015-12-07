;;;
;;; Program for GC testing
;;;

; CLASSES
(
    ( class Gc () () () )
    ( class NotGc () () () )
    ( class X ()
            ; fields
            (
                (fieldGc (Gc rep ()))
                (fieldNotGc (NotGc norep ()))
            )
            ; methods
            (
                (
                    constructor Unit () ()
                        ( seq
                            (= (this fieldGc) (new (Gc rep ())))           ; should be gc'ed because it's a single assignment to a rep field
                            (= (this fieldNotGc) (new (NotGc norep ())))   ; shouldn't be gc'ed because it's an assignment to a norep field
                        )
                )
                (
                    foo Unit ()
                        ; local vars
                        (
                            (localGc (Gc norep ()))
                            (localNotGc1 (NotGc norep ()))
                            (localNotGc2 (NotGc norep ()))
                        )
                        ; expression
                        ( seq
                           (= localGc (new (Gc norep ())))              ; should be gc'ed after return from foo because it's a single assignment to a local var
                           (= localNotGc1 (new (NotGc norep ())))
                           (= localNotGc2 localNotGc1)                 ; shouldn't be gc'ed because the object is being assigned twice
                        )
                )
            )
    )
)

; LOCAL VARS FOR MAIN
()

; MAIN EXPRESSION
(invoc (new (X norep ())) foo ())