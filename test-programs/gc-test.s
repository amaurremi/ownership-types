;;;
;;; Program for GC testing
;;;

; CLASSES
(
    ( class Gc ()
        ; fields
        (
            (fieldGc (Gc rep ()))
            (fieldNotGc (NotGc norep ()))
        )
        ; methods
        (
            ( constructor Unit () ()
                ( seq
                    ; ref address 3
                    (= (this fieldGc) (new (Gc rep ())))           ; should be gc'ed because it's a single assignment to a rep field
                    ; ref address 4
                    (= (this fieldNotGc) (new (NotGc norep ())))   ; shouldn't be gc'ed because it's an assignment to a norep field
                )
            )
        )
    )
    ( class NotGc () () () )
    ( class X () ()
            ; methods
            (
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
                           ; ref address 2
                           (= localGc (new (Gc norep ())))             ; should be gc'ed after return from foo because it's a single assignment to a local var
                           (invoc localGc constructor ())
                           ; ref address 5
                           (= localNotGc1 (new (NotGc norep ())))
                           (= localNotGc2 localNotGc1)                 ; shouldn't be gc'ed because the object is being assigned twice
                        )
                )
            )
    )
)

; LOCAL VARS FOR MAIN
((x (X norep ())))

; MAIN EXPRESSION
(seq
    ; ref address 1
    (= x (new (X norep ())))
    (invoc x foo ())
)