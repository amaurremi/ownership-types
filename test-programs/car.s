;;;
;;; Example program from Figure 1 in Clarke's "Ownership Types For Flexible Alias Protection"
;;;

;;;;;;;;;;;;;;;
;;; CLASSES ;;;
;;;;;;;;;;;;;;;
(
  ( class Engine
          () ; context parameters
          () ; fields
          (  ;;; METHODS ;;;
             (             ; method #1
                 start     ; name
                 Unit      ; return type
                 ()        ; parameters
                 ()        ; local vars
                 end       ; body
             )
             (             ; method #2
                 stop      ; name
                 Unit      ; return type
                 ()        ; parameters
                 ()        ; local vars
                 end       ; body
             )
          )
  )

  ( class Driver () () () )

  ( class Car
          ; context parameters
          ()
          ; fields
          (
              (                     ; field #1
                  engine            ; field name
                  (Engine rep ())   ; field type
              )
              (                     ; field #2
                  driver            ; field name
                  (Driver norep ()) ; field type
              )
          )
          ; methods
          (
             ; todo constructors??
              (
                  getEngine              ; method name
                  (Engine rep ())        ; return type
                  ()                     ; parameters
                  ()                     ; local vars
                  (this engine)          ; return `engine` field
              )

              (
                  setEngine               ; method name
                  Unit                    ; return type
                  (
                      (e (Engine rep ())) ; parameter
                  )
                  ()                      ; local vars
                  (= (this engine) e)     ; method body: this.engine = e
              )

              (
                  go
                  Unit
                  ; todo if expressions
                  ()
                  ()
                  (invoc (this engine) start ())
              )
          )

  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCAL VARS FOR MAIN ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(
      (bob (Driver norep ()))
      (car (Car norep ()))
      (e (Engine rep ()))
)

;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN EXPRESSION ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(
    seq                                     ; sequence of expressions
        (= bob (new (Driver norep ())))     ; bob = new norep Driver
        (= car (new (Car norep ())))        ; car = new norep Car
        (= (car driver) bob)                ; car.driver = bob
        (invoc car go ())                   ; car.go()
        ; the following line should fail if uncommented:
        ; (invoc (car engine) stop ())      ; car.engine.stop()
        (= e (new (Engine rep ())))         ; e = new rep Engine
        ; the following line should fail if uncommented:
        ; (invoc car setEngine (e))         ; car.setEngine(e)
)