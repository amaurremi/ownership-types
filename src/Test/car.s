( ; classes
  ( class Car
          () ; context parameters
          () ; fields
          () ; methods
  )
  ( class Engine
          () ; context parameters
          () ; fields
          (  ; methods
              (
                  start     ; name
                  Unit      ; return type
                  ()        ; args
                  ()        ; local vars
                  end       ; expression
              )
          )
  )
)

( ; locals
      (
          engine              ; name
          (Engine norep ())   ; type
      )
)

; main
(
    seq                                     ; sequence of expressions
        (= engine (new (Engine norep ())))  ; engine = new Engine<norep>
        (invoc engine start ())             ; engine.start()
)