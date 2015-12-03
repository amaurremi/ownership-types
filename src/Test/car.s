( ; classes
  ( class Car
          () ; context parameters
          () ; fields
          () ; methods
  )
  ( class Engine
          ()
          ()
          ()
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
    seq
    (= engine (new (Engine norep ()))) ; engine = new Engine<norep>
    (invoc engine start ()) ; engine.start()
)