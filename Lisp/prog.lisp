;
; prog1 は一連の式を評価し、最後に式1の結果を返す
;
(print
    (prog1 (print "a")
           (print "b")
           (print "c")))
; =>
; "a"
; "b"
; "c"
; "a"

;
; prog2 は式2の結果を返す
;
(print
    (prog2 (print "a")
           (print "b")
           (print "c")))
; =>
; "a"
; "b"
; "c"
; "b"