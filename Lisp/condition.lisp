;
; when は条件が true だった場合のみ式を実行する
;
(when T   (print "True"))  ; => True
(when nil (print "False"))

;
; cond は最初に条件に当てはまったものを返す
;
(print
    (let ((x 2))
        (cond
            ((eq x 1) "A")
            ((eq x 2) "B")
            ((eq x 3) "C")
            (T        "-"))))
; => "B"
