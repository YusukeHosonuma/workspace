;
; member
; リストから要素を探し、見つかったものから末尾を返す。なければnil。
;
(print (member 1 '(1 2 3 4))) ; => (1 2 3 4)
(print (member 2 '(1 2 3 4))) ; => (2 3 4)
(print (member 5 '(1 2 3 4))) ; => NIL


