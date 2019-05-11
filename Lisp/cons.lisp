;
; cons または . でコンスを作成できる
;
(print (cons 1 2)) ; => (1 . 2)
(print '(1 . 2))   ; => (1 . 2)

;
; コンスリスト
;
(print '(1 . (2 . nil)))       ; => (1 2)
(print '(1 . (2 . (3 . nil)))) ; => (1 2 3)

;
; car は先頭を取る
;
(print (car '(1 2 3 4))) ; => 1

;
; cdr は先頭以外を取る
;
(print (cdr '(1 2 3 4))) ; => (2 3 4)

;
; /c[ad]+r/ な変種も用意されている
;
(print "/c[ad]+r/")
(print (cadr '(1 2 3 4)))     ; => 2
(print (cddr '(1 2 3 4)))     ; => (3 4)
(print (caar '((1 2) (3 4)))) ; => 1

;
; first, second ... といったようにインデックスに対応する関数もある
;
(print (first  '(1 2 3 4)))
(print (second '(1 2 3 4)))
(print (third  '(1 2 3 4)))

;
; append はリストの連結処理（nconc はその破壊的バージョン）
;
(setq xs '(1 2))
(setq ys '(3 4))

; append
(print (append xs ys))  ; => (1 2 3 4)
(print xs)              ; => (1 2)
(print ys)              ; => (3 4)

; nconc
(print (nconc xs ys))   ; => (1 2 3 4)
(print xs)              ; => (1 2 3 4)　引数に渡したものは壊れる
(print ys)              ; => (3 4)

;
; find-if 条件に当てはまる最初の要素を返す
;
(print "find-if:")
(print
    (find-if 'oddp '(2 3 4 5))) ; => 3

;
; mapcar は普通の map
;
(print (mapcar (lambda (x)
    (+ x 1)) '(1 2 3 4)))
; => (2 3 4 5)

;
; mapcan は関数が返したリストを nconc で連結して返す
;
(print (mapcan (lambda (x)
    (list (identity x) (+ x 1))) '(1 2 3 4)))
; => (1 2 2 3 3 4 4 5)
