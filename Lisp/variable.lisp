
;
; setq で変数への代入
;
(setq xs '(1 2 3 4))
(print xs) ; => (1 2 3 4)

;
; setf は事前に第1引数を評価し、それに対して代入する
;
(setf (car xs) 'a)
(print xs) ; => (A 2 3 4)

;
; rplaca はリストの car 部を置き換える
;
(setq xs '(1 2 3 4))
(rplaca xs 'a)
(print xs) ; => (A 2 3 4)

;
; rplacd はリストの cdr 部を置き換える
;
(setq xs '(1 2 3 4))
(rplacd xs 'a)
(print xs) ; => (1 . A)

;
; let (変数 値) で局所変数を定義できる
;
(print
    (let ((x 1) (y 2))
        (+ x y)))
; => 3

;
; let で変数宣言のみでも大丈夫
;
(print
    (let ((x) (y 2))
        (setq x 1)
        (+ x y)))
; => 3

(defmacro mac (expr)
    `(pprint (macroexpand-1 ',expr)))

(mac (let ((x 1) (y 2))
        (+ x y)))