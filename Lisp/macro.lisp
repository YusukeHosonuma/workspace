;
; マクロの定義
;
(defmacro memq (obj lst)
    `(member ,obj ,lst :test #'eq))

;
; マクロの利用
;
(print (memq 'a '(a b c))) ; => (A B C)
(print (memq 'b '(a b c))) ; => (B C)

;
; マクロの展開
;
(pprint (macroexpand '(memq 'a '(a b c))))

;
; マクロの定義（任意の引数）
;
(defmacro while (test &body body)
    `(do ()
         ((not ,test))
       ,@body))

;
; macroexpand は再帰的にすべてのマクロを展開する
;
(pprint (macroexpand '(while (able) (laugh))))
; => (BLOCK NIL (LET NIL (TAGBODY #:LOOP-2814 (IF (NOT (ABLE)) (GO #:END-2815)) (LAUGH) (PSETQ) (GO #:LOOP-2814) #:END-2815 (RETURN-FROM NIL (PROGN)))))

;
; macroexpand-1 は1段階だけマクロを展開する
;
(pprint (macroexpand-1 '(while (able) (laugh))))
; => (DO NIL ((NOT (ABLE))) (LAUGH))

;
; macroexpand-1 が長いのでエイリアス的なマクロを用意
;
(defmacro mac (expr)
    `(pprint (macroexpand-1 ',expr)))

(mac (or x y)) ; => (COND (X) (T Y))

;
; let の独自実装
;
(defmacro our-let (binds &body body)
    `((lambda ,(mapcar #'(lambda (x)
                            (if (consp x) (car x) x))
                       binds)
        ,@body)
      ,@(mapcar #'(lambda (x)
                     (if (consp x) (cadr x) nil))
                binds)))

(print
    (our-let ((x 1) (y 2))
        (+ x y)))
; => 3

(mac (our-let ((x 1) (y 2))
        (+ x y)))
; => ((LAMBDA (X Y) (+ X Y)) 1 2)

;
; when-bind
;
(defmacro when-bind ((var expr) &body body)
    `(let ((,var ,expr))
        (when ,var
            ,@body)))

(when-bind (x (car '(1 2 3)))
    (print x)) ; => 1

(when-bind (x (car '()))
    (print x)) ; 実行されない

;
; when-bind*
; 条件式の結果を束縛しつつ評価していく。途中で nil になったら全体の結果も nil。
;
(defmacro when-bind* (binds &body body)
    (if (null binds)
        `(progn ,@body)
        `(let (,(car binds))
            (if ,(caar binds)
                (when-bind* ,(cdr binds) ,@body)))))

(when-bind* ((x (find-if #'consp '(a (1 2) b)))
             (y (find-if #'oddp x)))
    (+ y 10)) ; => 1

(mac (when-bind* ((x (find-if #'consp '(a (1 2) b)))
             (y (find-if #'oddp x)))
    (+ y 10)))
; => (LET ((X (FIND-IF #'CONSP '(A (1 2) B)))) (IF X (WHEN-BIND* ((Y (FIND-IF #'ODDP X))) (+ Y 10))))

;
; our-with-gensyms
;
(defmacro our-with-gensyms (syms &body body)
    `(let ,(mapcar #'(lambda (s)
                        `(,s (gensym)))
                    syms)
        ,@body))

(defmacro my-plus (x)
    (our-with-gensyms (y z)
        (setq y 1 z 2)
        (+ x y z)))

(print (my-plus 1)) ; => 4

