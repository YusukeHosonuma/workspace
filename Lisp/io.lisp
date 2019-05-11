;
; with-open-file はファイルハンドルを取得し、最後に自動的に閉じる
;
(with-open-file (s "dump" :direction :output)
    (princ 99 s))
