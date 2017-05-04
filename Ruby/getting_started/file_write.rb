
# 追記専用モード
File.open('time.log', 'a'){|f|
  f.puts Time.now
}
