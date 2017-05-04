
# 1行ずつ読み取り
File.open('time.log'){|f|
  f.each_line do |line|
    print "#{f.lineno}: #{line}"
  end
}

# 手動によるリソース管理
f = File.open('time.log')
p f.readline
f.close

# 標準入出力
p $stdin  # => #<IO:<STDIN>>
p $stdout # => #<IO:<STDOUT>>
p $stderr # => #<IO:<STDERR>>
$stderr.printf("%X\n", 0xcafe)  # => CAFE

# 標準出力の先を変更
File.open('hello.txt', 'w'){|f|
  $stdout = f
  puts 'Welcome ようこそジャパリパーク！'
  $stdout = STDOUT
}
