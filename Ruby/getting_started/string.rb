
# new() での構築
str = String.new()
str << 72 << 101 << 108 << 108 << 111
p str # => Hello

# Unicode
p "\u2318"             # => ⌘
p "\u{3042 3044 3046}" # => あいう

# 式展開（内部的に`.to_s()`が呼び出される）
a = 2
p "a is #{a}"       # => "a is 2"
p "a ^ 5 = #{a**5}" # => "a ^ 5 = 32"

p "Current time is: #{Time.now}" # => "Current time is: 2017-05-01 13:19:49 +0900"
p "Standard input is: #{$stdin}" # => "Standard input is: #<IO:0x007f8b5c07a988>"

# to_s
# `puts`の内部で利用される。人間の目で見て分かりやすい形で出力される。
p 1.to_s      # => "1"
p true.to_s   # => "true"
p nil.to_s    # => ""
p 'str'.to_s  # => "str"

# inspect
# `p`の内部で利用される。オブジェクトの内部表現に適した形で出力される。
puts
puts 'inspect'
p 1.inspect     # => "1"
p true.inspect  # => "true"
p nil.inspect   # => "nil"
p 'str'.inspect # => "\"str\""

# Marshal
# シリアライズ・デシリアライズ
p s = Marshal.dump('string') # => "\x04\bI\"\vstring\x06:\x06ET"
p Marshal.load(s)     # => "string"

# バッククォート文字列
# Shellコマンドを実行した結果が格納される
puts
puts 'バッククォート文字列'
p `date`  # => "Mon May  1 13:29:25 JST 2017\n"
p `pwd`   # => "/Users/yusuke/Desktop/workspace/Ruby\n"

# パーセント記法
# - シングルクォートやダブルクォートをエスケープしないで済むので可読性が上がるケースもある
# - %q!xxx! の`!`には任意の記号が使え、`[`などの開始記号の場合は対応した終了記号を使う
puts
puts 'パーセント記法'
p %q[中に"が含まれる文字列] # => "中に\"が含まれる文字列"
p %Q^中に'が含まれる文字列^ # => "中に'が含まれる文字列"

# ヒアドキュメント
#
# - <<"EOS" といった形で"を利用すると文字列展開される
# - <<EOS といった形で省略すると文字列展開は行われない
w = 'heredoc'
s = <<"EOS"
This
is
a
#{w}.
EOS
p s # => "This\nis\na\nheredoc.\n"

# `<<-`とすることで、最初にインデントが入っていてもヒアドキュメントの終わりとして認識される
fruits = %w(apple orange banana)
fruits.each do |fruit|
  p <<-"EOS"
    Fruit is >
    #{fruit}
  EOS
end

# 文字リテラル
# - 先頭に`?`をつけると文字リテラルとして認識される
p ?1  # => "1"
p ?a  # => "a"
p ?あ # => "あ"

# 正規表現
#
# - `if /regexp/ =~ string` で簡単に判定できる
# - `()`でキャプチャした結果は $1 などで参照できる
# - マッチしなかった場合は`nil`が返却される（`if`では偽と判定される）
#

# マッチ
s = '<a href="http://www.google.co.jp/">google</a>'
if /<a href="(.*?)"[ >]/ =~ s
  puts 'find link to ' + $1
end

# 鬼車によるマッチ（変数が使える）
if /<a href="(?<url>.*?)"[ >]/ =~ s
  puts 'find link to ' + url
end

# 部分文字列へのアクセス
s = 'Hello, world'
p s[0]      # => "H"
p s[1]      # => "e"
p s[-1]     # => "d"
p s[7, 3]   # => "wor" -- [index, size]
p s[7..9]   # => "wor" -- [begin..end]
p s[/\s.*/] # => " world" -- [/regex/]

# 部分文字列の更新（最初の一つのみが更新される）
fruits = <<EOS
Apple is red.
Orange is orange.
Banana is yellow.
EOS
fruits['Apple is red.'] = 'Bad apple'
p fruits  # => "Bad apple\nOrange is orange.\nBanana is yellow.\n"

# gsub（マッチするすべてが置換される）
fruits = fruits.gsub(/is/, '=')
p fruits  # => "Bad apple\nOrange = orange.\nBanana = yellow.\n"

# 文字列操作
p 'str' + 'ing' # => "string"
str = 'str'; str << 'ing' # `<<`は破壊的な代入を行う
p str # => "string"

# 反復
p 'Look! ' * 3 # => "Look! Look! Look! "

# 分解
p 'a,bb, ccc, dddd'.split(/,\s?/) # => ["a", "bb", "ccc", "dddd"]
p 'string'.split(//)              # => ["s", "t", "r", "i", "n", "g"]

# 比較
p 'abc' ==  'abc' # => true
p 'abc' <   'abd' # => true
p 'abc' <   'acc' # => true
p 'a'   <   'aa'  # => true
p 'add' <=> 'acc' # => 1

# その他のAPI
p 'string'.reverse      # => "gnirts"
p "\n \rstring  ".strip # => "string"
p 'string'.length       # => 6

# イテレータ

# byte単位のイテレート
'str'.each_byte do |byte|
  p byte
end

# char単位でのイテレート
'str'.each_char do |char|
  p char
end

# 行単位でのイテレート
s = <<EOS
Apple
Orange
Banana
EOS
s.each_line do |line|
  p line
end

# フォーマット

# sprintf
p sprintf('%04d', 3)                # => "0003"
p sprintf('%08.4f', Math::PI * 10)  # => "031.4159"
p sprintf('hex=%X, oct=%o', 10, 10) # => "hex=A, oct=12"

# String#%
p '%04d' % 3
p '%08.4f' % (Math::PI * 10)
p 'hex=%X, oct=%o' % [10, 10]

# シンボル
#
# - 文字列と違って同値であれば、プロセス内で同一オブジェクトとなる
# - 文字列と違い不変
:'Anna Terras'
:"#{$$}"
:if
:some_method_name

s1 = :ruby
s2 = :ruby
p s1 == s2  # => true
p s1.equal? s2  # => true

# $KCODE
#
# - 最新のRubyでは使用不可になったらしい
# - => warning: variable $KCODE is no longer effective; ignored
#
# str = "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e"
# p str # => "日本語"
#
# $KCODE = 'UTF8'
# puts str.inspect
#
# $KCODE = 'SJIS'
# puts str.inspect

# 正規表現
'あいうえお'.scan(/./u) do |c|
  puts c
end

# エンコーディング
p 'あいう'.encoding  # => #<Encoding:UTF-8>
p 'あいう'.length    # => 3
p 'あいう'.bytesize  # => 9

p Encoding.name_list
p Encoding.find('CP1258') # => #<Encoding:Windows-1258>

# 内部表現
p '将軍'.encoding       # => #<Encoding:UTF-8>
p 'shogunate'.encoding # => #<Encoding:UTF-8>

# コード変換
utf = '日本'
p utf.encoding  # => #<Encoding:UTF-8>
sjis = utf.encode('Shift_JIS')
p sjis.encoding # => #<Encoding:Shift_JIS>

# 文字列結合
sjis = 'ラグナグ'.encode 'SJIS'
euc = 'への旅'.encode 'EUC-JP'
# sjis + euc # => Encoding::CompatibilityError
