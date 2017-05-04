
# 変数は参照（ポインタ）が格納される
cattle = 'yahoo'
p cattle.length
animal = cattle
cattle[2] = ?p
p cattle                      # => "yapoo"
p animal                      # => "yapoo"
p cattle.equal? animal  # => true

# `dup`
# - 変数を複製する
def describe(name)
  name = name.dup
  puts "This is a #{name}." # => This is a yahoo.
  name[2] = ?p
  puts "This is a #{name}." # => This is a yapoo.
end

s = 'yahoo'
describe(s)
p s # => "yahoo"

# ローカル変数
def next_of(origin)
  value = origin + 1
  p value
end

def prev_of(origin)
  value = origin - 1
  p value
end

next_of(2)  # => 3
next_of(2)  # => 3
prev_of(2)  # => 1
# p value # => NameError

class Duration
  attribute_names = %w[ days hour minute seconds ]
  p attribute_names
  attribute_names.each do |name|
    attr_accessor name
  end
end
# => NameError
# p attribute_names

# 定数
CONST = 1
# CONST = 2 # => warning: already initialized constant CONST

# ライブラリの定数
p RUBY_VERSION  # => 2.4.1

# インクリメント・デクリメント演算子はない
#
# - `upto`などやイテレータが使える
0.upto(9){|i| puts i }
str = 'hello'
str.each_byte do |byte|
  printf "%x\n", byte
end

# 多重代入
#
# - 足りない分は`nil`が代入される
a, b, c = 1, 2
p a # => 1
p b # => 2
p c # => nil

# 配列展開
#
# - `*variable =`で多値を配列に展開する
# - 逆に右辺の配列に`*array`とすることで、多値に分解する
a, *b = 1, 2, 3, 4, 5
p b # => [2, 3, 4, 5]

array = [1, 2, 3]
a, b, c = *array
p a # => 1
p b # => 2
p c # => 3

# 論理和・論理積
#
# - C言語とは異なり、オペランドのいずれかを返す
p nil || 50 # => 50

# 初期化イディオム
#
# - `@a ||= default_value` という形でデフォルト値を適用できる
def generate_default_value
  p 'default value generated'
  return 'default value'
end
def some_method(param = nil)
  param ||= generate_default_value
end

some_method(nil)    # => デフォルト値が生成
some_method(false)  # => デフォルト値が生成
some_method(true)   # => なし
some_method(0)      # => なし
some_method()              # => デフォルト値が生成

# 範囲演算子
#
# - `a .. b`は`b`を含む
# - `a ... b`は`b`を含まない
for i in 0...10
  puts i
end

# 条件演算子（三項演算子）
a = nil
var = a ? 'true' : 'false'
p var # => false

# 制御式
color = 'green'
thought = if color == 'green' then
            'danger'
          else
            'undefined'
          end
p thought # => "danger"

# if-then-elsif-else
a = 2
s = if a == 1 then
      'one'
    elsif a == 2 then
      'two'
    else
      'many'
    end
p s # => "two"

# if 修飾子
a = 1
puts 'hello' if a == 1  # => "hello"
puts 'hello' if a == 2  # =>

# unless
unless a == 2
  puts 'a != 2'
end

# case 式
#
# `===`というcase比較演算子を利用して判定される
a = 2
s = case a
      when 1 then
        'one'
      when 2 then
        'two'
      else
        'many'
    end
p s # => "two"

# 範囲分岐
value = 3
s = case value
      when 0      then '0'
      when 1..9   then '1桁'
      when 10..99 then '2桁'
    end
p s # => "1桁"

# 正規表現による分岐
value = '3'
s = case value
      when '0'          then '0'
      when /\A\d\Z/     then '1桁'
      when /\A\d{2}\Z/  then '2桁'
      else                   'それ以外'
    end
p s # => "1桁"

# case式 その2
#
# - `case`の後に特定の値を与えない
# - 同じような判定が多い場合は、if-elsif よりも読みやすくなることがある
number = 2
s = case
      when number == 1 then 'one'
      when number == 2 then 'two'
      else 'many'
    end
p s # => "two"

# while
i = 0
while i < 10
  puts i
  i += 1
end

# 後置 while
i = 0
begin
  puts i
  i += 1
end while i < 10

# until
#
# - `until condition` で condition を満たすまで繰り返す
i = 0
until i > 10
  puts i
  i += 1
end

# for
for name, num in [ ['Jan', 1], ['Feb', 2] ]
  puts "#{name} is #{num}"
end

# loop
#
# - 無限ループ
i = 0
loop do
  puts i
  i += 1
  break if i > 5
end

# times
3.times {|i|
  puts "Yahoo: #{i}"
}

# upto / downto
1.upto(3)   do |i| print "#{i} " end
3.downto(1) do |i| print "#{i} " end

# break
#
# - `break`には引数をつけることが出来、ループ全体の戻り値となる
a = loop { break 1 }
p a # => 1

# next
1.upto(3) do |i|
  next if i == 2
  puts i
end

# 例外処理
#
# - begin-end で囲み、`rescue`でキャッチ、`ensure`が必ず処理
begin
  unefined.foo
rescue TypeError
  puts 'type error'
rescue => some_error
  puts "Some error: #{some_error}"
  puts some_error.class
else
  puts '例外なし'
ensure
  puts 'ensure'
end

# rescue 修飾子
#
# - 単純な例外処理
# - 例外の種類を指定したり、例外オブジェクトを補足することは出来ない
undefined.foo rescue puts 'エラー'

# raise
def raise_argument_error()
  raise ArgumentError, 'message'
end
def raise_runtime_error(message)
  if message
    raise message # message付きのRuntimeError
  else
    raise # messageなしのRuntimeError
  end
end

begin
  raise_argument_error
rescue ArgumentError
  puts 'rescue: ArgumentError'
else
  puts 'success'
end

begin
  raise_runtime_error 'hello'
rescue RuntimeError => error
  puts "rescue: #{error}"
else
  puts 'success'
end

# 大域脱出
catch(:exit) {
  loop do
    loop do
      throw :exit # スタックを遡り :exit まで探す
    end
  end
}

