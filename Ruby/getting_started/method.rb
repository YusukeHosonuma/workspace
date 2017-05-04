
# レシーバの省略
#
# - `self`は省略できる
class Laputa
  def hover
    vibrate # レシーバ`self`は省略可能
  end
  def vibrate
    p 'vibrate'
  end
end

x = Laputa.new
x.hover

# 関数的メソッド
def functional_method(a, b)
  return [a, b, a + b]
end
p functional_method(1,2)  # => [1, 2, 3]

# 引数展開
#
# - 引数についても`*array`で配列を展開して渡せる
def add(a, b)
  return a + b
end
array = [1, 2]
p add(*array) # => 3

# def 式

# 標準
def sum(x, y)
  x + y # return は省略可能
end

# ()を省略
def diff x, y
  x - y
end

# 1行で書くスタイル
def prod(x, y) x * y end
def quo x, y; x / y end

p sum(1, 2)   # => 3
p diff(1, 2)  # => -1
p prod(1,2)   # => 2
p quo(1, 2)   # => 0

# 多値の返却
#
# - `return` の後に`,`区切りで複数書いた場合は、配列として返却される
def increment(x)
  return x, x + 1
end
array = increment 1
p array # => [1, 2]

# 引数のデフォルト値
def greeting(message = 'hello')
  "#{message}!!"
end

p greeting                # => "hello!!"
p greeting 'good morning' # => "good morning!!"

# 引数のデフォルト値に評価式を入れることも可能
#
# - 呼び出したタイミングで評価される
def print_time(time = Time.now)
  p time
end
print_time
sleep 1
print_time

# 可変長引数
#
# - `*`をつけると可変長引数扱いになり、配列として受け取ることが出来る
def multi_var(a, *b)
  p [a, b]
end
multi_var(1, 2, 3) # => [1, [2, 3]]

# ブロック付きメソッド
fruits = <<EOS
Apple
Orange
Banana
EOS

fruits.each_line do |line|
  print line
end

# クロージャとしてのブロック
def create_counter
  count = 1
  return Proc.new do
    count += 1
    p count
  end
end
counter = create_counter
p counter.class # => Proc
counter.call    # => 2
counter.call    # => 3

# ブロック付きメソッドの定義
def foo_bar_baz
  yield "foo" # `yield`で、呼び出し側のブロックをコールバック
  yield "bar"
  yield "baz"
end

foo_bar_baz do |item|
  puts item
end

# イテレータの中の yield
def foo_bar_baz2
  %w[ foo bar baz ].each do |item|
    yield item
  end
end

foo_bar_baz2 do |item|
  puts item
end

# yield の評価値
def my_map
  [yield(1), yield(2), yield(3)]
end
p my_map{|i| i + 1 }  # => [2, 3, 4]

# Proc
#
# - `&`をつけることでブロックを参照できる
class SleepyPerson
  def register_handler(&handler)
    @event_handler = handler # インスタンス変数に保持
  end
  def wake_up!
    @event_handler.call Time.now, 'woke up'
  end
end
john = SleepyPerson.new
john.register_handler {|time, message| p [time, message]}
john.wake_up!

# Proc からブロックへ変換して渡す
#
# - 呼び出し時に`&`をつけることでブロックに変換されて渡される
proc = Proc.new { puts 'Proc was called' }
3.times(&proc)
