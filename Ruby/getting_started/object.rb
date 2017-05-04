# クラス定義
class Duration
  1 + 1 # 任意の式を書ける
  def display; puts self end
end

# 継承
class Animal
  def greeting
    puts 'Hello'
  end
end

class Cat < Animal
end

mike = Cat.new
mike.greeting # => "Hello"

duration = Duration.new
duration.display  # => #<Duration:0x007ff8f0869408>

# クラスメソッド
class Duration
  def Duration.print(x); p x end
end
Duration.print 1  # => 1

# `self`を使ったクラスメソッド定義（`self`は Durationを指すので）
class Duration
  def self.print2(x); p x end
end
Duration.print2 1 # => 1

# インスタンス化
class Duration
  def initialize(since, till)
    @since = since
    @until = till
  end
  attr_accessor :since, :until
end
duration = Duration.new(Time.now, Time.now + 3600)
p duration.until
p duration.since = Time.now

# クラス定義の追加
class String
  def caesar; tr 'a-zA-Z', 'n-za-mN-ZA-M' end
end
puts 'Learning Ruby'.caesar # => Yrneavat Ehol

# 定義の上書き
class Fixnum
  alias original_addition +
  def +(rhs)
    original_addition(rhs).succ
  end
end
p 1 + 1 # => 3
p 5 + 2 # => 8

# 上書きの禁止
Fixnum.freeze
class Fixnum
  # => can't modify frozen class (RuntimeError)
  # def +(rhs); return 5 end
end

class Duration
  def display(target=$>)
    super
    target.write "(#{self.since}-#{self.until})"
  end
end
duration.display  # => #<Duration:0x007fd179091120>(2017-05-01 22:40:52 +0900-2017-05-01 23:40:52 +0900)

# クラス変数
#
# - クラス変数はクラス継承階層で共有される（そのクラスのみではない）
class Foo
  @@class_variable = 'foo'
  def print
    p @@class_variable
  end
end
class Bar < Foo
  p @@class_variable
  @@class_variable = 'bar'
  def print
    p @@class_variable
  end
end
foo = Foo.new
foo.print     # => "bar" - L89で更新済み
bar = Bar.new
bar.print     # => "bar"

# 定数
class Duration
  DAYS_OF_WEEK = 7
  p DAYS_OF_WEEK

  def print_days_of_week
    p DAYS_OF_WEEK
  end
end
p Duration::DAYS_OF_WEEK

# アクセス制限
class Yapoo
  # デフォルトでは`public`
  def public_method; end

  # 以降は`private`となる
  private
  def internal_use; end
  def public_api
    return internal_use
  end
  public :public_api  # public_api が`public`になる
end
yapoo = Yapoo.new
yapoo.public_api
# => private method `internal_use' called for #<Yapoo:0x007f818304c3b8> (NoMethodError)
#yapoo.internal_use

# 特異メソッド
#
# - 特定のオブジェクトのみに所属するメソッド
message = 'Hello'
def message.build_greeting(target)
  "#{self}, #{target}"
end
message.build_greeting('world')
message2 = 'Hello'
# => undefined method `build_greeting' for "Hello":String (NoMethodError)
#message2.build_greeting('world')

# Singleton
# CENTRAL_REPOSITORY = Object.new
# def CENTRAL_REPOSITORY.register(target)
#   @registered_objects ||= []
#   unless @registered_objects.include? target
#     puts "register: #{target}"
#     @registered_objects << target
#   end
# end
# def CENTRAL_REPOSITORY.unregister(target)
#   @registered_objects ||= []
#   puts "unregister: #{target}"
#   @registered_objects.delete(target)
# end
# CENTRAL_REPOSITORY.register(1)
# CENTRAL_REPOSITORY.unregister(1)

# 特異クラス
#
# - ある特定のインスタンスのためだけのクラス
# - クラス式で`class << object`という形で記述する
# - 特異メソッドは、特異クラスのインスタンスメソッドとして実装される
CENTRAL_REPOSITORY = Object.new
class << CENTRAL_REPOSITORY
  def register(target)
    @registered_objects ||= []
    unless @registered_objects.include? target
      puts "register: #{target}"
      @registered_objects << target
    end
  end
  def unregister(target)
    @registered_objects ||= []
    puts "unregister: #{target}"
    @registered_objects.delete(target)
  end
end

# クラスメソッドとメタクラス
#
# - クラスメソッドは Class オブジェクトの特異メソッド
class Duration
  def self.a_week_from(from)
    return self.new(from, from + 7 * 24 * 60 * 60)
  end
end
p Duration.a_week_from(Time.now)

# メタクラス
class Duration
  class << self # Durationオブジェクトの特異クラスを実装
    def a_week_from(from)
      return self.new(from, from + 7 * 24 * 60 * 60)
    end
  end
end
p Duration.a_week_from(Time.now)

# Mix-in
class Foo
  include Comparable
  def initialize(x)
    @x = x
  end
  def <=>(rhs)
    @x <=> rhs.x
  end
  attr :x
end
foo1 = Foo.new(1)
foo2 = Foo.new(2)
# Comparable mix-in により（`<=>`の実装だけで）以下のメソッドが利用可能になっている
p foo1 <  foo2  # => true
p foo1 == foo2  # => false
p foo1 >  foo2  # => false

# 名前空間としてのモジュール
module Library
  class Service; end
end
service = Library::Service.new
p service # => #<Library::Service:0x007ffa8e81dbf8>

