
# eval
code = %q[puts 'hello, world']
eval(code) # => hello, world

# コンテキストのすり替え
class CybernatedAndroid
  def initialize(name) @name = name end
end
proc = Proc.new {
  p self
  p @name
}
proc.call
  # => main
  # => nil

dicey = CybernatedAndroid.new('dicey1')
dicey.instance_eval(&proc)  # diceyのインスタンスというコンテキストで`proc`を評価
  # => #<CybernatedAndroid:0x007fb4b016d6b0 @name="dicey1">
  # => "dicey1"

dicey = CybernatedAndroid.new('dicey2')
CybernatedAndroid.class_eval do # クラスのコンテキストで評価
  def save; puts 'saved' end
end
dicey.save  # => saved

# method_missing
class Reporter
  def method_missing(method_name, *arguments)
    puts "メソッド#{method_name}が次の引数で呼ばれました"
    arguments.each{|arg| puts arg }
  end
end
reporter = Reporter.new
reporter.report           # => メソッドreportが次の引数で呼ばれました
reporter.emergency 1, 2
  # => メソッドemergencyが次の引数で呼ばれました
  # => 1
  # => 2

# set_trace_func
#
# - Ruby処理系にフックできる
# set_trace_func proc { |event, file, line, id, binding, classname|
#   printf "%8s %s:%-2d %10s %8s\n", event, file, line, id, classname
# }
# 1 + 1

# 継続
#
# - 後続の処理を保存して、あとから再開できる仕組み
require 'continuation'
1.upto(10) do |i|
  if i == 3
    $cont = callcc{|continuation|
      continuation
    }
  end
  print i, ' '
end
# 非推奨になって、`Fiber`の利用が推奨されるようになったらしい
# => warning: callcc is obsolete; use Fiber instead
$cont.call(nil) if $cont
# => 1 2 3 4 5 6 7 8 9 10 3 4 5 6 7 8 9 10
