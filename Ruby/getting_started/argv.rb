# プログラム引数
expression = ARGV.join(' + ')
total = ARGV.inject(0){|subtotal, arg| subtotal + arg.to_i}
puts "#{expression} = #{total}" # => 1 + 2 + 3 = 6

