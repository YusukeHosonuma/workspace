require 'pry'

x = 10
y = 20
z = x + y

binding.pry # Debug

puts "x = #{x}"
puts "y = #{y}"
puts "z = #{z}"

# ~/.pryrc
#
# if defined?(PryByebug)
#   Pry.commands.alias_command 's', 'step'
#   Pry.commands.alias_command 'n', 'next'
#   Pry.commands.alias_command 'f', 'finish'
#   Pry.commands.alias_command 'c', 'continue'
# end
