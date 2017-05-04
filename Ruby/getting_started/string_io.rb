require 'stringio'

buffer = String.new
sio = StringIO.new(buffer)

$stderr = $stdout = sio

puts 'hello'
warn 'world'
$stdout = STDOUT

puts buffer
