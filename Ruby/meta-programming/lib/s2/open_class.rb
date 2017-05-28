class String
  def emphasize
    self + '!!'
  end
end

p "Hello, world".emphasize # => "Hello, world!!"


p "Apple".length # => 5

class String
  def length
    1
  end
end

p "Apple".length # => 1


3.times do
  class Foo
    puts "Hello"
  end
end
