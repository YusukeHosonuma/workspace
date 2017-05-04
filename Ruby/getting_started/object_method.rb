a, b = "str", "str"
def a.greet
  puts "I am the object #{self.object_id}"
end

a.greet #=> I am the object 70108609469400
b.greet #=> NoMethodError
