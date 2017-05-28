class Foo
  def bar
    @x = 1
  end
  attr_reader :x
end

f = Foo.new
p f.x                  # => nil
p f.instance_variables # => []

f.bar # これを呼び出すことによりインスタンス変数`@x`が追加

p f.x                  # => 1
p f.instance_variables # => [:@x]


class Bar 
  def foo
    puts "foo"
  end
end

b = Bar.new
p b.class                          # => Bar
p Bar.instance_methods.grep(/foo/) # => [:foo]


p "hello".class # => String
p String.class  # => Class
p Class.class   # => Class


# `false`は継承されたメソッドを表示しないという意味
p Class.instance_methods(false) # => [:new, :allocate, :superclass]


p Array.superclass        # => Object
p Object.superclass       # => BasicObject
p BasicObject.superclass  # => nil


p Class.superclass  # => Module


X = "::X"
module M
  X = "M::X"
  class C
    X = "M::C::X"  
  end
end

p ::X     # => "::X"
p M::X    # => "M::X"
p M::C::X # => "M::C::X"


Y = "::Y"
module M2
  Y = "M2::Y"
  p Y   # => "M2::Y"
  p ::Y # => "::Y"
end


module Editor
  class String
  end
end

p String == String          # => true
p String == Editor::String  # => false
