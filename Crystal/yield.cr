class A
  property a : String
  property b : Int32

  def initialize
    @a = ""
    @b = 0
    @c = 3.14
  end

  # Note:
  # Optional block are not supported.
  # ref: https://github.com/crystal-lang/crystal/issues/1013
  def initialize(&block)
    initialize
    yield self
  end
end

obj1 = A.new
obj1.a = "a"
obj1.b = 42

pp! obj1

obj2 = A.new do |o|
  o.a = "a"
  o.b = 42
end

pp! obj2
