class Person
  attr_accessor :age
  def initialize()
    @age = 13
  end
  def teen?
    13 <= @age && @age <= 19
  end
end
