#
# メソッド探索
#

class MyClass
  def my_method; 'my_method()'; end
end
class MySubclass < MyClass
end

module M1
  def my_method
    'M1#my_method()'
  end
end

class C1
  include M1
end

class D1 < C1
end
