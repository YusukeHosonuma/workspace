p Fixnum.class            # => Class
p Fixnum.object_id        # => 70247285962420
p Fixnum.ancestors        # => [Fixnum, Integer, Numeric, Comparable, Object, Kernel, BasicObject]
p Fixnum.instance_methods
p 1.kind_of? Fixnum       # => true
p "str".kind_of? Fixnum   # => false
p Fixnum.kind_of? Class   # => true
p Fixnum.kind_of? Object  # => true
