p 1.class           # => Fixnum
p 1.object_id       # => 3
p 1.methods
p "str".class       # => String
a, b = "str", "str"
p a.object_id       # => 70307948186920
p b.object_id       # => 70307948186900
p a == b            # => true
p a.equal? b        # => false
