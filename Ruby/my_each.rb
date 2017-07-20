class Array
  def my_each
    i = 0
    while i < self.length
      x = self[i]
      yield x
      i = i + 1
    end
  end
end

xs = [1, 2, 3]
xs.my_each do |x|
  puts x
end
