class Cons

  attr :value

  def initialize(x, tail)
    @value = x
    @next = tail
  end

  def cons(x)
    Cons.new(x, self)
  end

  def size()
    if @next.nil? then
      1
    else
      1 + @next.size
    end
  end

  def each(&handler)
    handler.call(@value)
    @next.each{|x|handler.call(x)} unless @next.nil?
  end

  def to_array()
      array = []
      each do |x|
        array.push(x)
      end
      array
  end

  def take(n) 
    if n <= 0 then
      nil
    elsif @next.nil? then
      Cons.new(@value, nil)
    else
      Cons.new(@value, @next.take(n - 1))
    end
  end
end
