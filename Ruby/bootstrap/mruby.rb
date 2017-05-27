require "minruby"

def evaluate(tree)
  case tree[0]
  when "lit"
    tree[1]
  when "+"
    left  = evaluate(tree[1])
    right = evaluate(tree[2])
    left + right
  when "-"
    left  = evaluate(tree[1])
    right = evaluate(tree[2])
    left - right
  when "*"
    left  = evaluate(tree[1])
    right = evaluate(tree[2])
    left * right
  when "/"
    left  = evaluate(tree[1])
    right = evaluate(tree[2])
    left / right
  when "%"
    left  = evaluate(tree[1])
    right = evaluate(tree[2])
    left % right
  when "**"
    left  = evaluate(tree[1])
    right = evaluate(tree[2])
    left ** right
  when "=="
    left  = evaluate(tree[1])
    right = evaluate(tree[2])
    left == right
  when "<"
    left  = evaluate(tree[1])
    right = evaluate(tree[2])
    left < right
  when ">"
    left  = evaluate(tree[1])
    right = evaluate(tree[2])
    left > right
  end
end

def max(tree)
  case tree[0]
  when "lit"
    tree[1]
  else
    left  = max(tree[1])
    right = max(tree[2])
    if left > right
      left
    else
      right
    end
  end
end

p(max(minruby_parse("1 + 2 * 3"))) #=> 3
p(max(minruby_parse("1 + 4 + 3"))) #=> 4

# str = gets
# tree = minruby_parse(str)
# answer = evaluate(tree)
# p(answer)
