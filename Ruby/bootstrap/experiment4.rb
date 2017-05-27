require "minruby"

pp(minruby_parse("
if 0 == 0
  p(42)
else
  p(43)
end
"))