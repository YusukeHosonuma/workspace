a = []
for i in 1..3
  a.push(lambda {i})
end

for f in a
  print "#{f.call()} "
end
