a = 1
b = "str"
p c = [a, b, 3, "文字列"]
p d = [a, c, [1, 2, 3]]

# 添字
p c[0]
p c[1]
p c[2]
p c[3]
p c[4] # => nil
p d[2]

# 負の添字
p c[-1] # => "文字列"
p c[-2] # => 3
p c[-5] # => nil

puts
puts "--- 長さ付き添字 ---"
p c[1, 2]   # => ["str", 3]
p c[1, 3]   # => ["str", 3, "文字列"]
p c[1, 4]   # => ["str", 3, "文字列"]
p c[-2, 2]  # => [3, "文字列"]
p c[4, 2]   # => []

puts
puts "--- 範囲添字 ---"
p c         # => [1, "str", 3, "文字列"]
p c[0..1]   # => [1, "str"]
p c[0...1]  # => [1]
p c[-2..-1] # => [3, "文字列"]
p c[-2..3]  # => [3, "文字列"]
p c[-2...3] # => [3]
p c[4..5]   # => []

puts
puts "--- 添字代入 ---"
a = [1, 2]
a[0] = 3
p a # => [3, 2]
a[4] = "4"
p a # => [3, 2, nil, nil, "4"]
a[0, 3] = 'a', 'b', 'c'
p a # => ["a", "b", "c", nil, "4"]
a[0, 3] = 'a', 'b', 'c', 'd'
p a # => ["a", "b", "c", "d", nil, "4"]
a[1..2] = 1, 2
p a # => ["a", 1, 2, "d", nil, "4"]
a[0, 2] = "?"
p a # => ["?", 2, "d", nil, "4"]
a[0..2] = "A"
p a # => ["A", nil, "4"]
a[-1]   = "Z"
p a # => ["A", nil, "Z"]

puts
puts "--- 配列の比較 ---"
p array1 = [1, 2, "str"]
p array2 = [1, 2, "str"]
p array1 == array2
p array1 == ["str", 1, 2] # 順序が違う
p array1 == [1, 2, "str", "extra"] # 要素が多い

puts
puts "--- メソッド ---"
array = ["a", "b", "c"]
p array.length
p array.size
p array *= 2

p array.include? "c"
p array.sort
p array
p array.uniq
array.uniq!
p array

puts
puts "--- Iterator ---"

array = ["a", "b", "c"]
array.each do |item|
  print item + " "
end
puts

array.each_with_index do |item, index|
  p [item, index]
end

puts
puts "--- map ---"

fruits = ["Apple", "Orange", "Banana"]
signs = fruits.map{|fruit| fruit[0, 1] }
p signs
p fruits.map{|f| f.downcase}
p fruits.map(&:downcase)

puts
puts "--- sort ---"

array = ["73", "2", "5", "1999", "53"]
p array.sort
p array.sort{|x, y| x.to_i <=> y.to_i}

puts
puts "--- select ---"

p array.select{|n| n.to_i % 2 == 0}

puts
puts '--- Hash ---'

month_to_ordinal = {
    'Jan' => 1, 'Feb' => 2, 'Mar' => 3, 'Apr' => 4, 'May' => 5, 'Jun' => 6,
    'Jul' => 7, 'Aug' => 8, 'Sep' => 9, 'Oct' => 10, 'Nov' => 11, 'Dec' => 12,
}
p month_to_ordinal['Aug']
p month_to_ordinal['Jun']

params = { rin: 5, kimiko: 7, kayo: nil }
p params

puts
puts '--- 添字演算式 ---'

book_to_author = {
    'Ruby in Nutshell' => 'Flanagan',
    'Programming Ruby' => 'Thomas',
    'AWDwR' => 'Thomas',
}
p book_to_author['Programming Ruby']
p book_to_author['Programming Perl']
book_to_author['Ruby in Nutshell'] = %w(Flanagan Matz)
book_to_author['The Ruby Way'] = 'Fulton'
p book_to_author

# each
book_to_author.each do |book, author|
  puts "#{book} by #{author}"
end

# map
p book_to_author.map{|book, author|
  "#{book} by #{author}"
}

# map.with_index
p book_to_author.map.with_index {|(book, author), index|
  "#{index+1}. #{book}"
}