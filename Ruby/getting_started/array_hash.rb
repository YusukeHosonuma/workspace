
array = ["Bear", "Benford", "Egan"]
p array.map{|name| "G. #{name}" }

hash = {
  "Baxter" => "Stephen",
  "Stross" => "Charles",
  "Reynolds" => "Alastair",
}
hash.each do |key, value|
  puts "#{value} #{key}"
end