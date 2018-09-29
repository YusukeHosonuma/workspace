require 'hashie'

yaml = Hashie::Mash.load('./sample.yml')

# method_missing を利用して、.記法でアクセスできるようにしている
puts yaml.foo # => xxx
puts yaml.bar # => yyy
