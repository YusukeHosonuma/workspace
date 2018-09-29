require 'yaml'

yaml = YAML.load_file('./anchor.yml')

# アンカー（&xxx）が付いた属性を参照
puts yaml['foo'][0]['name'] # => hello

# エイリアス（*xxx）が付いた属性を参照
puts yaml['bar'][0]['name'] # => hello

# アンカー（&xxx）が付いた構造を参照
config = yaml['foo'][0]['config']
puts config['min'] # => 0
puts config['max'] # => 5

# エイリアス（*xxx）が付いた構造を参照
config = yaml['bar'][0]['config2']
puts config['min'] # => 0
puts config['max'] # => 5

# マージ（<<:）された構造を参照
setting = yaml['bar'][0]
puts setting['min'] # => 1
puts setting['max'] # => 3
