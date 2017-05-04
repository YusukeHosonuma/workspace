
# プログラム引数からの仮想ファイルから読み込む
#
# $ ruby ARGF.rb hello.txt time.log
#
ARGF.each_line do |line|
  print line
end
