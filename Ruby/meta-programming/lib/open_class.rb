#
# オープンクラス
#

# 既存の`String`に対してメソッドを追加できる
class String
  def to_alphanumeric
    gsub(/[^\w\s]/, '')
  end
end

class D
  def x; 'x'; end
end
class D # ここで再オープン
  def y; 'y'; end
end

module M
  class C
    X = 'X'
  end
  C::X # => "X"
end
module M
  Y = 'Y'
end
