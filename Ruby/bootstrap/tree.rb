def preorder(tree)
  p(tree[0])
  if tree[0].start_with?("節")
    preorder(tree[1])
    preorder(tree[2])
  end
end

def preorder2(tree)
  if tree[1] == nil # 葉だけ出力
    p(tree[0])
  else
    preorder2(tree[1])
    preorder2(tree[2])
  end
end

def postorder(tree)
  if tree[0].start_with?("節")
    postorder(tree[1])
    postorder(tree[2])
  end
  p(tree[0])
end

node1 = ["節1", ["節2", ["葉A"], ["葉B"]], ["節3", ["葉C"], ["葉D"]]] # 動作確認用のデータ
# preorder(node1)
# preorder2(node1)
postorder(node1)
