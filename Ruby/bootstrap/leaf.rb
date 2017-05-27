leaf_a = ["葉A"]
leaf_b = ["葉B"]
leaf_c = ["葉C"]
leaf_d = ["葉D"]

node2 = ["節2", leaf_a, leaf_b]
node3 = ["節3", leaf_c, leaf_d]

node1 = ["節1", node2, node3]

node1 = [
  "節1",
  ["節2", ["葉A"], ["葉B"]],
  ["節3", ["葉C"], ["葉D"]]
]

p(node1[0])

node3 = node1[2]
p(node3)

p(node2[0])
p(node2[1])
p(node2[2])

leaf_a = node2[1]
p(leaf_a[0])
p(leaf_a[1]) #=> nil　- 範囲外参照した場合はnil
p(leaf_a[2])
