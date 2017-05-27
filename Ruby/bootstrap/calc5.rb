answer = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10
guess = gets.to_i
if guess == answer
  p("あたり！")
  p("おめでとうございます！")
else 
  p("はずれ！")
  if guess > answer
    p("それはちょっと大きすぎるよ！")
  else
    p("それはちょっと小さすぎるよ！")
  end
end
p("また遊んでね！")