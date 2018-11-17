def factorial(x):
	if x == 0:
		return 1
	else:
		return x * factorial(x - 1)

x = factorial(5)
print(f"5! = {x}")
