def f(x):
    if x in (0, 1):
        return 1
    return f(x - 1) + f(x - 2)

for x in range(10):
    print(x, f(x))

g = f
del f
print('\n Renamed f to g. \n')

for x in range(10):
    print(x, g(x))
