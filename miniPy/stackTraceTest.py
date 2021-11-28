def f():
    1 / 0

def g():
    print('This is g,', f())
    return 97

if g() == 97:
    pass
