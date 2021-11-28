def f():
    a = 0
 
    def g():
        print('a =', a)
 
    return g

def h():
    g = f()
    a = 1
    g()

h()
