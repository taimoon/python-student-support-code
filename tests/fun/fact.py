def accumulate(op:Callable[[int,int],int],
               nxt:Callable[[int],int],
               f:Callable[[int],int],
               init:int,
               a:int,
               b:int) -> int:
    while a < b:
        init = op(init,f(a))
        a = nxt(a)
    return init

def inc(x:int) -> int:
    return add(x,1)

def identity(x:int) -> int:
    return x

def add(a:int,b:int) -> int:
    return a+b

def mul(a:int,b:int) -> int:
    init = 0
    i = 0
    while i < b:
        init = add(init,a)
        i = inc(i)
    return init

def Mul() -> Callable[[int,int],int]:
    return mul

def Identity() -> Callable[[int],int]:
    return identity

def Inc() -> Callable[[int],int]:
    return inc

def factorial(n:int) -> int:
    return accumulate(Mul(),Inc(),Identity(),1,1,n+1)

print(factorial(10))
