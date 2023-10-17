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

def mul(a:int,b:int) -> int:
    return accumulate(lambda x,y: x+y,
                      lambda a: a+1,
                      lambda x:b, # this requires closure
                      0,0,a)

def fact(n:int) -> int:
    return accumulate(mul,
                      lambda x: x+1,
                      lambda x: x,
                      1,1,n+1)

print(1 if fact(10) == 3628800 else 0)