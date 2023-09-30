def accumulate(op:Callable[[int,int],int],
               nxt:Callable[[int],int],
               pred:Callable[[int],bool],
               init:int,
               a:int,
               b:int) -> int:
    while a < b:
        init = op(init,a) if pred(a) else init
        a = nxt(a)
    return init

def add(a:int,b:int) -> int:
    return a+b

def mul(a:int,b:int) -> int:
    init = 0
    i = 0
    while i < b:
        init = init + a
        i = i+1
    return init

def remainder(x:int,d:int) -> int:
    return remainder(x+(-d),d) if x+(-d) >= 0 else x

def is_even(x:int) -> bool:
    return remainder(abs(x),2) == 0

def abs(x:int) -> int:
    return x if x >= 0 else (-x)

def inc(x:int) -> int:
    return x + 1

r = accumulate(add,inc,is_even,0,0,1000)
print(1 if r == 249500 else 0)