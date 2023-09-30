def accumulate(op:Callable[[int,int],int],
               nxt:Callable[[int],int],
               f:Callable[[int],int],
               pred:Callable[[int],bool],
               init:int,
               a:int,
               b:int) -> int:
    
    while a < b:
        init = op(init,f(a)) if pred(f(a)) else init
        a = nxt(a)
    return init

def mul(a:int,b:int) -> int:
    init = 0
    i = 0
    while i < b:
        init = init + a
        i = i+1
    return init

def remainder(x:int,d:int) -> int:
    return remainder(x+(-d),d) if x+(-d) >= 0 else x

def abs(x:int) -> int:
    return x if x >= 0 else (-x)

def smallest_divisor_iter(x:int,d:int) -> int:
    if remainder(x,d) == 0:
        return d
    elif mul(d,d) > x:
        return x
    else:
        return smallest_divisor_iter(x,d+1)

def smallest_divisor(x:int) -> int:
    return smallest_divisor_iter(abs(x),2)

def is_prime(p:int) -> bool:
    return smallest_divisor(p) == p

def add(a:int,b:int) -> int:
    return a+b

def inc(x:int) -> int:
    return x + 1

def identity(x:int) -> int:
    return x

def prime_sum(n:int) -> int:
    return accumulate(add,inc,identity,is_prime,0,2,n)

print(prime_sum(32768))