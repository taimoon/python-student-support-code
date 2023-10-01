def mul(x:int,y:int) -> int:
    return mul_iter(x,y,0)

def mul_iter(x:int,y:int,r:int) -> int:
    return r if y == 0 else mul_iter(x,y-1,r+x)

def remainder(x:int,d:int) -> int:
    return remainder(x-d,d) if x-d >= 0 else x

def smallest_divisor_iter(x:int,d:int) -> int:
    if remainder(x,d) == 0:
        return d
    elif mul(d,d) > x:
        return x
    else:
        return smallest_divisor_iter(x,d+1)

def smallest_divisor(x:int) -> int:
    return smallest_divisor_iter(x,2)

p = 16777213
print(1 if smallest_divisor(p) == p else 0)