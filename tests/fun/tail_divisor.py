def mul(a:int,b:int) -> int:
    init = 0
    i = 0
    while i < b:
        init = init + a
        i = i+1
    return init


def remainder(x:int,d:int) -> int:
    return remainder(x+(-d),d) if x+(-d) >= 0 else x

def smallest_divisor_iter(x:int,d:int) -> int:
    if remainder(x,d) == 0:
        return d
    elif mul(d,d) > x:
        return x
    else:
        return smallest_divisor_iter(x,d+1)

def smallest_divisor(x:int) -> int:
    return smallest_divisor_iter(x,2)

print(smallest_divisor(23))