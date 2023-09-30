def mul(x:int,y:int) -> int:
    return mul_iter(x,y,0)

def mul_iter(x:int,y:int,r:int) -> int:
    return r if y == 0 else mul_iter(x,y-1,r+x)

print(mul(10,100000))