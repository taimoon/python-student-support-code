def mul_iter(x:int,y:int,r:int) -> int:
    return r if y == 0 else mul_iter(x,y-1,r+x)

def mul(x:int,y:int) -> int:
    return mul_iter(x,y,0) if y >= 0 else -mul_iter(x,-y,0)

def abs(x:int) -> int:
    return -x if not (x >= 0) else x

def euclid_div_iter(x:int,d:int,m:int) -> tuple[int,int]:
    return euclid_div_iter(x-d,d,m+1) if (d != 0 and x-d >= 0) else (m,x)

def euclid_div(x:int,d:int) -> tuple[int,int]:
    return euclid_div_iter(x,d,0)

def remainder(x:int,d:int) -> int:
    return euclid_div(abs(x),abs(d))[1]

def div(x:int,d:int) -> int:
    return euclid_div(x,d)[0] if d >= 0 else -euclid_div(x,-d)[0]

def gcd(a:int,b:int) -> int:
    if b < 0:
        return -gcd(a,abs(b))
    elif a < 0:
        return -gcd(abs(a),b)
    else:
        return gcd(b,remainder(a,b)) if b != 0 else a

def rational(n:int,d:int) -> tuple[int,int]:
    g = gcd(n,d)
    return div(n,g),div(d,g)

def numer(r:tuple[int,int]) -> int:
    return r[0]

def denom(r:tuple[int,int]) -> int:
    return r[1]

def equiv_rat(r1:tuple[int,int],r2:tuple[int,int]) -> bool:
    return mul(numer(r1),denom(r2)) == mul(numer(r2),denom(r1))

def add_rat(r1:tuple[int,int],r2:tuple[int,int]) -> tuple[int,int]:
    return rational(mul(numer(r1),denom(r2))
                    +mul(numer(r2),denom(r1)),
                    mul(denom(r1),denom(r2)))

def mul_rat(r1:tuple[int,int],r2:tuple[int,int]) -> tuple[int,int]:
    return rational(mul(numer(r1),numer(r2)),
                    mul(denom(r1),denom(r2)))

def sub_rat(r1:tuple[int,int],r2:tuple[int,int]) -> tuple[int,int]:
    return add_rat(r1,rational(-numer(r2),denom(r2)))

def inverse_rat(r: tuple[int,int]) -> tuple[int,int]:
    return rational(denom(r),numer(r))

def div_rat(r1: tuple[int,int], r2: tuple[int,int]) -> tuple[int,int]:
    return mul_rat(r1,inverse_rat(r2))

r = True
r1 = rational(2,4)
r2 = rational(4,8)
r3 = rational(7,7)
r = r and equiv_rat(add_rat(r1,r2),add_rat(r2,r1))
r = r and equiv_rat(add_rat(r1,r2),r3)
r = r and equiv_rat(r3,rational(1,1))
r = r and equiv_rat(mul_rat(r1,r2),rational(1,4))
r = r and equiv_rat(sub_rat(r1,r1),rational(0,1))
r = r and equiv_rat(sub_rat(rational(7,1),rational(29,7)),rational(20,7))
r = r and equiv_rat(div_rat(r1,r2),rational(1,1))
r = r and equiv_rat(div_rat(rational(11,13),rational(7,1)),rational(11,91))
print(1 if r else 0)
