n = 10
res = 1
while n > 0:
    # res = res * n
    c = n
    p = 0
    while c > 0:
        p = p + res
        c = c - 1
    res = p
    n = n - 1
print(res)