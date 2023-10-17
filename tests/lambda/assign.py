def g(z : int) -> int:
    x = 0
    y = 0
    f : Callable[[int],int] = lambda a: a + x + z
    x = 10
    y = 12
    return f(y)
print(g(20))