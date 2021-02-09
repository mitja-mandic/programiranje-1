from functools import lru_cache

@lru_cache(maxsize=None)
def fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n-1) + fib(n-2)

def fibo(n):
    a = 0
    b = 1
    c = a+b
    for i in range(2,n):
        a = b
        b = c
        c = a+b
        i +=1
    return c        