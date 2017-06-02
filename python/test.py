

import functools


@functools.lru_cache(maxsize=4)
def fib(n):
    if n < 2:
        return n

    return fib(n-1) + fib(n-2)


def fib2(n):
    if n < 2:
        return n

    return fib2(n-1) + fib2(n-2)

