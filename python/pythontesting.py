


def fib():
    a, b = 0, 1
    while 1:
        yield a
        a, b = b, a+b


def take(amount, it):
    for i, e in zip(it(), range(amount)):
        yield i



