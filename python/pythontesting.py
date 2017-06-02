from math import sqrt


def fib():
    a, b = 0, 1
    while 1:
        yield a
        a, b = b, a+b


def fib_n(max_n=0):
    n, a, b = 0, 0, 1
    while max_n and n >= max_n:
        yield a
        a, b = b, a+b
        n += 1


def primes():
    yield 2
    yield 3
    # print("delivered first")
    current = 3
    got = [2, 3]
#    nmax = sqrt(current) + 1
    while 1:
        for i in range(0, current + 2, 2):
            divided = False
            for n in got:
                if (current + i) % n == 0:
                    divided = True
                    # print(current + i, "divisible by", n, "!")
            if not divided:
                current += i
                got.append(current)
                yield current
                # print("delivered " + str(current))
                break


def drop(amount, it):
    g = iter(it)
    for i, e in zip(g, range(amount - 1)):
        pass
    yield from g


def take(amount, it):
    g = iter(it)
    for i, e in zip(g, range(amount)):
        yield i


class FibIterator():
    def __init__(self, max_n=0):
        self.max_n = max_n
        self.n, self.a, self.b = 0, 0, 1

    def __iter__(self):
        return self

    def __next__(self):
        self.n += 1
        self.a, self.b = self.b, self.a + self.b
        if not self.max_n or self.n <= self.max_n:
            return self.a
        else:
            raise StopIteration



def iter_f(f, a_begin=0, b_begin=1, max_n = 0):
    n = 0
    while not max_n or n <= max_n:
        yield a_begin
        a_begin, b_begin = f(a_begin, b_begin)

fib_3 = iter_f(lambda x, y: (y, x + y))


def toInf():
    yield 2
    a = 3
    while True:
        yield a
        a += 2


def sieve():
    """ (fast?) Implementation of the sieve of Erastoteles in Python,
    using Laziness and a dictionary of items already used.
    """
    table = {}
    def sieve2(lst):
        a = next(lst, None)
        while a:
            if a not in table:
                table[a * a] = a
                yield a
            else:
                table[a + table.get(a)] = table[a]
                del(table[a])
            a = next(lst, None)
    yield from sieve2(iter(toInf()))







