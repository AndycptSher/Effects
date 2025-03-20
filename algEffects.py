from typing import Any, Generator


store = 0


class Get:
    def __init__(self, key: int):
        self.key = key
        pass

class Set:
    def __init__(self, key: int, value: str):
        self.key = key
        self.value = value


def replace(key: int, value: str):
    temp: str = yield Get(key)
    yield Set(key, value)
    return temp

# def chain(op1, op2):
#     try:
#         temp = yield next(op1)
#         while True:
#             temp = yield op1.send(temp)
#     except StopIteration:
#         pass
#     print("stopped after first in chain")
#     temp = yield next(op2)
#     while True:
#         temp = yield op2.send(temp)

def getSetHandler(thing):
    global store
    value = next(thing)
    try:
        while True:
            match (value):
                case Get(key=_):
                    value = thing.send(store)
                    pass
                case Set(key=_, value=val):
                    store = val
                    value = next(thing)
                    pass
                case x:
                    pass
    except StopIteration as e:
        return e.value
    

mapping = {}
def getSetHandler1(thing):
    global mapping
    value = next(thing)
    try:
        while True:
            match value:
                case Get(key = key):
                    value = thing.send(mapping.get(key, ""))
                case Set(key=key, value = val):
                    mapping[key] = val
                    value = next(thing)
                case x:
                    pass
    except StopIteration as e:
        return e.value

def handle(handler, free: Generator):
    pass

def handleState(handler, free: Generator):
    def _(p):
        pass
    return _


def main():
    hdlr: Generator[Get| Set, Any, str] = getSetHandler


    val = hdlr(replace(2, "eeeee"))
    print(val)
    val1 = hdlr(replace(0, "aeeee"))
    print(val1)
    # def get(key):
    #     return (yield Get(key))

    def get(key):
        val = yield from replace(key, "")
        yield from replace(key, val)
        return val

    last = hdlr(get(0))
    print(last)

    def set(key, value):
        yield from replace(key, value)

    setVal = hdlr(set(0, "e"))
    print(setVal)

    print(hdlr(get(0)))


if __name__ == "__main__":
    print("main output", main())
    pass


# def foo():
#     for i in range(5):
#         yield i
#     return 5 # raise StopIteration(5)

