def lazy_list(iterable):
    return IteratorNode(iterable).solidify()


class IteratorNode:
    def __init__(self, iterable):
        self._iterator = iter(iterable)

    def solidify(self):
        try:
            return Cons(next(self._iterator), self)
        except StopIteration as ret:
            return Nil(ret.value)

    def __repr__(self):
        return '...'

    def __bool__(self):
        raise ValueError('IteratorNode must be solidified')



class Cons:
    __slots__ = 'head', '_tail'
    __match_args__ = 'head', 'tail'

    def __init__(self, head, tail):
        self.head = head
        self._tail = tail

    def solidify(self):
        return self

    @property
    def tail(self):
        if isinstance(self._tail, IteratorNode):
            self._tail = self._tail.solidify()
        return self._tail

    def __repr__(self):
        return f'Cons({self.head!r}, {self._tail})'

    def __bool__(self):
        return True

    # Indirectly implemented as static method because it seems bound
    # generators retain a reference to self even if you reassign it, so
    # nodes would not get GC'd even if they are no longer needed.
    @staticmethod
    def _iter(ll):
        while ll:
            yield ll.head
            ll = ll.tail
        return ll.value

    def __iter__(self):
        return self._iter(self)



class Nil:
    # Nil's value represents the return value of generators
    # We're using it for the end position of the last token
    def __init__(self, value=None):
        self.value = value

    def solidify(self):
        return self

    def __repr__(self):
        return f'Nil({self.value})'

    def __bool__(self):
        return False

    def __iter__(self):
        yield from ()
        return self.value
