import dataclasses as dc
import bisect
from collections import deque
from itertools import repeat

from .asm import DynamicValue

# TODO: I might want to combine this together with the ChainMap I'm
#  using for the local variables, and maybe some other stuff too, to get
#  some of the block-scope related stuff out of the CodeGen class and
#  together in one place.

@dc.dataclass
class Tracker:
    max_vals: list = dc.field(init=False, default_factory=list)
    levels: deque[deque[tuple[int, DynamicValue]]] = dc.field(init=False)

    def __post_init__(self):
        self.levels = deque([deque()])

    def add(self, cur_val):
        dyn_val = DynamicValue()
        idx = bisect.bisect_right(self.max_vals, cur_val)
        self.levels[-1].append((idx, dyn_val))
        self.max_vals.insert(idx, cur_val)
        return dyn_val

    def pop_level(self):
        for idx, dyn_val in reversed(self.levels.pop()):
            dyn_val.finalize(self.max_vals.pop(idx))

        if not self.levels:
            self.push_level()

    def push_level(self):
        self.levels.append(deque())

    def update(self, new_max):
        num_less = bisect.bisect_left(self.max_vals, new_max)
        self.max_vals[:num_less] = repeat(new_max, num_less)
