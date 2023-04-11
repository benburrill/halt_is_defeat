from abc import ABCMeta
import typing as ty

T = ty.TypeVar('T')
abstract_mark = object()
Abstract = ty.Annotated[T, abstract_mark]


class DataABCMeta(ABCMeta):
    def __new__(mcls, name, bases, namespace, /, **kwargs):
        cls = super().__new__(mcls, name, bases, namespace, **kwargs)
        abstract = cls.__abstractmethods__
        hints = ty.get_type_hints(cls, include_extras=True)

        for field, anno in hints.items():
            if getattr(anno, '__metadata__', None) == (abstract_mark,):
                if not hasattr(cls, field):
                    abstract |= {field}
                elif not getattr(getattr(cls, field), '__isabstractmethod__', False):
                    abstract -= {field}
            elif field in abstract and not hasattr(cls, field):
                abstract -= {field}

        cls.__abstractmethods__ = abstract
        return cls


class DataABC(metaclass=DataABCMeta):
    __slots__ = ()
