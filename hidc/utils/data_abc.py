import abc
import typing as ty

T = ty.TypeVar('T')
abstract_mark = object()
Abstract = ty.Annotated[T, abstract_mark]


class DataABCMeta(abc.ABCMeta):
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

    # This __getattr__ SHOULDN'T be necessary!
    # However, dataclasses call abc.update_abstractmethods, which strips
    # out all entries from __abstractmethods__, and only adds them back
    # in if there is an attribute on the class with __isabstractmethod__
    # set to True.  There is no attribute for my Abstract annotations,
    # so they don't get added back in.
    # Not having an actual attribute for the Abstract data fields is the
    # main goal of DataABC, and it is impossible to define attributes on
    # a superclass that get deleted from a subclass, so our only option
    # is to define a __getattr__
    def __getattr__(cls, name):
        try:
            abstract = super().__getattribute__('__abstractmethods__')
        except AttributeError: pass
        else:
            if name in abstract:
                # Fake abstract property for abc.update_abstractmethods
                return property(abc.abstractmethod(lambda self: None))

                # Brief aside: although you can create a property() that
                # lacks fget simply by calling it with no arguments, you
                # cannot so easily create an abstract property without
                # fget.  There's abc.abstractproperty(), but that's
                # deprecated.  So we create a getter that returns None,
                # although it's not quite the same it doesn't matter
                # much since it should never be called anyway.
                # Still, the fget=None behavior of raising an exception
                # would be arguably better here.  Of course in the space
                # I took to write this comment I could have simply done
                # that, but it's more fun to grumble about it.
        return super().__getattribute__(name)



class DataABC(metaclass=DataABCMeta):
    __slots__ = ()
