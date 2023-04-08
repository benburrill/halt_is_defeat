import dataclasses

# Helpers to add fields to dataclasses that are backed by properties
# This is a bit goofy and has the drawback that the init method that it
# creates has the wrong keyword argument.  But apart from that it seems
# to work alright

def property_field(descriptor):
    return dataclasses.field(init=False, metadata={'prop': descriptor})

def add_properties(cls):
    for field in dataclasses.fields(cls):
        if prop := field.metadata.get('prop'):
            assert not hasattr(cls, field.name)
            setattr(cls, field.name, prop)
    return cls

def internal_field(**kwargs):
    return dataclasses.field(repr=False, compare=False, **kwargs)
