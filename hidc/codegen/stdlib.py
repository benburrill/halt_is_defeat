import textwrap
from . import asm
from .symbols import ConcreteSignature, DataType, ConcreteArrayType, Ident, AccessMode

# Terminal non-termination labels
# These never return, so may be jumped to directly
# all_is_win and all_is_broken also happen to be exposed as functions
all_is_win = asm.LabelRef('all_is_win')
all_is_broken = asm.LabelRef('all_is_broken')
out_of_bounds = asm.LabelRef('out_of_bounds')
stack_overflow = asm.LabelRef('stack_overflow')
division_by_zero = asm.LabelRef('division_by_zero')
nonlocal_preempt = asm.LabelRef('nonlocal_preempt')
halt = asm.LabelRef('halt')

# NOTE:
# !is_defeat(), !truth_is_defeat(bool), write(byte), and writeln(...)
# are not implemented here should all be inlined.
# The stdlib also expects registers r0, r1, r2, and fp
# I may consider moving them + their state definitions here.
stdlib_funcs = {
    # empty all_is_win()
    ConcreteSignature(Ident('all_is_win'), ()): all_is_win,
    # empty all_is_broken()
    ConcreteSignature(Ident('all_is_broken'), ()): all_is_broken,
    # empty write(const byte[] s)
    ConcreteSignature(Ident('write'), (
        ConcreteArrayType(DataType.BYTE, AccessMode.RC),
    )): asm.LabelRef('write_const_byte_array'),
    ConcreteSignature(Ident('write'), (
        ConcreteArrayType(DataType.BYTE, AccessMode.R),
    )): asm.LabelRef('write_state_byte_array'),
    # empty write(string s)
    ConcreteSignature(Ident('write'), (DataType.STRING,)): asm.LabelRef('write_string'),
    # empty write(bool b)
    ConcreteSignature(Ident('write'), (DataType.BOOL,)): asm.LabelRef('write_bool'),
    # empty write(int i)
    ConcreteSignature(Ident('write'), (DataType.INT,)): asm.LabelRef('write_int')
}

abstract_funcs = {}
for _csig in stdlib_funcs:
    abstract_funcs.setdefault(_csig.name, set()).add(_csig.abstract_params)


# an interesting idea: we might be able to skip halt propagation in some
# places in the stdlib write routines (except in cases where the halt
# propagation prevents an infinite loop), since it doesn't actually
# matter if the wrong thing gets written in a branch where defeat is
# inevitable.  But it's probably a pointless optimization.

# TODO: test for stack overflow in write_int,
#  May need up to 4 words of stack space: 1 for RA + 3 for buffer.
#  Also probably restructure write loops to make most jumps look only
#  lookahead locally.

stdlib_lines = list(filter(None, textwrap.dedent("""
    all_is_win:
        flag win
        tnt: sleep 0x7f7f
        j tnt
        halt: halt
    all_is_broken:
        flag error
        j tnt
        halt
    stack_overflow:
        flag stack_overflow
        j all_is_broken
        halt
    division_by_zero:
        flag division_by_zero
        j all_is_broken
        halt
    out_of_bounds:
        flag out_of_bounds
        j all_is_broken
        halt
    nonlocal_preempt:
        flag nonlocal_preempt
        j all_is_broken
        halt


    write_const_byte_array:
        lwso [r0], [fp], -3w
        lwso [r1], [fp], -2w
        j write_string_loop
        hgt [r1], 0
        j write_string_done
        halt
    write_string:
        lwso [r0], [fp], -2w
        lwc [r1], [r0]
        add [r0], [r0], 1w
        j write_string_loop
        hgt [r1], 0
        j write_string_done
        halt
        write_string_loop:
        hle [r1], 0
            lbc [r2], [r0]
            yield [r2]
            add [r0], [r0], 1
            sub [r1], [r1], 1
        j write_string_loop
        hgt [r1], 0
        write_string_done:
        lwso [r0], [fp], -1w
        j [r0]
        halt


    write_state_byte_array:
        lwso [r0], [fp], -3w
        lwso [r1], [fp], -2w
        j write_state_byte_array_loop
        hgt [r1], 0
        j write_state_byte_array_done
        halt
        write_state_byte_array_loop:
        hle [r1], 0
            lbs [r2], [r0]
            yield [r2]
            add [r0], [r0], 1
            sub [r1], [r1], 1
        j write_state_byte_array_loop
        hgt [r1], 0
        write_state_byte_array_done:
        lwso [r0], [fp], -1w
        j [r0]
        halt


    write_bool:
        lbso [r0], [fp], -1w - 1
        j write_bool_is_true
        hne [r0], 0
        yield 'f'
        yield 'a'
        yield 'l'
        yield 's'
        yield 'e'
        lwso [r0], [fp], -1w
        j [r0]
        halt
        write_bool_is_true:
        heq [r0], 0
        yield 't'
        yield 'r'
        yield 'u'
        yield 'e'
        lwso [r0], [fp], -1w
        j [r0]
        halt


    write_int:
        add [r0], [fp], -1w
        lwso [r2], [fp], -2w
        j write_int_pos
        hge [r2], 0
        yield '-'
        sub [r2], 0, [r2]
        j write_int_pos
        hge [r2], 0
        ; Deal with the special case of the minimum signed integer
        sub [r2], [r2], 10
        mod [r1], [r2], 10
        div [r2], [r2], 10
        add [r2], [r2], 1
        j write_int_push
        halt
        write_int_pos:
        hlt [r2], 0

        j write_int_get_digits_body
        halt
        write_int_get_digits:
        heq [r2], 0
        write_int_get_digits_body:
            mod [r1], [r2], 10
            div [r2], [r2], 10
            write_int_push:
            add [r1], [r1], '0'
            sub [r0], [r0], 1
            sbs [r0], [r1]
        j write_int_get_digits
        hne [r2], 0

        sub [r1], [fp], [r0]
        sub [r1], [r1], 1w
        j write_state_byte_array_loop
        hgt [r1], 0
        j write_state_byte_array_done
        halt
""").strip('\n').encode('utf-8').split(b'\n')))
