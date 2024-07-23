import random
import weakref
from dataclasses import dataclass
from contextlib import contextmanager

objects = {}
functions = {}
frame_stack = [{"": "<static>"}]
globalvars = {}
string_constants = {}
attrs = weakref.WeakKeyDictionary()

def intern_string_constant(arg):
    try:
        return string_constants[arg]
    except KeyError:
        pass
    data = arg.encode("utf8") + b"\0"
    objects[(addr := alloc(data)) >> 32][:] = data
    string_constants[arg] = addr
    return addr

@contextmanager
def in_frame(init={}):
    frame_stack.append({**init})
    try:
        yield frame_stack[-1]
    finally:    
        for obj in frame_stack.pop().values():
            if hasattr(obj, "at"):
                free(obj.at.value)

def set_at(pointer, data):
    oid = pointer >> 32
    offs = pointer & 0xFFFFFFFF
    if oid not in objects:
        raise InterpreterError(f"invalid pointer 0x{pointer:016x} (nonexistent object {oid})")
    if len(objects[oid][offs:offs+len(data)]) != len(data):
        raise InterpreterError(f"invalid pointer 0x{pointer:016x} (range {offs}-{offs+len(data)} out of bounds)")
    objects[oid][offs:offs+len(data)] = data

def get_at(pointer, width):
    oid = pointer >> 32
    offs = pointer & 0xFFFFFFFF
    if oid not in objects:
        raise InterpreterError(f"invalid pointer 0x{pointer:016x} (nonexistent object)")
    if len(objects[oid][offs:offs+width]) != width:
        raise InterpreterError(f"invalid pointer 0x{pointer:016x} (range {offs}-{offs+width} out of bounds)")
    return objects[oid][offs:offs+width]

def var(name):
    for vardict in reversed(frame_stack):
        if name in vardict:
            return vardict[name].ensure_reduced()
        if "" in vardict:
            break
    try:
        return globalvars[name].ensure_reduced()
    except KeyError:
        raise InterpreterError(f"no variable named {name}") from None

def alloc(n):
    # lmao worst code
    while True:
        idx = random.randint(1, (1<<32)-1)
        if idx not in objects:
            objects[idx] = bytearray(n)
            return idx << 32

def free(addr):
    if addr & 0xFFFFFFFF:
        raise InterpreterError("free pointer with offset")
    del objects[addr >> 32]

class InterpreterError(Exception):

    def __init__(self, *args):
        global last_node
        self.at = "\n".join(f"    at {frame['']} ({frame['?'].file}:{frame['?'].line}:{frame['?'].column})" for frame in reversed(frame_stack) if "" in frame)
        self.msg = str(args[0]) if len(args) == 1 else repr(args)

    def __str__(self):
        return f"{self.msg}\n{self.at}"
