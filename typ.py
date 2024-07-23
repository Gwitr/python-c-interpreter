import struct

import pycparser

import base

class TypedValue:

    def __init__(self, typ, blob_or_ptr, reduce=True):
        self.reduced = reduce
        if reduce:
            self.type, self.blob_or_ptr = typ.reduce(blob_or_ptr)
        else:
            self.type, self.blob_or_ptr = typ, blob_or_ptr

    def ensure_reduced(self):
        if self.reduced:
            return self
        return TypedValue(self.type, self.blob_or_ptr)

    @property
    def is_lvalue(self):
        return isinstance(self.blob_or_ptr, int)

    @property
    def raw(self):
        return base.get_at(self.blob_or_ptr, self.type.width) if self.is_lvalue else self.blob_or_ptr

    @raw.setter
    def raw(self, value):
        if not self.is_lvalue:
            raise base.InterpreterError("assign to rvalue")
        base.set_at(self.blob_or_ptr, value)

    def __repr__(self):
        try:
            return f"({self.type!r}){self.value!r}"
        except base.InterpreterError:
            return f"({self.type!r})???"

    @property
    def value(self):
        return self.type.value(self.raw)

    def cast(self, typ, implicit=False):
        return typ.cast(self, implicit)

    def __getitem__(self, index):
        if isinstance(index, int):
            return self.type.deref((self + LONG_LONG.conv(index)).raw)
        return self.type.deref((self + index).raw)

    def __setitem__(self, index, value):
        if isinstance(index, int):
            index = LONG_LONG.conv(index)
        target = self.type.deref((self + index).raw)
        target.raw = target.type.cast(value, implicit=True).raw

    @property
    def at(self):
        if self.is_lvalue:
            return INTPTR_T.conv(self.blob_or_ptr).cast(PointerType(self.type))
        raise base.InterpreterError("take address of rvalue")

    def arth(self, other, castfunc, forbidden, op):
        left, right = self, other
        if isinstance(left, forbidden) or isinstance(right, forbidden):
            raise base.InterpreterError(f"arithmetic operation between {left.type!r} and {right.type!r}")
        left, right, outtype = arithmetic_cast(left, right)
        return outtype.conv(op(left, right))

    def __and__(self, other):
        return self.arth(other, arithmetic_cast, (PointerType, FloatingPointType), lambda x, y: x.value & y.value)

    def __add__(self, other):
        left, right = self, other
        if isinstance(right.type, PointerType):
            right, left = left, right
        if isinstance(left.type, PointerType):
            return left.arth(right, arithmetic_cast, (), lambda x, y: x.value + y.value * x.type.target_type.width)
        return left.arth(right, arithmetic_cast, (), lambda x, y: x.value + y.value)

    def __sub__(self, other):
        if isinstance(other.type, PointerType):
            if isinstance(self.type, PointerType):
                return PTRDIFF_T.conv(self.value - other.value)
            raise base.InterpreterError(f"arithmetic operation between {self.type!r} and {other.type!r}")
        if isinstance(self.type, PointerType):
            return self.arth(other, arithmetic_cast, (), lambda x, y: x.value - y.value * x.type.target_type.width)
        return self.arth(other, arithmetic_cast, (), lambda x, y: x.value - y.value)

    def __mul__(self, other):
        return self.arth(other, arithmetic_cast, (PointerType,), lambda x, y: x.value * y.value)

    def __truediv__(self, other):
        return self.arth(other, arithmetic_cast, (PointerType,), lambda x, y: x.value / y.value)  # TODO: have .conv do coercion

    def __mod__(self, other):
        return self.arth(other, arithmetic_cast, (PointerType, FloatingPointType), lambda x, y: x.value % y.value)

    def __eq__(self, other):
        return self.arth(other, comparison_cast, (), lambda x, y: x.value == y.value)

    def __lt__(self, other):
        return self.arth(other, comparison_cast, (), lambda x, y: x.value < y.value)
    
    def __le__(self, other):
        return self.arth(other, comparison_cast, (), lambda x, y: x.value <= y.value)

    def __gt__(self, other):
        return self.arth(other, comparison_cast, (), lambda x, y: x.value > y.value)

    def __ge__(self, other):
        return self.arth(other, comparison_cast, (), lambda x, y: x.value >= y.value)

    def __neg__(self):
        if isinstance(self.type, (FloatingPointType, IntegralType)):
            return self.type.conv(-self.value)
        raise base.InterpreterError(f"arithmetic operation on {self.type!r}")

    def __call__(self, *args):
        return self.type.call(self.raw, *args)

class CType:
    name: str

    def __repr__(self):
        return self.typename().replace("$", "").strip()

    @property
    def width(self):
        raise base.InterpreterError(f"{self!r} does not have a width")

    def typename(self):
        return "?"

    def cast(self, value, implicit=False):
        raise base.InterpreterError(f"can't {'implicitly '*implicit}cast {value.type!r} to {self!r}")

    def value(self, data):
        raise base.InterpreterError(f"value not implemented for type {self!r}")

    def call(self, data, *args):
        raise base.InterpreterError(f"can't call {self!r}")

    def deref(self):
        raise base.InterpreterError(f"can't deref {self!r}")

    def reduce(self, raw):
        return self, raw

class IntegralType(CType):
    LENGTH_TO_WIDTH = {-2: 1, -1: 2, 0: 4, 1: 4, 2: 8}
    WIDTH_TO_LENGTH = {1: -2, 2: -1, 4: 0, 8: 2}

    def __init__(self, signed, length):
        self.signed = signed
        self.length = length

    def with_name(self, name):
        pt = IntegralType(self.signed, self.length)
        pt.name = name
        return pt

    def __eq__(self, other):
        if not isinstance(other, IntegralType):
            return NotImplemented
        return (self.signed, self.length) == (other.signed, other.length)

    def conv(self, value):
        value = int(value)
        width = self.LENGTH_TO_WIDTH[self.length]
        offs = (1 << width * 8 - 1) if self.signed else 0
        wrapped = ((value + offs) & ((1 << width * 8) - 1))
        return TypedValue(self, (wrapped - offs * self.signed).to_bytes(width, "little", signed=self.signed))

    def typename(self):
        if self.signed and self.length == 0:
            return "int $"
        if self.length == -2:
            return "unsigned " * (not self.signed) + "char $"
        return "unsigned " * (not self.signed) + "short " * -self.length + "long " * self.length + "$"

    @property
    def width(self):
        return self.LENGTH_TO_WIDTH[self.length]

    def value(self, data):
        return int.from_bytes(data, "little", signed=self.signed)

    def cast(self, value, implicit=False):
        if isinstance(value.type, PointerType) and not implicit:
            if self.signed:
                raise base.InterpreterError("cast pointer to signed integer type")
            if self.LENGTH_TO_WIDTH[self.length] != 8:
                raise base.InterpreterError("cast pointer to integer type of wrong width")
            return TypedValue(self, value.raw)
        if isinstance(value.type, IntegralType):
            width = self.LENGTH_TO_WIDTH[self.length]
            offs = (1 << width * 8 - 1) if self.signed else 0
            wrapped = ((value.value + offs) & ((1 << width * 8) - 1))
            return TypedValue(self, (wrapped - offs * self.signed).to_bytes(width, "little", signed=self.signed))
        super().cast(value, implicit)

CHAR                           = INT8_T    = IntegralType(True,  -2)
SHORT                          = INT16_T   = IntegralType(True,  -1)
INT                            = INT32_T   = IntegralType(True,   0)
LONG                                       = IntegralType(True,   1)
LONG_LONG          = PTRDIFF_T = INT64_T   = IntegralType(True,   2)
UNSIGNED_CHAR                  = UINT8_T   = IntegralType(False, -2)
UNSIGNED_SHORT                 = UINT16_T  = IntegralType(False, -1)
UNSIGNED_INT                   = UINT32_T  = IntegralType(False,  0)
UNSIGNED_LONG                              = IntegralType(False,  1)
UNSIGNED_LONG_LONG = INTPTR_T  = UINT64_T  = IntegralType(False,  2)

class PointerType(CType):

    def __init__(self, target_type):
        if not isinstance(target_type, CType):
            raise TypeError(f"argument 1 must be CType, got {type(target_type).__qualname__}")
        self.target_type = target_type

    def with_name(self, name):
        pt = PointerType(self.target_type)
        pt.name = name
        return pt

    def __eq__(self, other):
        if not isinstance(other, PointerType):
            return NotImplemented
        return self.target_type == other.target_type

    def typename(self):
        return (self.target_type.typename() if self.target_type else "void $").replace("$", "(*$)" if isinstance(self.target_type, FunctionType) else "*$")

    def cast(self, value, implicit=False):
        if isinstance(value.type, PointerType):
            return TypedValue(self, value.raw)
        if isinstance(value.type, IntegralType) and not implicit:
            if value.value == 0:
                return TypedValue(self, b"\0"*8)
            if value.type.signed:
                raise base.InterpreterError("cast signed integer to pointer")
            if value.type.width != 8:
                raise base.InterpreterError("cast integer type of wrong width to pointer")
            return TypedValue(self, value.raw)
        super().cast(value, implicit)

    def call(self, data, *args):
        if isinstance(self.target_type, FunctionType):
            import ceval
            return ceval.call(base.functions[self.value(data)], args)

        super().call(data, *args)

    def value(self, raw):
        return int.from_bytes(raw, "little")

    def conv(self, value):
        return TypedValue(self, int(value).to_bytes(8, "little"))

    @property
    def width(self):
        return 8

    def deref(self, data):
        if self.target_type == VoidType():
            raise base.InterpreterError(f"dereference void pointer")
        if isinstance(self.target_type, FunctionType):
            return self
        return TypedValue(self.target_type, self.value(data))

class ArrayType(CType):

    def __init__(self, target_type, n):
        self.target_type = target_type
        self.n = n

    def with_name(self, name):
        pt = ArrayType(self.target_type, self.n)
        pt.name = name
        return pt

    def __eq__(self, other):
        if not isinstance(other, ArrayType):
            return NotImplemented
        return (self.target_type, self.n) == (other.target_type, other.n)

    def typename(self):
        return self.target_type.typename().replace("$", f"$[{self.n}]").strip()

    @property
    def width(self):
        return self.target_type.width * self.n

    def reduce(self, raw):
        if not isinstance(raw, int):
            raise base.InterpreterError("reducing an rvalue array? somehow?")
        return PointerType(self.target_type), raw.to_bytes(8, "little")

class FunctionType(CType):

    def __init__(self, rtype, argtypes, variadic):
        self.rtype = rtype
        self.argtypes = argtypes
        self.variadic = variadic

    def with_name(self, name):
        pt = FunctionType(self.rtype, self.argtypes, self.variadic)
        pt.name = name
        return pt

    def __eq__(self, other):
        if not isinstance(other, FunctionType):
            return NotImplemented
        return (self.rtype, self.argtypes) == (other.rtype, other.argtypes)

    def typename(self):
        return (self.rtype.typename() if self.rtype else "void $").replace("$", "$(" + ", ".join(repr(i) for i in self.argtypes) + ")")

    def reduce(self, raw):
        if not isinstance(raw, int):
            raise base.InterpreterError("reducing an rvalue function? somehow?")
        return PointerType(self), raw.to_bytes(8, "little")

structs = None

def new_struct_ctx():
    global structs
    structs = {}

def make_struct(tag, fields):
    if tag in structs:
        if fields is not None:
            if structs[tag].fields is not None:
                raise base.InterpreterError(f"redefinition of struct {tag}")
            structs[tag].fields = fields
        return structs[tag]
    structs[tag] = StructType(tag, fields)
    return structs[tag]

class StructType(CType):

    def __init__(self, tag, fields):
        self.tag = tag
        self.fields = fields

    def with_name(self, name):
        pt = StructType(self.tag, self.fields)
        pt.name = name
        return pt

    def __eq__(self, other):
        if not isinstance(other, StructType):
            return NotImplemented
        return (self.tag,) == (other.tag,)

    def get_field_type(self, name):
        for fname, ftype in self.fields:
            if fname == name:
                return ftype
        raise base.InterpreterError(f"struct {self.tag} has no field {name}")

    def get_field_offs(self, name):
        offs = 0
        for fname, ftype in self.fields:
            if fname == name:
                return offs
            offs += ftype.width
        raise base.InterpreterError(f"struct {self.tag} has no field {name}")

    def typename(self):
        return f"struct {self.tag} $"

    def dump(self):
        if self.fields is None:
            return f"struct {self.tag}"
        return f"struct {self.tag} {{ " + "".join(ftype.typename().replace("$", fname).strip() + "; " for fname, ftype in self.fields) + "} $"

    @property
    def width(self):
        return sum(typ.width for _, typ in self.fields)

class FloatingPointType(CType):

    def __init__(self, short):
        self.short = short

    def with_name(self, name):
        pt = FloatingPointType(self.short)
        pt.name = name
        return pt

    def __eq__(self, other):
        if not isinstance(other, FloatingPointType):
            return NotImplemented
        return self.short == other.short

    def conv(self, value):
        return TypedValue(self, struct.pack("<f" if self.short else "<d", float(value)))

    def typename(self):
        return "float $" if self.short else "double $"

    @property
    def width(self):
        return 4 if self.short else 8

    def value(self, data):
        return struct.unpack("<f" if self.short else "<d", data)[0]

    def cast(self, value, implicit=False):
        if isinstance(value.type, FloatingPointType):
            return self(value.value)
        if isinstance(value.type, IntegralType):
            return self(value.value)
        super().cast(value, implicit)

class VoidType(CType):

    def with_name(self, name):
        pt = VoidType()
        pt.name = name
        return pt

    def __eq__(self, other):
        return isinstance(other, VoidType)
    
    def typename(self):
        return "void $"

FLOAT = FloatingPointType(True)
DOUBLE = FloatingPointType(False)

def arithmetic_cast(left, right):
    if not isinstance(left.type, (FloatingPointType, IntegralType, PointerType)):
        raise base.InterpreterError(f"arithmetic operation on {left.type!r}")
    if not isinstance(right.type, (FloatingPointType, IntegralType, PointerType)):
        raise base.InterpreterError(f"arithmetic operation on {right.type!r}")

    if isinstance(left.type, FloatingPointType) and isinstance(right.type, FloatingPointType):
        typ1 = typ2 = typ3 = FloatingPointType(left.type.short and right.type.short)
    elif isinstance(left.type, IntegralType) and isinstance(right.type, IntegralType):
        typ1 = typ2 = typ3 = IntegralType(left.type.signed or right.type.signed, max(left.type.length, right.type.length))
    elif isinstance(left.type, FloatingPointType) and isinstance(right.type, IntegralType):
        typ1 = typ2 = typ3 = left.type
    elif isinstance(left.type, IntegralType) and isinstance(right.type, FloatingPointType):
        typ1 = typ2 = typ3 = right.type
    elif (isinstance(left.type, PointerType) and isinstance(right.type, IntegralType)) or (isinstance(left.type, IntegralType) and isinstance(right.type, PointerType)):
        typ1 = left.type
        typ2 = right.type
        typ3 = left.type if isinstance(left.type, PointerType) else right.type
    else:
        raise base.InterpreterError(f"arithmetic operation between {left.type!r} and {right.type!r}")

    return left.cast(typ1, implicit=True), right.cast(typ2, implicit=True), typ3

def comparison_cast(left, right):
    if isinstance(left.type, PointerType) and isinstance(right.type, PointerType):
        return left, right, INT
    try:
        return *arithmetic_cast(left, right)[:2], INT
    except base.InterpreterError as e:
        raise base.InterpreterError(f"comparison operation between {left.type!r} and {right.type!r}") from None
