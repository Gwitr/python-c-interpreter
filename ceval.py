import ast
import operator

import pycparser

import base
from typ import *

class Return(Exception):

    def __init__(self, value):
        self.value = value

class Continue(Exception):
    pass

class _BoolCast:
    def cast(self, i, implicit=False):
        return coerce_to_bool(i)
BOOL_CAST = _BoolCast()
BINARY = object()
UNARY = object()

BASE_INT_ARITH_CASTS = {
    (IntegralType, IntegralType): lambda x, y: (IntegralType(x.signed or y.signed, max(x.length, y.length)),) * 3
}

BASE_ARITH_CASTS = {
    **BASE_INT_ARITH_CASTS,
    (FloatingPointType, IntegralType):      lambda x, y: (x, x, x),
    (IntegralType, FloatingPointType):      lambda x, y: (y, y, y),
    (FloatingPointType, FloatingPointType): lambda x, y: (FloatingPointType(x.short and y.short),) * 3,
}

BASE_COMPARE_CASTS = {
    (IntegralType, IntegralType):           lambda x, y: (IntegralType(x.signed or y.signed, max(x.length, y.length)),) * 2 + (INT,),
    (FloatingPointType, IntegralType):      lambda x, y: (x, x, INT),
    (IntegralType, FloatingPointType):      lambda x, y: (y, y, INT),
    (FloatingPointType, FloatingPointType): lambda x, y: (FloatingPointType(x.short and y.short),) * 2 + (INT,),
    (PointerType, PointerType):             lambda x, y: (x, y, INT),
}

OP_CASTS = {
    (BINARY, "+"):  {**BASE_ARITH_CASTS, (PointerType, IntegralType): lambda x, y: (x, y, x), (IntegralType, PointerType): lambda x, y: (x, y, y)},
    (BINARY, "-"):  {**BASE_ARITH_CASTS,
        (PointerType, IntegralType): lambda x, y: (x, y, x),
        (PointerType, PointerType):  lambda x, y: (x, y, PTRDIFF_T) if x.target_type == y.target_type else None
    },
    (BINARY, "*"):  BASE_ARITH_CASTS,
    (BINARY, "/"):  BASE_ARITH_CASTS,
    (BINARY, "%"):  BASE_INT_ARITH_CASTS,

    (BINARY, "&"):  BASE_INT_ARITH_CASTS,
    (BINARY, "<<"): BASE_INT_ARITH_CASTS,

    (BINARY, "<"):  BASE_COMPARE_CASTS,
    (BINARY, ">"):  BASE_COMPARE_CASTS,
    (BINARY, "<="): BASE_COMPARE_CASTS,
    (BINARY, ">="): BASE_COMPARE_CASTS,
    (BINARY, "!="): {**BASE_COMPARE_CASTS, (StructType, StructType): lambda x, y: (x, y, INT) if x.tag == y.tag else None},
    (BINARY, "=="): {**BASE_COMPARE_CASTS, (StructType, StructType): lambda x, y: (x, y, INT) if x.tag == y.tag else None},

    (UNARY, "-"):   {(IntegralType,): lambda x: (x, x), (FloatingPointType,): lambda x: (x, x)},

    (BINARY, "||"): {None: lambda x, y: (BOOL_CAST, BOOL_CAST, INT)},
    (BINARY, "&&"): {None: lambda x, y: (BOOL_CAST, BOOL_CAST, INT)},
    (UNARY, "!"):   {None: lambda x:    (BOOL_CAST, INT)},

    (UNARY, "*"):   {(PointerType,): lambda x: (x, x.target_type)},
    (UNARY, "&"):   {None:           lambda x: (None, PointerType(x))}
}

BASIC_ARTH_OP = lambda op: {(IntegralType,)*2: lambda x, y: op(x.value, y.value), (FloatingPointType,)*2: lambda x, y: op(x.value, y.value)}
BASIC_CMP_OP = lambda op:  {None: lambda x, y: op(x.value, y.value)}  # `value`s for every type happen to be comparable in Python too

OPS = {
    (BINARY, "+"):  {**BASIC_ARTH_OP(operator.add),
        (PointerType, IntegralType): lambda x, y: x.value + x.type.target_type.width * y.value,
        (IntegralType, PointerType): lambda x, y: y.type.target_type.width * x.value + y.value
    },
    (BINARY, "-"):  {**BASIC_ARTH_OP(operator.sub),
        (PointerType, IntegralType): lambda x, y: x.value - x.type.target_type.width * y.value,
        (PointerType, PointerType):  lambda x, y: (x.value - y.value) // x.type.target_type.width,
    },
    (BINARY, "*"):  BASIC_ARTH_OP(operator.mul),
    (BINARY, "/"):  {
        (IntegralType, IntegralType):           lambda x, y: x.value // y.value,
        (FloatingPointType, FloatingPointType): lambda x, y: x.value / y.value
    },
    (BINARY, "%"):  {(IntegralType, IntegralType): lambda x, y: x.value % y.value},

    (BINARY, "&"):  {(IntegralType, IntegralType): lambda x, y: x.value & y.value},
    (BINARY, "<<"): {(IntegralType, IntegralType): lambda x, y: x.value << y.value},
    
    (BINARY, "<"):  BASIC_CMP_OP(operator.lt),
    (BINARY, ">"):  BASIC_CMP_OP(operator.gt),
    (BINARY, "<="): BASIC_CMP_OP(operator.le),
    (BINARY, ">="): BASIC_CMP_OP(operator.ge),
    (BINARY, "=="): {**BASIC_CMP_OP(operator.eq), (StructType, StructType): lambda x, y: all(OPS[BINARY, "=="](x.type.field_value(i, x.raw), y.type.field_value(i, y.raw)) for i in range(x.nfields))},
    (BINARY, "!="): {**BASIC_CMP_OP(operator.ne), (StructType, StructType): lambda x, y: any(OPS[BINARY, "!="](x.type.field_value(i, x.raw), y.type.field_value(i, y.raw)) for i in range(x.nfields))},

    (UNARY, "-"):   {(IntegralType,): lambda x: -x.value, (FloatingPointType,): lambda x: -x.value},

    (UNARY, "!"):   {None: lambda x: not x},

    (UNARY, "*"):   {(PointerType,): lambda x: base.InterpreterError("deref void*") if x.type.target_type == VOID else TypedValue(x.type.target_type, x.value)},
    (UNARY, "&"):   {None:           lambda x: x.blob_or_ptr if isinstance(x.blob_or_ptr, int) else base.InterpreterError(f"take address of rvalue {x}")}
}
NO_CONV_OPS = {(UNARY, "*")}

def coerce_to_bool(val):
    return (isinstance(val.type, PointerType) and val.value != 0) or val.cast(INT, implicit=True).value != 0

def get_op_casts_binary(opname, lhs, rhs):
    func = OP_CASTS[BINARY, opname].get((type(lhs), type(rhs)), OP_CASTS[BINARY, opname].get(None))
    if func is None or (res := func(lhs, rhs)) is None:
        raise base.InterpreterError(f"can't apply binary {opname} to {lhs}, {rhs}") from None
    return res

def get_op_casts_unary(opname, rhs):
    func = OP_CASTS[UNARY, opname].get((type(rhs),), OP_CASTS[UNARY, opname].get(None))
    if func is None or (res := func(rhs)) is None:
        raise base.InterpreterError(f"can't apply unary {opname} to {rhs}") from None
    return res

def do_op_binary(opname, lhs, rhs):
    lhstype, rhstype, resconv = get_op_casts_binary(opname, lhs.type, rhs.type)
    casted_lhs, casted_rhs = lhstype.cast(lhs, implicit=True) if lhstype else lhs, rhstype.cast(rhs, implicit=True) if rhstype else rhs
    res = OPS[BINARY, opname].get((type(lhstype), type(rhstype)), OPS[BINARY, opname].get(None))(casted_lhs, casted_rhs)
    if isinstance(res, base.InterpreterError):
        raise res
    return res if (BINARY, opname) in NO_CONV_OPS else resconv.conv(res)

def do_op_unary(opname, rhs):
    rhstype, resconv = get_op_casts_unary(opname, rhs.type)
    casted_rhs = rhstype.cast(rhs, implicit=True) if rhstype else rhs
    res = OPS[UNARY, opname].get((type(rhstype),), OPS[UNARY, opname].get(None))(casted_rhs)
    if isinstance(res, base.InterpreterError):
        raise res
    return res if (UNARY, opname) in NO_CONV_OPS else resconv.conv(res)

def call(func, args):
    if callable(func):
        res = func(*(argtype.cast(arg, implicit=True) for argtype, arg in zip(func.argtypes, args)))
        return TypedValue(VOID, b"") if res is None else res.cast(func.restype, implicit=True)

    functype = func.decl.type
    arginfos = []
    variadic = functype.variadic
    for argtype in functype.argtypes:
        arginfos.append((argtype, argtype.name))

    with base.in_frame({"": func.decl.name, "?": func.coord}) as frame:
        for (argtype, argname), arg in zip(arginfos, args[:len(arginfos)] if variadic else args, strict=True):
            frame[argname] = TypedValue.new_lvalue(init=argtype.cast(arg, implicit=True))

        if variadic:
            converted_args = []
            size = 0
            for arg in args[len(arginfos):]:
                if isinstance(arg.type, FloatingPointType):
                    arg = arg.cast(DOUBLE)
                if isinstance(arg.type, IntegralType) and (arg.type.length < 0):
                    arg = arg.cast(IntegralType(arg.type.signed, 0))
                converted_args.append(arg)
                size += arg.type.width
            rest = base.alloc(size)
            i = 0
            for arg in converted_args:
                base.set_at(rest + i, arg.raw)
                i += arg.type.width
            frame["..."] = TypedValue(ArrayType(CHAR, size), rest, reduce=False)

        rtype = functype.rtype
        try:
            ast_eval(func.body)
            if rtype != VOID:
                raise base.InterpreterError("function with return type returned nothing")
            return TypedValue(VOID, b"")
        except Return as e:
            if e.value is None:
                if rtype != VOID:
                    raise base.InterpreterError("function with return type returned nothing") from None
                return TypedValue(VOID, b"")
            return e.value.cast(rtype, implicit=True)
        except Continue:
            raise base.InterpreterError("continue outside loop") from None

def decl(node, target):
    if node.name in target and target[node.name] is not None:
        raise base.InterpreterError(f"re-declaration of {node.name}")
    target[node.name] = TypedValue.new_lvalue(node.type, reduce=False, init=ast_eval(node.init).cast(node.type, implicit=True) if node.init else None)

def ast_convert_types(ast, typedefs=None):
    typedefs = {} if typedefs is None else typedefs

    def get_type(parent, path, name=None):
        node = getattr(parent, path) if isinstance(path, str) else parent[path]
        if isinstance(node, CType):
            return node

        if isinstance(node, pycparser.c_ast.FuncDecl):
            res = FunctionType(get_type(node, "type"), tuple(y for i, x in enumerate(node.args.params) if (not isinstance(x, pycparser.c_ast.EllipsisParam)) and None != (y := get_type(node.args.params, i))), any(isinstance(x, pycparser.c_ast.EllipsisParam) for x in node.args.params))

        elif isinstance(node, (pycparser.c_ast.Decl, pycparser.c_ast.TypeDecl, pycparser.c_ast.Typename)):
            res = get_type(node, "type", getattr(node, "name", None))

        elif isinstance(node, pycparser.c_ast.PtrDecl):
            res = PointerType(get_type(node, "type"))

        elif isinstance(node, pycparser.c_ast.ArrayDecl):
            res = ArrayType(get_type(node, "type"), ast_eval(node.dim).cast(INT, implicit=True).value)

        elif isinstance(node, pycparser.c_ast.IdentifierType):
            if "float" in node.names or "double" in node.names:
                if "long" in node.names:
                    raise base.InterpreterError("long double not supported")
                res = FloatingPointType("float" in node.names)
            elif "short" in node.names or ("long" in node.names) or "char" in node.names or "int" in node.names or "unsigned" in node.names or "signed" in node.names:
                signed = not ("unsigned" in node.names)
                length = node.names.count("long") - node.names.count("short") - 2 * node.names.count("char")
                res = IntegralType(signed, length)
            elif "void" in node.names:
                res = VOID
            elif len(node.names) == 1 and node.names[0] in typedefs:
                res = typedefs[node.names[0]]
            else:
                raise NotImplementedError(f"type {node!r}")
        
        elif isinstance(node, pycparser.c_ast.Struct):
            decls = None if node.decls is None else [(x.name, get_type(node.decls, i)) for i, x in enumerate(node.decls)]
            if node.name is not None:
                res = make_struct(node.name, decls)
            else:
                res = StructType(None, decls)
        
        else:
            raise NotImplementedError(f"type {node!r}")

        if name is not None:
            res = res.with_name(name)
        if isinstance(path, str):
            setattr(parent, path, res)
        else:
            parent[path] = res
        return res

    for k, v in list(enumerate(ast)) if isinstance(ast, list) else ast.children():
        if isinstance(v, pycparser.c_ast.Typedef):
            typedefs[v.name] = get_type(v, "type")
            ast[k] = None  # will definitely be a list, typedef can't occur as a subexpression or anything



        elif isinstance(v, (pycparser.c_ast.Struct, pycparser.c_ast.IdentifierType, pycparser.c_ast.ArrayDecl, pycparser.c_ast.PtrDecl, pycparser.c_ast.TypeDecl, pycparser.c_ast.Typename, pycparser.c_ast.FuncDecl)):
            get_type(ast, k)
        else:
            ast_convert_types(v, typedefs)
    if isinstance(ast, list):
        while None in ast:
            ast.remove(None)

def dump_stack():
    frames_repr = "{"
    for frame in reversed(base.frame_stack):
        if len(frame) > 0:
            items = []
            for key, value in frame.items():
                if isinstance(value, TypedValue):
                    try:
                        items.append(f"{value.type.typename().replace('$', key).strip()} = {value.value!r}")
                    except base.InterpreterError:
                        items.append(f"{value.type.typename().replace('$', key).strip()} = ???")
            frames_repr += "; ".join(items) + ("}(" + f":{frame['?'].line}:{frame['?'].column} {frame['']}" + ") {" if "" in frame else "| ")
    frames_repr += "}"
    print(frames_repr)

def ast_type_tree(nodes, nearest_block=None):
    node = nodes[-1]
    # if node.coord:
        # print(node.coord.file, node.coord.line, node.coord.column)

    def rec(node):
        return ast_type_tree([*nodes, node], next_nearest_block)

    def typ():
        # A statement
        if isinstance(node, (
            pycparser.c_ast.FileAST, pycparser.c_ast.FuncDef, pycparser.c_ast.Compound, pycparser.c_ast.If,
            pycparser.c_ast.For, pycparser.c_ast.While, pycparser.c_ast.EmptyStatement, pycparser.c_ast.DeclList,
            pycparser.c_ast.Return, pycparser.c_ast.Continue, pycparser.c_ast.Decl
        )):
            return VOID

        # An expression
        if isinstance(node, pycparser.c_ast.Constant):
            if node.type == "string" and isinstance(nodes[-2], pycparser.c_ast.Decl) and isinstance(nodes[-2].type, ArrayType):
                return nodes[-2].type
            return {"string": PointerType(CHAR), "int": INT, "long long int": LONG_LONG, "unsigned long long int": UNSIGNED_LONG_LONG, "unsigned int": UNSIGNED_INT, "char": CHAR}[node.type]

        if isinstance(node, pycparser.c_ast.UnaryOp):
            if node.op == "sizeof":
                return INT
            return (get_op_casts_binary(node.op[-1], rec(node.expr), INT) if node.op in {"++", "--", "p++", "p--"} else get_op_casts_unary(node.op, rec(node.expr)))[-1]

        if isinstance(node, pycparser.c_ast.Assignment):
            return rec(node.rvalue)

        if isinstance(node, pycparser.c_ast.StructRef):
            if node.type == "->":
                return rec(node.name).target_type.get_field_type(node.field.name)

        if isinstance(node, pycparser.c_ast.InitList):
            if isinstance(nodes[-2], pycparser.c_ast.Decl):
                return nodes[-2].type
            elif isinstance(nodes[-2], pycparser.c_ast.Cast):
                return nodes[-2].to_type
            raise InterpreterError("incorrect position for initializer list")

        if isinstance(node, pycparser.c_ast.ID):
            # print("===========")
            block = nearest_block
            while block is not None:
                # print("search in", base.attrs[block]["local_vars"])
                if node.name in base.attrs[block]["local_vars"]:
                    return base.attrs[block]["local_vars"][node.name].reduce(0)[0]
                block = base.attrs[block]["parent_block"]
            raise base.InterpreterError(f"no such variable {node.name}")

        try:
            return {
                pycparser.c_ast.BinaryOp:  lambda: get_op_casts_binary(node.op, rec(node.left), rec(node.right))[2],
                pycparser.c_ast.Cast:      lambda: node.to_type,
                pycparser.c_ast.FuncCall:  lambda: rec(node.name).target_type.rtype,
                pycparser.c_ast.ArrayRef:  lambda: rec(node.name).target_type,
                pycparser.c_ast.ExprList:  lambda: rec(node.exprs[-1]),
                pycparser.c_ast.TernaryOp: lambda: rec(node.iftrue)
            }[type(node)]()
        except KeyError:
            raise NotImplementedError(node) from None

    if node in base.attrs:
        return base.attrs[node]["type"]

    base.attrs[node] = {}

    next_nearest_block = nearest_block

    if isinstance(node, (pycparser.c_ast.FileAST, pycparser.c_ast.Compound, pycparser.c_ast.If, pycparser.c_ast.For, pycparser.c_ast.While, pycparser.c_ast.FuncDef)):
        base.attrs[node]["parent_block"] = nearest_block
        base.attrs[node]["local_vars"] = {}
        next_nearest_block = node
    if isinstance(node, pycparser.c_ast.FuncDef):
        base.attrs[node]["local_vars"].update({i.name: i for i in node.decl.type.argtypes})
        base.attrs[nearest_block]["local_vars"][node.decl.name] = node.decl.type
    if isinstance(node, pycparser.c_ast.Decl):
        base.attrs[nearest_block]["local_vars"][node.name] = node.type

    if not isinstance(node, pycparser.c_ast.StructRef):
        for _, child in node.children():
            if isinstance(child, pycparser.c_ast.Node):
                rec(child)
            if isinstance(child, list):
                for subchild in child:
                    if isinstance(subchild, pycparser.c_ast.Node):
                        rec(subchild)

    base.attrs[node]["type"] = typ()
    return base.attrs[node]["type"]

nodes_evald = 0
DEBUG = False
def ast_eval(node):
    global nodes_evald
    nodes_evald += 1
    for frame in reversed(base.frame_stack):
        if frame is base.frame_stack[0] or "" in frame:
            frame["?"] = node.coord
            break

    if DEBUG:
        dump_stack()

    # The statements
    if isinstance(node, pycparser.c_ast.Compound):
        with base.in_frame():
            for subnode in node.block_items:
                ast_eval(subnode)
        return None

    if isinstance(node, pycparser.c_ast.If):
        with base.in_frame():
            if coerce_to_bool(ast_eval(node.cond)):
                ast_eval(node.iftrue)
            elif node.iffalse:
                ast_eval(node.iffalse)
        return None

    if isinstance(node, pycparser.c_ast.For):
        with base.in_frame():
            if node.init:
                ast_eval(node.init)
            while coerce_to_bool(ast_eval(node.cond)):
                try:
                    if node.stmt:
                        ast_eval(node.stmt)
                except Continue:
                    pass
                if node.next:
                    ast_eval(node.next)
        return None

    if isinstance(node, pycparser.c_ast.While):
        with base.in_frame():
            while ast_eval(node.cond).cast(INT, implicit=True).value != 0:
                try:
                    if node.stmt:
                        ast_eval(node.stmt)
                except Continue:
                    pass
        return None

    if isinstance(node, pycparser.c_ast.EmptyStatement):
        return None

    if isinstance(node, pycparser.c_ast.DeclList):
        for subnode in node.decls:
            ast_eval(subnode)
        return None

    if isinstance(node, pycparser.c_ast.Decl):
        return decl(node, base.frame_stack[-1])

    if isinstance(node, pycparser.c_ast.Return):
        value = ast_eval(node.expr)
        raise Return(TypedValue(value.type, value.raw))

    if isinstance(node, pycparser.c_ast.Continue):
        raise Continue

    # The expressions
    if isinstance(node, pycparser.c_ast.Constant):
        if node.type == "string" and isinstance((typ := base.attrs[node]["type"]), ArrayType):
            return TypedValue(typ, ast.literal_eval(node.value).encode("utf8").ljust(typ.width, b"\0")[:typ.width], reduce=False)
        type_func_map = {
            "string": lambda x: PointerType(CHAR).conv(base.intern_string_constant(ast.literal_eval(x))), "int": lambda x: INT.conv(x),
            "long long int": lambda x: LONG_LONG.conv(x[:-2]), "char": lambda x: CHAR.conv(ord(ast.literal_eval(x))),
            "unsigned int": lambda x: UNSIGNED_INT.conv(x[:-1]), "unsigned long long int": lambda x: UNSIGNED_LONG_LONG.conv(x[:-3])
        }
        return type_func_map[node.type](node.value)

    if isinstance(node, pycparser.c_ast.InitList):
        target_type = base.attrs[node]["type"]
        if isinstance(target_type, ArrayType):
            init = bytearray(target_type.width)
            if len(node.exprs) == 1:
                initvals = [ast_eval(node.exprs[0]).cast(target_type.target_type, implicit=True)] * len(target_type.n)
            else:
                initvals = [ast_eval(subnode).cast(target_type.target_type, implicit=True) for subnode in node.exprs]
            for i, initval in zip(range(0, target_type.width, target_type.target_type.width), initvals):
                init[i:i+target_type.target_type.width] = initval.raw
            return TypedValue(target_type, init, reduce=False)

        elif isinstance(target_type, StructType):
            init = bytearray(target_type.width)
            offs = 0
            for (_, ftype), subnode in zip(target_type.fields, node.exprs):
                init[offs:offs+ftype.width] = ast_eval(subnode).cast(ftype, implicit=True).raw
                offs += ftype.width
            return TypedValue(target_type, init)

        raise InterpreterError(f"invalid initializer list of type {target_type!r}")

    if isinstance(node, pycparser.c_ast.UnaryOp):
        # TODO: unsigned inc/dec
        if node.op == "++" or node.op == "--":
            return (lvalue := ast_eval(node.expr)).assign(do_op_binary(node.op[-1], lvalue, CHAR.conv(1)))

        if node.op == "p++" or node.op == "p--":
            lvalue = ast_eval(node.expr)
            rvalue = TypedValue(lvalue.type, lvalue.raw)
            lvalue.assign(do_op_binary(node.op[-1], lvalue, CHAR.conv(1)))
            return rvalue

        if node.op == "sizeof":
            return INT.conv(node.expr.width) if isinstance(node.expr, CType) else INT.conv(base.attrs[node.expr]["type"].width)

        return do_op_unary(node.op, ast_eval(node.expr))

    if isinstance(node, pycparser.c_ast.Assignment):
        lvalue = ast_eval(node.lvalue)
        rvalue = ast_eval(node.rvalue) if node.op == "=" else do_op_binary(node.op[:-1], lvalue, ast_eval(node.rvalue))
        lvalue.assign(rvalue.cast(lvalue.type, implicit=True))
        return rvalue

    if isinstance(node, pycparser.c_ast.StructRef):
        if node.type == "->":
            ptr = ast_eval(node.name)
            return TypedValue(ptr.type.target_type.get_field_type(node.field.name), ptr.value + ptr.type.target_type.get_field_offs(node.field.name))

    if isinstance(node, pycparser.c_ast.BinaryOp) and node.op == "&&":
        return INT.conv(coerce_to_bool(ast_eval(node.left)) and coerce_to_bool(ast_eval(node.right)))
    
    if isinstance(node, pycparser.c_ast.BinaryOp) and node.op == "||":
        return INT.conv(coerce_to_bool(ast_eval(node.left)) or coerce_to_bool(ast_eval(node.right)))

    exprtype_func_map = {
        pycparser.c_ast.BinaryOp:  lambda: do_op_binary(node.op, ast_eval(node.left), ast_eval(node.right)),
        pycparser.c_ast.FuncCall:  lambda: ast_eval(node.name)(*(ast_eval(subnode) for subnode in (node.args or []))),
        pycparser.c_ast.ID:        lambda: base.var(node.name),
        pycparser.c_ast.ArrayRef:  lambda: do_op_unary("*", do_op_binary("+", ast_eval(node.name), ast_eval(node.subscript))),
        pycparser.c_ast.ExprList:  lambda: [ast_eval(subnode) for subnode in node.exprs][-1],
        pycparser.c_ast.TernaryOp: lambda: ast_eval(node.iftrue) if coerce_to_bool(ast_eval(node.cond)) else ast_eval(node.iffalse),
        pycparser.c_ast.Cast:      lambda: ast_eval(node.expr).cast(node.to_type)
    }
    try:
        f = exprtype_func_map[type(node)]
    except KeyError:
        raise NotImplementedError(node) from None
    return f()
