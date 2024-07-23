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

def call(func, args):
    if callable(func):
        res = func(*(argtype.cast(arg, implicit=True) for argtype, arg in zip(func.argtypes, args)))
        return TypedValue(VoidType(), b"") if res is None else res.cast(func.restype, implicit=True)

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
            if rtype != VoidType():
                raise base.InterpreterError("function with return type returned nothing")
            return TypedValue(VoidType(), b"")
        except Return as e:
            if e.value is None:
                if rtype != VoidType():
                    raise base.InterpreterError("function with return type returned nothing") from None
                return TypedValue(VoidType(), b"")
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
                res = VoidType()
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
            if isinstance(ast, list):
                ast[k] = None
            else:
                delattr(ast, k)
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

def coerce_to_bool(val):
    return (isinstance(val.type, PointerType) and val.value != 0) or val.cast(INT, implicit=True).value != 0

def ast_type_tree(nodes, nearest_block=None):
    node = nodes[-1]
    # if node.coord:
        # print(node.coord.file, node.coord.line, node.coord.column)

    def rec(node, new_nearest_block=None):
        return ast_type_tree([*nodes, node], new_nearest_block if new_nearest_block else nearest_block)

    def typ():
        # A statement
        if isinstance(node, pycparser.c_ast.FileAST):
            base.attrs[node]["parent_block"] = nearest_block
            base.attrs[node]["local_vars"] = {}
            for i in node.ext:
                rec(i, node)
            return VoidType()

        if isinstance(node, pycparser.c_ast.FuncDef):
            base.attrs[node]["parent_block"] = nearest_block
            base.attrs[node]["local_vars"] = {i.name: i for i in node.decl.type.argtypes}
            base.attrs[nearest_block]["local_vars"][node.decl.name] = node.decl.type
            rec(node.body, node)
            return VoidType()

        if isinstance(node, pycparser.c_ast.Compound):
            base.attrs[node]["parent_block"] = nearest_block
            base.attrs[node]["local_vars"] = {}
            for subnode in node.block_items:
                rec(subnode, node)
            return VoidType()

        if isinstance(node, pycparser.c_ast.If):
            base.attrs[node]["parent_block"] = nearest_block
            base.attrs[node]["local_vars"] = {}
            rec(node.cond, node)
            rec(node.iftrue, node)
            if node.iffalse:
                rec(node.iffalse, node)
            return VoidType()

        if isinstance(node, pycparser.c_ast.For):
            base.attrs[node]["parent_block"] = nearest_block
            base.attrs[node]["local_vars"] = {}
            if node.init:
                rec(node.init, node)
            if node.cond:
                rec(node.cond, node)
            if node.stmt:
                rec(node.stmt, node)
            if node.next:
                rec(node.next, node)
            return VoidType()

        if isinstance(node, pycparser.c_ast.While):
            base.attrs[node]["parent_block"] = nearest_block
            base.attrs[node]["local_vars"] = {}
            rec(node.cond, node)
            rec(node.stmt, node)
            return VoidType()

        if isinstance(node, pycparser.c_ast.EmptyStatement):
            return VoidType()

        if isinstance(node, pycparser.c_ast.DeclList):
            for subnode in node.decls:
                rec(subnode)
            return VoidType()

        if isinstance(node, pycparser.c_ast.Decl):
            if node.init:
                rec(node.init)
            base.attrs[nearest_block]["local_vars"][node.name] = node.type
            return VoidType()

        if isinstance(node, pycparser.c_ast.Return):
            rec(node.expr)
            return VoidType()

        if isinstance(node, pycparser.c_ast.Continue):
            return VoidType()

        # An expression
        if isinstance(node, pycparser.c_ast.Constant):
            if node.type == "string" and isinstance(nodes[-2], pycparser.c_ast.Decl) and isinstance(nodes[-2].type, ArrayType):
                return nodes[-2].type
            return {"string": PointerType(CHAR), "int": INT, "long long int": LONG_LONG, "unsigned long long int": UNSIGNED_LONG_LONG, "char": CHAR}[node.type]

        if isinstance(node, pycparser.c_ast.BinaryOp):
            # TODO: Automatically introduce cast
            lhst, rhst = rec(node.left), rec(node.right)
            if node.op in {"&&", "||", "==", "!=", "<", ">", "<=", ">="}:
                return INT
            return arithmetic_types(lhst, rhst)[2]

        if isinstance(node, pycparser.c_ast.UnaryOp):
            if node.op == "sizeof":
                return INT
            exprt = rec(node.expr)
            if node.op == "++" or node.op == "--" or node.op == "p++" or node.op == "p--":
                return arithmetic_types(exprt, INT)[2]
            return {"&": lambda: PointerType(exprt), "*": lambda: exprt.target_type, "-": lambda: exprt, "!": lambda: INT}[node.op]()

        if isinstance(node, pycparser.c_ast.Assignment):
            rec(node.lvalue)
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

        exprtype_func_map = {
            pycparser.c_ast.Cast:      lambda: [rec(node.expr), node.to_type][1],
            pycparser.c_ast.FuncCall:  lambda: [rec(node.name), *(rec(subnode) for subnode in (node.args or []))][0],
            pycparser.c_ast.ArrayRef:  lambda: [rec(node.name), rec(node.subscript)][0].target_type,
            pycparser.c_ast.ExprList:  lambda: [rec(subnode) for subnode in node.exprs][-1],
            pycparser.c_ast.TernaryOp: lambda: [rec(node.iftrue), rec(node.cond), rec(node.iffalse)][0]
        }

        try:
            func = exprtype_func_map[type(node)]
        except KeyError:
            raise NotImplementedError(node) from None
        return func()

    base.attrs[node] = {}
    res = base.attrs[node]["type"] = typ()
    return res

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
            while ast_eval(node.cond).cast(INT, implicit=True).value != 0:
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
            "long long int": lambda x: LONG_LONG.conv(x[:-2]), "char": lambda x: CHAR.conv(ord(ast.literal_eval(x)))
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

    if isinstance(node, pycparser.c_ast.BinaryOp):
        op_func_map = {
            "==": operator.eq, "!=": lambda x, y: INT.conv(1) - (x == y),
            "&": operator.and_, "<": operator.lt, ">": operator.gt, "<=": operator.le,
            ">=": operator.ge, "*": operator.mul, "/": operator.truediv, "%": operator.mod,
            "+": operator.add, "-": operator.sub
        }
        if node.op == "&&":
            return INT.conv(coerce_to_bool(ast_eval(node.left)) and coerce_to_bool(ast_eval(node.right)))
        if node.op == "||":
            return INT.conv(coerce_to_bool(ast_eval(node.left)) or coerce_to_bool(ast_eval(node.right)))
        return op_func_map[node.op](ast_eval(node.left), ast_eval(node.right))

    if isinstance(node, pycparser.c_ast.UnaryOp):
        if node.op == "++" or node.op == "--":
            return (lvalue := ast_eval(node.expr)).assign(lvalue + CHAR.conv(1 if node.op == "++" else -1))

        if node.op == "p++" or node.op == "p--":
            lvalue = ast_eval(node.expr)
            rvalue = TypedValue(lvalue.type, lvalue.raw)
            lvalue.assign(lvalue + CHAR.conv(1 if node.op == "p++" else -1))
            return rvalue

        if node.op == "sizeof":
            if isinstance(node.expr, CType):
                return INT.conv(node.expr.width)

        op_func_map = {"&": lambda v: v.at, "*": lambda v: v[0], "-": lambda v: -v, "!": lambda v: INT.conv(not coerce_to_bool(v))}
        return op_func_map[node.op](ast_eval(node.expr))

    if isinstance(node, pycparser.c_ast.Assignment):
        op_func_map = {"=": lambda _, v: v, "*=": operator.mul, "/=": operator.truediv, "%=": operator.mod, "+=": operator.add, "-=": operator.sub}
        lvalue = ast_eval(node.lvalue)
        lvalue.assign((rvalue := op_func_map[node.op](lvalue, ast_eval(node.rvalue))).cast(lvalue.type, implicit=True))
        return rvalue

    if isinstance(node, pycparser.c_ast.StructRef):
        if node.type == "->":
            ptr = ast_eval(node.name)
            return TypedValue(ptr.type.target_type.get_field_type(node.field.name), ptr.value + ptr.type.target_type.get_field_offs(node.field.name))

    exprtype_func_map = {
        pycparser.c_ast.FuncCall:  lambda: ast_eval(node.name)(*(ast_eval(subnode) for subnode in (node.args or []))),
        pycparser.c_ast.ID:        lambda: base.var(node.name),
        pycparser.c_ast.ArrayRef:  lambda: ast_eval(node.name)[ast_eval(node.subscript)],
        pycparser.c_ast.ExprList:  lambda: [ast_eval(subnode) for subnode in node.exprs][-1],
        pycparser.c_ast.TernaryOp: lambda: ast_eval(node.iftrue) if coerce_to_bool(ast_eval(node.cond)) else ast_eval(node.iffalse),
        pycparser.c_ast.Cast:      lambda: ast_eval(node.expr).cast(node.to_type)
    }
    try:
        return exprtype_func_map[type(node)]()
    except KeyError:
        raise NotImplementedError(node) from None
