import os
import sys
import time

import pycparser

import base
import ceval
from typ import *

# sys.setrecursionlimit(150)

if __name__ == "__main__":
    BUILTINS = {}

    def builtin(name, restype, argtypes):
        def decorator(func):
            BUILTINS[name] = func
            func.argtypes = argtypes
            func.restype = restype
            return func
        return decorator

    def register_builtins():
        for name, func in BUILTINS.items():
            try:
                base.functions[base.globalvars[name].value] = func
            except KeyError:
                pass  # Code doesn't seem to use this function at all

    @builtin("__malloc_size", UNSIGNED_LONG_LONG, (PointerType(VoidType()), ))
    def builtin_malloc_size(ptr):
        return INTPTR_T.conv(len(base.objects[ptr.value >> 32]))

    @builtin("malloc", PointerType(VoidType()), (INT, ))
    def builtin_malloc(n):
        return INTPTR_T.conv(base.alloc(n.value)).cast(PointerType(VoidType()))

    @builtin("free", None, (PointerType(VoidType()), ))
    def builtin_free(ptr):
        base.free(ptr.value)

    @builtin("abort", None, ())
    def builtin_abort():
        raise base.InterpreterError("aborted")

    @builtin("putch", None, (CHAR, ))
    def builtin_putch(c):
        print(chr(c.value), end="")

    @builtin("__va_start", PointerType(VoidType()), ())
    def builtin_va_start():
        return base.var("...").cast(PointerType(VoidType()))

    @builtin("dbg", None, (PointerType(CHAR),))
    def builtin_dbg(s):
        start = base.objects[s.value>>32][s.value&0xFFFFFFFF:]
        print(start[:start.find(b"\0")].decode("ascii"))

    @builtin("dbg_ptr", None, (PointerType(VoidType()),))
    def builtin_dbg_ptr(s):
        print(f"ptr={s.value}")
        print(f"val={int.from_bytes(base.objects[s.value >> 32], 'little')}")

    @builtin("dump_stack", None, ())
    def builtin_dump_stack():
        dump_stack()

    function_idx = 1
    for fn in ["libc.c", "main.c"]:
        new_struct_ctx()
        tree = pycparser.parse_file(fn, use_cpp=True, cpp_args="-Iinclude")
        ext = tree.ext
        ceval.ast_convert_types(ext)
        ceval.ast_type_tree([tree])
        for nodei, node in enumerate(ext):
            if isinstance(node, pycparser.c_ast.FuncDef):
                if node.decl.name in base.globalvars:
                    base.functions[base.globalvars[node.decl.name].value] = node
                else:
                    base.functions[function_idx] = node
                    base.globalvars[node.decl.name] = TypedValue.new_lvalue(init=INTPTR_T.conv(function_idx).cast(PointerType(node.decl.type)))
                    function_idx += 1

            elif isinstance(node, pycparser.c_ast.Decl):
                if isinstance(node.type, FunctionType):
                    if node.name in base.globalvars:
                        if base.globalvars[node.name].type.target_type != node.type:
                            raise base.InterpreterError(f"re-declaration of function {node.name} with different type")
                    else:
                        base.globalvars[node.name] = TypedValue.new_lvalue(init=INTPTR_T.conv(function_idx).cast(PointerType(node.type)))
                        function_idx += 1
                else:
                    if "extern" not in node.storage:
                        ceval.decl(node, base.globalvars)

            else:
                raise base.InterpreterError(f"unknown global node type {type(node).__qualname__}")

    register_builtins()

    addresses = []
    for arg in sys.argv:
        addresses.append(base.intern_string_constant(arg))
    base.objects[(argv_addr := base.alloc(8 * len(sys.argv) + 8)) >> 32] = struct.pack("<" + "Q" * len(sys.argv) + "Q", *addresses, 0)

    print("program start")
    s = time.process_time()
    res = base.globalvars["main"](INT.conv(len(sys.argv)), INTPTR_T.conv(argv_addr).cast(PointerType(PointerType(CHAR))))
    print(round(time.process_time() - s, 3))
    print("program end")

    print("leaked:", set(i for i in base.objects.keys()) - set(i.blob_or_ptr >> 32 for i in base.globalvars.values()) - set(value >> 32 for value in base.string_constants.values()) - {argv_addr >> 32})
    print("nodes eval'd:", ceval.nodes_evald)

    sys.exit(res.value)

    # except base.InterpreterError as e:
        # print(e, file=sys.stderr)
