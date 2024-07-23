#ifndef __LIBC_STDARG
#define __LIBC_STDARG

// TODO: this is weird this doesn't work right
// Like I get va_start/va_copy depending on the va_list it initializer being an lvalue, I don't think you can go around that, but va_arg?
// Doing it this way now means that every va_list is implicitly copied without the need for va_copy

typedef void *va_list;

#define va_start(arg, lastN) (arg = __va_start())
#define va_arg(list, T) (*(T*)__va_arg(&list, sizeof(T)))
#define va_copy(dstlist, srclist) (dstlist = srclist)
#define va_end(list)

va_list __va_start(void);
void *__va_arg(va_list list, int size);

#endif  // __LIBC_STDARG
