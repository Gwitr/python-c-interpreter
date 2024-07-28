#ifndef __LIBC_STDDEF
#define __LIBC_STDDEF

typedef long long ssize_t, ptrdiff_t;
typedef unsigned long long size_t, max_align_t;
typedef short wchar_t;

#define NULL ((void*)0ull)
#define offsetof(type, memb) ((size_t)(&(((type*)0ull)->memb)))

#endif  // __LIBC_STDDEF
