#ifndef __LIBC_STDLIB
#define __LIBC_STDLIB

#define NULL ((void*)0)

void *malloc(int x);
void free(void *addr);
void abort(void);

typedef unsigned long long intptr_t;
typedef unsigned long long size_t;
typedef long long ptrdiff_t;
typedef long long ssize_t;

#endif  // __LIBC_STDLIB
