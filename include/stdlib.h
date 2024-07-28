#ifndef __LIBC_STDLIB
#define __LIBC_STDLIB

#include <stddef.h>

void *malloc(int x);
void free(void *addr);
void abort(void);

typedef unsigned long long intptr_t;  // TODO: should be signed

#endif  // __LIBC_STDLIB
