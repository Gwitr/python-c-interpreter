#ifndef __LIBC_STDIO
#define __LIBC_STDIO

#include <stdlib.h>
#include <stdarg.h>

#define __FILE_ERROR 1
#define __FILE_EOF 2

typedef struct FILE {
    int flags;
    int (*write)(void *buf, int sz, int n, struct FILE *file);
    void (*close)(struct FILE *file);
    void *userdata;
} FILE;

extern struct FILE __stdout, __stderr;

#define stdout (&__stdout)
#define stderr (&__stderr)

FILE *fmemopen(void *buf, size_t sz, const char *mode);
FILE *open_memstream(void **pbuf, size_t *pbufsz);

int fwrite(void *buf, int sz, int n, FILE *f);
void fclose(FILE *f);
int ferror(FILE *f);
int feof(FILE *f);

int vfprintf(FILE *f, const char *fmt, va_list args);
int fprintf(FILE *f, const char *fmt, ...);
void fputs(const char *str, FILE *f);
void fputc(char c, FILE *f);

int vprintf(const char *fmt, va_list list);
int printf(const char *fmt, ...);
void puts(const char *str);
void putc(char c, FILE *f);

int vsnprintf(char *buf, size_t n, const char *fmt, va_list args);
int snprintf(char *buf, size_t n, const char *fmt, ...);

int vsprintf(char *buf, const char *fmt, va_list args);
int sprintf(char *buf, const char *fmt, ...);

#endif  // __LIBC_STDIO
