#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

void dbg(const char *);
void dbg_ptr(void *);

//////////////////// stdarg

void *__va_arg(va_list *list, int size)
{
    void *tmp = *list;
    *(char**)list += size;
    return tmp;
}

//////////////////// string

int strlen(const char *str)
{
    int i;
    for (i = 0; *str; ++i)
        ++str;
    return i;
}

void memcpy(void *dest, void *src, size_t n)
{
    char *cpdest = dest, *cpsrc = src;
    for (; n--; ++cpdest, ++cpsrc)
        *cpdest = *cpsrc;
}

//////////////////// stdlib

size_t __malloc_size(void *ptr);

void *realloc(void *orig, size_t n)
{
    size_t oldn = __malloc_size(orig);
    if (orig == NULL) return malloc(n);
    void *new = malloc(n);
    if (!new) return NULL;
    memcpy(new, orig, oldn < n ? oldn : n);
    free(orig);
    return new;
}

//////////////////// stdio

// memstream

struct __memstream_userdata {
    int borrowed;
    char **pbuf;
    size_t *pbufsz, offs;

    char *dummy1;
    size_t dummy2;
};

int __write_memstream(void *buf, int sz, int n, FILE *file)
{
    struct __memstream_userdata *userdata = file->userdata;
    while (*userdata->pbufsz < userdata->offs + sz * n + 1) {
        if (userdata->borrowed) {  // Don't attempt to expand a borrowed buffer
            int nout = *userdata->pbufsz > userdata->offs ? __write_memstream(buf, 1, *userdata->pbufsz - userdata->offs, file) : 0;
            file->flags = __FILE_ERROR;
            return nout;
        }
        void *new = realloc(*userdata->pbuf, 2 * *userdata->pbufsz);
        if (!new) {
            file->flags = __FILE_ERROR;
            return 0;
        }
        *userdata->pbuf = new;
        *userdata->pbufsz *= 2;
    }
    memcpy(*userdata->pbuf + userdata->offs, buf, sz * n);
    userdata->offs += sz * n;
    return sz * n;
}

void __close_memstream(FILE *file)
{
    struct __memstream_userdata *userdata = file->userdata;
    if (!userdata->borrowed)
        free(*userdata->pbuf);
    free(file->userdata);
    free(file);
}

FILE *__open_memstream(void **pbuf, size_t *pbufsz, int borrowed_memory)
{
    FILE *f = malloc(sizeof(FILE));
    if (!f)
        return NULL;
    struct __memstream_userdata *userdata = f->userdata = malloc(sizeof(struct __memstream_userdata));
    if (!userdata) {
        free(f);
        return NULL;
    }
    if (!borrowed_memory) {
        *pbuf = malloc(1);
        if (!pbuf) {
            free(f);
            return NULL;
        }
        *pbufsz = 1;
        userdata->pbuf = pbuf;
        userdata->pbufsz = pbufsz;
    } else {
        userdata->dummy1 = *pbuf;
        userdata->dummy2 = *pbufsz;
        userdata->pbuf = &userdata->dummy1;
        userdata->pbufsz = &userdata->dummy2;
    }
    f->write = __write_memstream;
    f->close = __close_memstream;
    userdata->offs = 0;
    userdata->borrowed = borrowed_memory;
    return f;
}

FILE *open_memstream(void **pbuf, size_t *pbufsz)
{
    return __open_memstream(pbuf, pbufsz, 0);
}

FILE *fmemopen(void *buf, size_t sz, const char *mode)
{
    // mode is not enforced
    return __open_memstream(&buf, &sz, 1);
}

// standard stream i/o
int putch(char c);

int __write_stdout(void *buf, int sz, int n, FILE *file)
{
    for (int i = 0; i < sz * n; ++i)
        putch(((char*)buf)[i]);
    return sz * n;
}

struct FILE __stdout = { 0, __write_stdout, NULL, NULL }, __stderr = { 0, __write_stdout, NULL, NULL };

// FILE* access apis
int fwrite(void *buf, int sz, int n, FILE *f)
{
    if (f->flags == 0)
        return f->write(buf, sz, n, f);
}

void fclose(FILE *f)
{
    f->close(f);
}

void fputs(const char *str, FILE *f)
{
    fwrite(str, 1, strlen(str), f);
}

void fputc(char c, FILE *f)
{
    fwrite(&c, 1, 1, f);
}

void puts(const char *str)
{
    fputs(str, stdout);
    fputc('\n', stdout);
}

void putc(char c)
{
    fputc(c, stdout);
    fputc('\n', stdout);
}

int ferror(FILE *f)
{
    return !!(f->flags & __FILE_ERROR);
}

int feof(FILE *f)
{
    return !!(f->flags & __FILE_EOF);
}

// printf
int __print_long_long(FILE *f, long long n, int sign_mode)
{
    if (n == 0) {
        fputs("0", f);
        return 1;
    }
    char buf[21], *pbuf = buf + 20;
    int minus = 0;
    if (n < 0) {
        n = -n;
        minus = 1;
    }
    for (; n; n /= 10) {
        *(pbuf--) = '0' + n % 10;
    }
    if (minus) {
        *(pbuf--) = '-';
    } else if (sign_mode) {
        *(pbuf--) = sign_mode > 0 ? '+' : ' ';
    }
    return fwrite(pbuf + 1, 1, buf + 20 - pbuf, f);
}

int __long_long_length(long long n, int sign_mode)
{
    int length = n <= 0 || sign_mode;
    for (; n; n /= 10)
        length++;
    return length;
}
// void dump_stack(void);
int __rjust(FILE *f, int width, int diff, int zeropad)
{
    // dump_stack();
    for (int i = 0; i < width - diff; ++i) {
        fputc(zeropad ? '0' : ' ', f);  // todo error detection here
        if (ferror(f))
            return i;
    }
    return width - diff;
}

int vfprintf(FILE *f, const char *fmt, va_list args)
{
    int idx = 0;
    int gather = 0, ljust, force_sign, space_sign, use_prefix, zeropad, width, precision, length;
    int nout = 0;
    for (; *fmt; ++fmt, ++idx) {
        char current = *fmt;
        if (*fmt == '%' && gather == 0) {
            gather = 1;
            ljust = force_sign = space_sign = use_prefix = zeropad = width = precision = length = 0;
            continue;
        }

        if (gather == 1) {
            if (*fmt == '-')      { ljust = 1;      continue; }
            else if (*fmt == '+') { force_sign = 1; continue; }
            else if (*fmt == ' ') { space_sign = 1; continue; }
            else if (*fmt == '#') { use_prefix = 1; continue; }
            else if (*fmt == '0') { zeropad = 1;    continue; }
            else if (*fmt == '%') { gather = 0; }
            else { gather = 2; }
        }
        if (gather == 2) {
            if (*fmt == '*') { width = va_arg(args, int); gather = 4; continue; }
            else { gather = 3; }
        }
        if (gather == 3) {
            if (*fmt >= '0' && *fmt <= '9') { width *= 10; width += *fmt - '0'; }
            else { gather = 4; }
        }
        if (gather == 4) {
            if (*fmt == '.') { gather = 5; continue; }
            else { gather = 7; }
        }
        if (gather == 5) {
            if (*fmt == '*') { precision = va_arg(args, int); gather = 7; continue; }
            else { gather = 6; }
        }
        if (gather == 6) {
            if (*fmt >= '0' && *fmt <= '9') { precision *= 10; precision += *fmt - '0'; }
            else { gather = 7; }
        }
        if (gather == 7) {
            if (*fmt >= '0' && *fmt <= '9') { precision *= 10; precision += *fmt - '0'; }
            else { gather = 8; }
        }
        if (gather == 8) {
            if (*fmt == 'h')      --length;
            else if (*fmt == 'l') ++length;
            else                  gather = 9;
        }
        if (gather == 9) {
            if (use_prefix || precision) {
                fputs("vfprintf: no flag support yet, sorry\n", stderr);
                abort();
            }
            int sign_mode = force_sign ? 1 : space_sign ? -1 : 0;

            int diff = 0;
            if (*fmt == 'd' || *fmt == 'i' || *fmt == 'u') {
                long long val;
                if (length < -2 || length > 2) {
                    fputs("vfprintf: invalid length\n", stderr);
                    abort();
                }
                if (*fmt == 'u') {
                    if      (length <= 0) val = va_arg(args, unsigned int);
                    else if (length == 1) val = va_arg(args, unsigned long);
                    else if (length == 2) val = va_arg(args, unsigned long long);
                } else {
                    if      (length <= 0) val = va_arg(args, int);
                    else if (length == 1) val = va_arg(args, long);
                    else if (length == 2) val = va_arg(args, long long);
                }
                if (width && !ljust)
                    diff += __rjust(f, width, __long_long_length(val, sign_mode), zeropad);
                if (diff < 0)
                    return nout;
                diff += __print_long_long(f, val, sign_mode);

            } else if (*fmt == 'c') {
                if (length != 0) { fputs("vfprintf: %c with nonzero length\n", stderr); abort(); }
                char c = va_arg(args, int);
                if (width && !ljust)
                    diff += __rjust(f, width, 1, zeropad);
                if (diff < 0)
                    return nout;
                diff += fwrite(&c, 1, 1, f);

            } else if (*fmt == 's') {
                if (length != 0) { fputs("vfprintf: %s with nonzero length\n", stderr); abort(); }
                const char *string = va_arg(args, const char *);
                int len = strlen(string);
                if (width && !ljust)
                    diff += __rjust(f, width, len, zeropad);
                if (diff < 0)
                    return nout;
                diff += fwrite(string, 1, len, f);
            } else {
                char tmp[2] = { *fmt, 0 };
                fputs("vfprintf: unknown format specifier: \n", stderr);
                fputs(tmp, stderr);
                fputs("\n", stderr);
                abort();
            }
            if (diff < 0)
                return nout;

            if (width && ljust) {
                for (int i = 0; i < width - diff; ++i) {
                    fputc(zeropad ? '0' : ' ', f);
                    if (ferror(f))
                        return nout + i;
                }
                diff += width - diff;
            }
            nout += diff;
            gather = 0;
            continue;
        }
        if (gather == 0) {
            nout += fwrite(fmt, 1, 1, f);
        }
    }
    return nout;
}

int fprintf(FILE *f, const char *fmt, ...)
{
    va_list list;
    va_start(list, fmt);
    int nout = vfprintf(f, fmt, list);
    va_end(list);
    return nout;
}

int vprintf(const char *fmt, va_list list)
{
    return vfprintf(stdout, fmt, list);
}

int printf(const char *fmt, ...)
{
    va_list list;
    va_start(list, fmt);
    int nout = vprintf(fmt, list);
    va_end(list);
    return nout;
}

int vsnprintf(char *buf, size_t n, const char *fmt, va_list args)
{
    if (buf == NULL && n == 0) {
        // TODO: This could be more efficient
        FILE *f = open_memstream(&buf, &n);
        int nout = vfprintf(f, fmt, args);
        fclose(f);
        return nout;
    }
    FILE *f = fmemopen(buf, n, "r+");
    int nout = vfprintf(f, fmt, args);
    int err = ferror(f);
    fclose(f);
    if (err || nout >= n-1) {
        buf[n-1] = 0;
        return n-1;
    }
    buf[nout] = 0;
    return nout;
}

int snprintf(char *buf, size_t n, const char *fmt, ...)
{
    va_list list;
    va_start(list, fmt);
    int nout = vsnprintf(buf, n, fmt, list);
    va_end(list);
    return nout;
}

int vsprintf(char *buf, const char *fmt, va_list args)
{
    return vsnprintf(buf, 1ULL<<31, fmt, args);
}

int sprintf(char *buf, const char *fmt, ...)
{
    va_list list;
    va_start(list, fmt);
    int nout = vsprintf(buf, fmt, list);
    va_end(list);
    return nout;
}
