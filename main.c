#include <stdio.h>

unsigned long long factorial(unsigned long long n)
{
    if (n == 0)
        return 1;
    return factorial(n - 1) * n;
}

int main(int argc, char **argv)
{
    printf("command-line parameters:\n");
    for (int i = 0; i < argc; ++i)
        printf("%-2d | %s\n", i, argv[i]);
    printf("========================\n");
    printf("factorial output:\n");
    for (int i = 0; i <= 20; ++i)
        printf("%3d! = %llu\n", i, factorial(i));
    return 0;
}
