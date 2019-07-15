#include <stdio.h>

int fac(int n) {
    if(n <= 0)
        return 0;
    if(n == 1)
        return 1;
    return n * fac(n-1);
}

int main(void) {
    printf("%d\n", fac(4));
    return 0;
}
