#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void rbp_bump_2(int z) {
    for(int cz=1; cz < z; cz++) {
        int x[cz];
        x[cz / 2] = 8;
    }
}

void rbp_bump_1(int y) {
    for(int cy=1; cy < y; cy++) {
        int x[cy];
        x[cy / 2] = 8;
        rbp_bump_2(x[cy/2]);
    }
}

int main(int argc, char** argv) {
    if(argc < 2) {
        fprintf(stderr, "Missing argument: n\n");
        return 1;
    }
    int num = atoi(argv[1]);
    rbp_bump_1(num);
    return 0;
}
