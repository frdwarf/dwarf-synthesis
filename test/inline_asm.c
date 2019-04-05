#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    if(argc < 2) {
        fprintf(stderr, "Missing argument: loop upper bound.\n");
        exit(1);
    }

    int upper_bound = atoi(argv[1]);
    int count = 0;
    for(int i=0; i < upper_bound; ++i) {
        __asm__("sub $8, %rsp; movq $42, (%rsp)");
        count++;
        __asm__("add $8, %rsp");
    }
    printf("%d\n", count);
    return 0;
}
