#include <stdlib.h>
#include <stdio.h>
#include "dwarf_write.h"

int main() {
    struct pre_dwarf_entry entries[3] = {
        { 0x1300, DW_REG_RSP, 8 },
        { 0x1310, DW_REG_RSP, 16 },
        { 0x1340, DW_REG_RSP, 8 }
    };
    struct pre_dwarf_fde fde = {
        3, entries, 0x1300, 0x1342
    };
    struct pre_dwarf dwarf_data = {1, &fde};

    write_dwarf("test.bin", &dwarf_data);
    return 0;
}
