/** dwarf_write -- set of functions to add a given DWARF table to an ELF file
 *
 * Mostly based on work by emersion on Dareog
 * <https://github.com/emersion/dareog>
 */

#pragma once

#define _XOPEN_SOURCE 700

#include "../../libdwarfw/include/dwarfw.h"

#include <libdwarf/dwarf.h>
#include <gelf.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <libelf.h>
#include <limits.h>
#include <stddef.h>

typedef uint8_t reg_t;
typedef long long int offset_t;
typedef uintptr_t addr_t;


// ====== Registers definition for x86_64 ======
#define DW_REG_RA   ((reg_t)16)
#define DW_REG_RBP  ((reg_t)6)
#define DW_REG_RSP  ((reg_t)7)
#define DW_REG_INV  ((reg_t)0xff)

#define DW_MAX_REG  ((reg_t)31)

/// Very basic DWARF structure representing only CFA -- RA is at fixed offset
struct pre_dwarf_entry {
    addr_t location;
    reg_t cfa_offset_reg;
    offset_t cfa_offset;
};

struct pre_dwarf_fde {
    size_t num;
    struct pre_dwarf_entry* entries;

    addr_t initial_location, end_location;
};

struct pre_dwarf {
    size_t num_fde;
    struct pre_dwarf_fde* fdes;
};

/// Writes the provided `pre_dwarf` as DWARF in the ELF file at `obj_path`
int write_dwarf(char* obj_path, struct pre_dwarf* pre_dwarf);
