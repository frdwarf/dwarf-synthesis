CDEFINE =
CFLAGS = -Wall -Wextra -std=c11 -O2 -g $(CDEFINE) -I$(OPAM_SWITCH_PREFIX)/lib/ocaml
LIBDWARFW_DIR=$(shell readlink -f ../../libdwarfw/build)
CLIBS = -L$(LIBDWARFW_DIR) -Wl,-rpath,$(LIBDWARFW_DIR) -lz -lelf -ldwarf -ldwarfw
#CLIBELF = /usr/lib/x86_64-linux-gnu/libelf.a
OCAMLCLIBS = -ccopt -L$(LIBDWARFW_DIR) -ccopt -Wl,-rpath,$(LIBDWARFW_DIR) -cclib -lz -cclib -lelf -cclib -ldwarf -cclib -ldwarfw


all: test_dw.bin ml_dwarf_write.bin

test_%.bin: test_%.o dwarf_write.o
	gcc $(CFLAGS) $^ $(CLIBELF) $(CLIBS) -o $@

%.o: %.c
	gcc $(CFLAGS) -c $< -o $@

ml_dwarf_write.bin: ml_dwarf_write.ml dwarf_write_stubs.c dwarf_write.o
	ocamlopt -c dwarf_write_stubs.c
	ocamlopt -o ml_dwarf_write.bin ml_dwarf_write.ml dwarf_write_stubs.o dwarf_write.o $(OCAMLCLIBS)

clean:
	rm -f *.o *.bin

.PRECIOUS: %.o 
