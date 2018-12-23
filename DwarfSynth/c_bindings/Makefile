CDEFINE =
CFLAGS = -Wall -Wextra -std=c11 -O2 -g $(CDEFINE)
LIBDWARFW_DIR=../../libdwarfw/build
CLIBS = -L$(LIBDWARFW_DIR) -Wl,-rpath,$(LIBDWARFW_DIR) -lelf -ldwarf -ldwarfw

all: test_dw.bin

test_%.bin: test_%.o dwarf_write.o
	gcc $(CFLAGS) $(CLIBS) $^ -o $@

%.o: %.c
	gcc $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o test_*.bin
