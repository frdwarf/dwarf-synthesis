OCAMLBUILD=bapbuild -no-hygiene
BAPBUNDLE=bapbundle
ROOT_MODULE=dwarfsynth

LIBDWARFW_SO=libdwarfw/build/libdwarfw.so
LIBDWARFW_SO_MESON=libdwarfw/build/build.ninja


all: install ml_dwarf_write.bin

.PHONY: ml_dwarf_write.bin
ml_dwarf_write.bin: $(LIBDWARFW_SO)
	$(MAKE) -C DwarfSynth/c_bindings
	ln -fs DwarfSynth/c_bindings/ml_dwarf_write.bin .

.PHONY: $(LIBDWARFW_SO)
$(LIBDWARFW_SO):
	cd libdwarfw && test -d build || meson build
	ninja -C libdwarfw/build

.PHONY: $(ROOT_MODULE).plugin
$(ROOT_MODULE).plugin:
	$(OCAMLBUILD) $(ROOT_MODULE).plugin

.PHONY: install
install: $(ROOT_MODULE).plugin
	$(BAPBUNDLE) install $<

.PHONY: clean
clean:
	rm -rf _build
