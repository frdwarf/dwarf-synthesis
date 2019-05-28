OCAMLBUILD=bapbuild -no-hygiene
BAPBUNDLE=bapbundle
ROOT_MODULE=dwarfsynth
TARBALL=dwarfsynth.tar.gz

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

###############################################################################
.PHONY: install
install: $(ROOT_MODULE).plugin
	$(BAPBUNDLE) install $<

###############################################################################
.PHONY: clean
clean:
	rm -rf _build

###############################################################################
tarball: $(TARBALL)

.PHONY: $(TARBALL)
$(TARBALL):
	tar czf $(TARBALL) \
		--exclude=.git \
		--exclude=.gitignore \
		--exclude=libdwarfw/build \
		--exclude-backups \
		--exclude=*.bck \
		--exclude=*.bin \
		--exclude=*.o \
		--exclude=*.cmi \
		--exclude=*.cmx \
		--transform='s#^#dwarfsynth/#g' \
		DwarfSynth dwarfsynth.ml DwarfSynth.mlpack libdwarfw LICENSE Makefile \
		README.md synthesize_dwarf.sh _tags
