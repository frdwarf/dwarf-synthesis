OCAMLBUILD=bapbuild -no-hygiene
BAPBUNDLE=bapbundle
ROOT_MODULE=dwarfsynth


all: install ml_dwarf_write.bin

.PHONY: ml_dwarf_write.bin
ml_dwarf_write.bin:
	$(MAKE) -C DwarfSynth/c_bindings
	ln -fs DwarfSynth/c_bindings/ml_dwarf_write.bin .

.PHONY: $(ROOT_MODULE).plugin
$(ROOT_MODULE).plugin:
	$(OCAMLBUILD) $(ROOT_MODULE).plugin

.PHONY: install
install: $(ROOT_MODULE).plugin
	$(BAPBUNDLE) install $<

.PHONY: clean
clean:
	rm -rf _build
