OCAMLBUILD=bapbuild -no-hygiene
BAPBUNDLE=bapbundle
ROOT_MODULE=dwarfsynth


all: install

.PHONY: $(ROOT_MODULE).plugin
$(ROOT_MODULE).plugin:
	$(OCAMLBUILD) $(ROOT_MODULE).plugin

.PHONY: install
install: $(ROOT_MODULE).plugin
	$(BAPBUNDLE) install $<

.PHONY: clean
clean:
	rm -rf _build
