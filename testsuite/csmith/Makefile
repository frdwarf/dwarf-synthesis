ORIGBIN=$(shell find . -name '*.orig.bin')
BASES=$(ORIGBIN:.orig.bin=)

TEST_TARGETS=$(addsuffix -check, $(BASES))

CHECK=../../csmith/check_generated_eh_frame.sh
SYNTH=../../synthesize_dwarf.sh

all: $(TEST_TARGETS)

%-check: %.eh.bin
	@$(CHECK) "$*" || true

.PRECIOUS: %.eh.bin
%.eh.bin: %.bin
	BAP_ARGS='--dwarfsynth-no-rbp-undef' $(SYNTH) "$<" "$@"

clean-synth:
	rm -f $(BASES:=.eh.bin)
