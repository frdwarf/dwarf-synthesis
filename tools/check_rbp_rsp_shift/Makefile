TARGETS:= \
	$(shell find /usr/bin -executable) \
	$(shell find /usr/lib -executable)

all: $(TARGETS:=.elf)

%.elf:
	@readelf -wF "$*" 2>/dev/null | ./check_rbp_rsp_shift.py "$*"
