# dwarf-synthesis

A tool for automatic synthesis of DWARF.

The purpose of this tool is to take any given binary program or library,
examine its assembly code and, based solely on that, generate the corresponding
`.eh_frame` DWARF data.

## Dependencies

This tool relies on [BAP](https://github.com/BinaryAnalysisPlatform/bap), which
is available through OPAM.

## Running

First, run `make` to compile and install the BAP plugin.

### Running the BAP plugin

`bap prog_to_analyze.bin -p dwarfsynth --dwarfsynth-output tmp.marshal`

### Running `ml_dwarf_write`

You can get a help text with `./ml_dwarf_write.bin`. Otherwise, you can run

```
./ml_dwarf_write.bin tmp.marshal prog_to_analyze.bin eh_frame_section`
```

### Stitching the section into a binary

```
objcopy --add-section .eh_frame=eh_frame_section prog_to_analyze.bin prog_to_analyze.eh.bin
```

## Commonly used commands

### List a binary sections

`objdump -h blah.bin`

### Strip a binary of its `eh_frame`

`objcopy --remove-section '.eh_frame' --remove-section '.eh_frame_hdr' blah.bin`
