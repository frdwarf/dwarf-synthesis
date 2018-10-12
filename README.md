# dwarf-synthesis

A tool for automatic synthesis of DWARF.

The purpose of this tool is to take any given binary program or library,
examine its assembly code and, based solely on that, generate the corresponding
`.eh_frame` DWARF data.

## Dependencies

This tool relies on [BAP](https://github.com/BinaryAnalysisPlatform/bap), which
is available through OPAM.
