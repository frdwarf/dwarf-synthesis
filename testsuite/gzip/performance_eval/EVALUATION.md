# Synthesis of gzip, statically linked

* `.text` section: 698882B (ie. ~0.7M)

## Uncached bap, 10 runs

bap startup: 39.524±0.988 sec
dwarfsynth generation: 3.833±0.153 sec
dwarfsynth cleaning: 0.085±0.007 sec
write DWARF table: 0.011±0.001 sec
insert DWARF table in binary: 0.006±0.001 sec
Total: 43.459±1.132 sec

## Cached bap, 100 runs

bap startup: 1.700±0.061 sec
dwarfsynth generation: 3.827±0.135 sec
dwarfsynth cleaning: 0.078±0.005 sec
write DWARF table: 0.012±0.001 sec
insert DWARF table in binary: 0.006±0.001 sec
Total: 5.622±0.183 sec
