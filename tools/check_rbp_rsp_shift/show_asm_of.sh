#!/bin/bash

grep "(E)" | while read line; do
    elf=$(echo "$line" | cut -d' ' -f1 | sed 's/\[\(.*\)\]/\1/g')
    addr=$(echo "$line" | sed 's/^.*0x\([0-9a-fA-F]*\):.*$/\1/g')

    echo "$line"
    objdump -d "$elf" | grep -C 1 -e "^ *$addr:"
    echo "-----"
done
