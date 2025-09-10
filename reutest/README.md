# REU Test 

General purpose REU test program.   Based on the [optimized REU test program](../cbm-1764-patched/) originally from the [1764 RAM expansion utility disk](https://www.zimmers.net/anonftp/pub/cbm/demodisks/other/1764-utility.d64.gz).

Assemble with DASM:

`dasm reutest.asm -oreutest.prg`

Load it as a binary on the C64:

`LOAD "REUTEST.PRG",8,1`

and run it using

`SYS 32768`
