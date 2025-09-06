# 1764 RAMTEST

REU test program from the [1764 RAM expansion utility disk](https://www.zimmers.net/anonftp/pub/cbm/demodisks/other/1764-utility.d64.gz).

Assemble with DASM:

`dasm 1764ramtest.asm -o1764ramtest.bin`

Load it as a binary on the C64:

`LOAD "1764RAMTEST.BIN",8,1`

and run it using

`SYS 32768`

