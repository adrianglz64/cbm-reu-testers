# Optimized 1764 RAMTEST 

Optimized version of the REU test program from the [1764 RAM expansion utility disk](https://www.zimmers.net/anonftp/pub/cbm/demodisks/other/1764-utility.d64.gz).

Optimizations mostly affect tests 3 and 4 which used some very inefficient code to fill and verify memory.  A memory fill routine used for test 2 was also optimized.

The test now runs continuously instead of waiting for a keypress when finished.

Assemble with DASM:

`dasm 1764patched.asm -o1764patched.bin`

Load it as a binary on the C64:

`LOAD "1764PATCHED.BIN",8,1`

and run it using

`SYS 32768`

