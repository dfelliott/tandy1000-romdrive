T1KDRV.SYS
==========

A DOS driver for the Tandy 1000 BIOS ROM drive.

This driver offers functionality equivalent to Tandy 1000 DOS to other DOS
versions.  In particular, it supports launching DeskMate from ROM.

It is known to work on the following models:
  * 1000 TL, built-in DOS 3.30.20, real hardware and 86Box
  * 1000 SL/2, built-in DOS 3.30.30, 86Box

It may work with other machines having ROM drives like the 1000 HX.

The following DOS versions have been tested:
  * MS-DOS 5 (retail)
  * Compaq DOS 3.31

Successful testing with DOS 5 should cover all DOS >= 4, including DR/Novell.

In theory it can work with another vendor's DOS 3.30, but why bother?

It almost certainly will not work with DOS < 3.20 due to missing `DRIVER.SYS`
support.  Again, why bother?

## Technical Overview

The BIOS makes the ROM drive available as drive 8.  In theory, DOS's
`DRIVER.SYS` or `DRIVPARM` should be able to access it given the right
parameters.  In practice, I could not get this to work, so I started writing
my own driver.  The mechanism of action is identical to DOS's own.  Once the
right "BDS" (logical drive) structure is added, DOS itself implements all
calls as it would for any other BIOS drive.

This was enough to make the drive usable, but running DeskMate's `DESK.COM`
still printed the dreaded `Can't execute DESK from ROM`.  To make DeskMate
from ROM work, the word at `40:C2` must contain the 1-based DOS logical drive
number.  For example, if the ROM drive is `D:` then it must be `4`.  This
is used to find the `DESK.HID` file.  This word is updated by T1KDRV at the
end of initialization.

This is probably enough to make it work, but I was having issues in 86Box that
were ultimately caused by a bug in 86Box.  As part of the investigation I
discovered that Tandy DOS hooks INT 13h to patch a bug in the "Get Drive
Parameters" (AH=08h) and "Get Disk Type" (AH=15h) functions for the ROM drive.
Specifically, they fail to restore the page.  In actual practice this may not
matter, but I figured better safe than sorry.

T1KDRV does all of these things for you.

## Masked files

Running `DIR /A` reveals multiple files with the name `AAAAAAAA.AAA`.  There
are two independent reasons for this:
  * The `SETUPTL /A` or similar program has been used to disable them.
  * They are for the wrong DOS version.

This masking is implemented in the BIOS INT 13h read sector code.  Why it marks
the files hidden and overwrites the names to all `A` is a bit of a mystery.  It
may be that because the FAT clusters remain allocated, an ordinary delete might
leave the volume failing `CHKDSK`.  Surprisingly, DOS doesn't seem to choke on
three (or more) files having the same name.

The EEPROM value is directly read by the BIOS code for the first set of files,
but the second set is based on the word at `40:C4` which is ordinarily `0` but
set by Tandy DOS to bytes `3` and `30` (LSB, MSB).  This is checked against
reserved data in the directory entries of these files.

It is not useful to have the `FORMAT.COM`, `DISKCOPY.COM` and `COMMAND.COM`
from Tandy DOS 3.30 when running any other version of DOS, so no attempt
is made to set the word at `40:C4`.

Feel free to experiment, just note that DOS has buffers so you have
to poke the value _before_ accessing the ROM drive.
