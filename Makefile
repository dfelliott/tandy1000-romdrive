FILES=t1kdrv.img t1kdrv.sys


all: t1kdrv.img

clean:
	rm -f $(FILES)


%.sys: %.asm
	nasm -o $@ $<

%.img: %.sys
	dd if=/dev/zero bs=512 count=1440 of=$@
	mformat -i $@ -f 720
	mcopy -i $@ -m $< ::/
