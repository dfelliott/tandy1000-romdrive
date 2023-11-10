FILES=t1kdrv.img t1kdrv.sys empty.img

BASE_IMAGE=empty.img


all: t1kdrv.img

clean:
	rm -f $(FILES)


%.sys: %.asm
	nasm -o $@ $<

empty.img:
	dd if=/dev/zero bs=512 count=1440 of=$@
	mformat -i $@ -f 720

%.img: %.sys $(BASE_IMAGE)
	cp "$(BASE_IMAGE)" $@
	mcopy -i $@ -m $< config.sys autoexec.bat ::/
