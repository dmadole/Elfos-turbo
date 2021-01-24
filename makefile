PROJECT = turbo

$(PROJECT).prg: $(PROJECT).asm bios.inc
	rcasm -l -v -x -d1802 $(PROJECT) 2>&1 | tee $(PROJECT).lst
	hextobin $(PROJECT)

clean:
	-rm -f $(PROJECT).bin
	-rm -f $(PROJECT).lst
	-rm -f $(PROJECT).prg


