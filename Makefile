
all: 	
	@echo "Must specific platform: "
	@echo "    make linux"
	@echo "    make linux64"
	@echo "    make mac"
	@echo "    make win32"
	
	
linux:
	@make -f Makefile.linux
	
linux64:
	@make -f Makefile.linux64
	
mac:
	@make -f Makefile.mac
	
win32:
	@make -f Makefile.win32

clean:
	rm unmscheme unm_viewer unmscheme.o unm_viewer.o 
