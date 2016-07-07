NASM=nasm
AR=ar
ASM_FILES=cobraruntime.asm
OBJ_FILES=$(patsubst %.asm, %.asm.o, $(ASM_FILES))
LIB=libcobraruntime.a

$(LIB): $(OBJ_FILES)
	@echo [AR] $<
	@$(AR) rcs $@ $(OBJ_FILES)

%.asm.o: %.asm
	@echo [NASM] $<
	@$(NASM) -g -felf64 $< -o $@

clean:
	@rm $(OBJ_FILES) $(LIB)
