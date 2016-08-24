O=-Wall -Wno-unused-function -Wno-char-subscripts -nostdlib -static\
	-ffreestanding -fno-builtin -fno-unwind-tables -fno-asynchronous-unwind-tables -fno-unroll-loops -ferror-limit=3\
	-Oz k.c
t:b #test
	@./t.sh
b: #build
	@clang $(O) -o k
	@strip -R .comment -R '.note*' k
	@stat -c 'size:%s' k
a: #asm
	@clang $(O) -o k.s -masm=intel -S
