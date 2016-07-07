CC=clang -Wall -Wno-unused-function -ffreestanding -nostdlib -static -fno-builtin -fno-unwind-tables \
         -fno-asynchronous-unwind-tables -fno-unroll-loops -ferror-limit=3
b: #build
	@mkdir -p _
	@$(CC) k.c -O3 -o _/k
	@strip -R .comment -R '.note*' _/k
	@stat -c 'size:%s' _/k
c: #clean
	@rm -rf _
