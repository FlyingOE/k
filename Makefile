CC=clang -Wall -ferror-limit=3 -ffreestanding -nostdlib -fno-builtin -static -fno-unwind-tables \
         -fno-asynchronous-unwind-tables -fno-unroll-loops
b: #build
	@mkdir -p _
	@$(CC) k.c -O3 -o _/k
	@strip -R .comment -R '.note*' _/k
	@stat -c 'size:%s' _/k
c: #clean
	@rm -rf _
