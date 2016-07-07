#define F(n,b) {L _n=(n),i=0;for(;i<_n;i++){b;}}
#define R return
#define S static
#define TD typedef
TD void V;TD char C;TD int I;TD long long L;

//syscalls
#include<syscall.h>
#define XSTR(x) STR(x)
#define STR(x) #x
#define H(f,...) L f(__VA_ARGS__);asm(#f":               mov $"XSTR(__NR_##f)",%rax\nsyscall\nret");//<4 args
#define G(f,...) L f(__VA_ARGS__);asm(#f":mov %rcx,%r10\nmov $"XSTR(__NR_##f)",%rax\nsyscall\nret");//4+ args
H(read,I,V*,I)H(write,I,C*,I)G(mmap,V*,L,I,I,I,I)H(munmap,V*,L)
#undef H
V exit(I);asm("exit:mov $60,%rax\nsyscall");

//memory
S V*mp;
S V m0(){mp=(V*)mmap(0,1L<<46,3,0x4022,-1,0);if((L)mp<0)exit(1);}
S V mc(V*x,V*y,L z){C*p=x,*q=y;F(z,*p++=*q++);}
S V ms(V*x,C y,L z){C*p=x;F(z,*p++=y);}
S V*ma(L x){*(L*)mp=x;mp+=x+8;R mp-x;}
S V mf(V*x){L n=*(L*)x-1;ms(x-8,0xaa,n+8);}
S L mz(V*x){R*((L*)x-1);}
S V*mr(V*x,L n){if(n<=mz(x)){*((L*)x-1)=n;R x;}V*r=ma(n);mc(r,x,mz(x));mf(x);R r;}

//arrays
TD struct{L r,n;C t;}*A;

//main
asm(".globl _start\n_start:pop %rdi\nmov %rsp,%rsi\njmp go");
V go(I ac,C**av){m0();write(1,"hello\n",6);exit(0);}
