#define TD typedef
TD void V;TD char C;TD int I;TD long long L;

//syscalls
#include<syscall.h>
#define XSTR(x) STR(x)
#define STR(x) #x
#define H(f,...) L f(__VA_ARGS__);asm(#f":mov $"XSTR(__NR_##f)",%rax\nsyscall\nret");
H(read,I,V*,I)H(write,I,C*,I)
#undef H
V exit(I);asm("exit:mov $60,%rax\nsyscall");

//main
asm(".globl _start\n_start:pop %rdi\nmov %rsp,%rsi\njmp go");
V go(I ac,C**av){
  write(1,"hello\n",6);
  exit(0);
}
