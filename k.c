#define F(n,b) {L _n=(n),i=0;for(;i<_n;i++){b;}}
#define J if
#define E else
#define R return
#define S static
#define SZ sizeof
#define TD typedef
#define W while
TD void V;TD char C;TD int I;TD long long L;

//syscalls
#include<syscall.h>
#define XSTR(x) STR(x)
#define STR(x) #x
#define H(f,...) L f(__VA_ARGS__);asm(#f":               mov $"XSTR(__NR_##f)",%rax\nsyscall\nret");//<4 args
#define G(f,...) L f(__VA_ARGS__);asm(#f":mov %rcx,%r10\nmov $"XSTR(__NR_##f)",%rax\nsyscall\nret");//4+ args
H(read,I,V*,I)H(write,I,C*,I)H(open,C*,I,I)H(close,I)H(fstat,I,V*)G(mmap,V*,L,I,I,I,I)H(munmap,V*,L)
#undef H
V exit(I);asm("exit:mov $60,%rax\nsyscall");

//utils
#define ee(m,c){J(c){write(2,"'"m"\n",2+SZ(m));exit(1);}}

//memory
S V*mp;
S V m0(){mp=(V*)mmap(0,1L<<46,3,0x4022,-1,0);ee("mm",(L)mp<0);}
S V mc(V*x,V*y,L z){C*p=x,*q=y;F(z,*p++=*q++);}
S V ms(V*x,C y,L z){C*p=x;F(z,*p++=y);}
S V*ma(L x){*(L*)mp=x;mp+=x+8;R mp-x;}
S V mf(V*x){L n=*(L*)x-1;ms(x-8,0xaa,n+8);}
S L mz(V*x){R*((L*)x-1);}
S V*mr(V*x,L n){J(n<=mz(x)){*((L*)x-1)=n;R x;}V*r=ma(n);mc(r,x,mz(x));mf(x);R r;}

//arrays
TD struct{L r,n;C t;}*A;

//main
S V exec(C*x){J(*x=='\\'&&!x[1])exit(0);write(1,"pong\n",5);}
asm(".globl _start\n_start:pop %rdi\nmov %rsp,%rsi\njmp go");
V go(I ac,C**av){
  m0();
  J(av[1]){I f=open(av[1],0,0);ee("open",f<0);L h[18];L r=fstat(f,h);ee("fstat",r);L n=h[6];ee("empty",!n);
           C*s=(C*)mmap(0,n,3,0x4002,f,0);ee("mmap",s==(V*)-1);r=close(f);ee("close",r);ee("eof",s[n-1]!='\n');
           s[n-1]=0;exec(s);r=munmap(s,n);ee("munmap",r);}
  C b[256];I nb=0,k;
  W((k=read(0,b,256-nb))>0){C*p=b,*q=b+nb,*r=q+k;W(q<r){J(*q=='\n'){*q=0;exec(p);p=q+1;}q++;}mc(b,p,nb=q-p);}
  exit(0);}
