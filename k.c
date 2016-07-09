#define B break
#define DF default
#define F(n,b){L _n=(n),i=0;for(;i<_n;i++){b;}}
#define J if
#define E else
#define R return
#define S static
#define SW switch
#define SZ sizeof
#define TD typedef
#define Q case
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
S L min(L x,L y){R x>y?x:y;}
S L max(L x,L y){R x>y?x:y;}
S L abs(L x){R x>0?x:-x;}
S L len(C*x){C*p=x;W(*x)x++;R x-p;}
S C hex(L x){R(x>9?'a'-10:'0')+x;}
S L unh(C x){R x>='a'?x-'a'+10:x>='A'?x-'A'+10:x-'0';}
S V ps(C*x){write(2,x,len(x));}
S V ph(L x){C s[17];s[16]=0;F(16,{s[15-i]=hex(x&15);x>>=4;});write(2,s,17);}
#define pv(x) pv1(#x":",(L)(x))
S L pv1(C*s,L x){write(2,s,len(s));ph((L)x);write(2,"\n",1);R x;}
S V pm(V*x,V*y){
  ph((L)x);ps(":");C s[3];*s=' ';
  for(V*p=x;p<y;p++){L v=*(C*)p;s[1]=hex((v>>4)&15);s[2]=hex(v&15);write(2,s,3);
                     J(!((L)p&31)){write(2,"\n",1);}E J(!((L)p&7)){write(2," ",1);}}
  write(2,"\n",1);
}

//array header
TD struct{L z,r,n,t;}*A;
#define xr ((x)->r)
#define yr ((y)->r)
#define zr ((z)->r)
#define xn ((x)->n)
#define yn ((y)->n)
#define zn ((z)->n)
#define xt ((x)->t)
#define yt ((y)->t)
#define zt ((z)->t)
#define xA ((A*)(x+1))
#define yA ((A*)(y+1))
#define zA ((A*)(z+1))
#define xL ((L*)(x+1))
#define yL ((L*)(y+1))
#define zL ((L*)(z+1))

//memory
S V*mp0,*mp;
S V mi(){mp0=mp=(V*)mmap(0,1L<<46,3,0x4022,-1,0);ee("mm",(L)mp<0);}
S V mc(V*x,V*y,L z){C*p=x,*q=y;F(z,*p++=*q++);}
S V ms(V*x,C y,L z){C*p=x;F(z,*p++=y);}
S L mz(A x){R(max(1,xn)*SZ(L));}
S A ma(C t,L n){A x=mp;xr=1;xt=t;xn=n;mp+=SZ(*x)+mz(x);R x;}
S V mf(A x){J(--xr)R;J(!xt)F(max(1,xn),mf(xA[i]));ms(x,0xaa,SZ(*x)+mz(x));}

//parser
S L ltr(C x){x|=32;R'a'<=x&&x<='z';}
S L dgt(C x){R'0'<=x&&x<='9';}
S L ldg(C x){R ltr(x)||dgt(x);}
S L num(C*x){R dgt(*x)||(*x=='-'&&dgt(x[1]));}
S A addL(A x,L y){A z=ma(xt,xn+1);mc(zL,xL,mz(x));zL[xn]=y;mf(x);R z;}
S A prs(C*s){
  J(ltr(*s)){L v=*s,h=8;s++;W(ldg(*s)){v|=*s++<<h;h+=8;};A x=ma(-11,1);*xL=v;R x;}
  J(num(s)){A x=ma(6,0);
            W(1){I m=*s=='-';s+=m;L v=0;W(dgt(*s))v=10*v+(*s++-'0');x=addL(x,m?-v:v);J(*s!=' '||!num(s+1))B;s++;}
            J(xn==1)xt=-xt;R x;}
  ee("parse",1);R 0;
}

//output
#define oN 0x400
S C ob[oN];S L on=0;
S V ofl(){write(1,ob,on);on=0;}
S V oC(C x){J(on==oN)ofl();ob[on++]=x;}
S V oS(C*x,L n){W(n>oN-on){mc(ob+on,x,oN-on);x+=oN-on;n-=oN-on;ofl();}J(n){mc(ob+on,x,n);on+=n;}}
S V oL(L x){C b[32],*u=b+31;I m=x<0;J(m)x=-x;do{*u--='0'+x%10;x/=10;}W(x);J(m)*u--='-';oS(u+1,b+31-u);}
S V oA(A x){SW(abs(xt)){
  Q 6:J(xn){F(xn,{J(i)oC(' ');oL(xL[i]);});}E{oS("!0",2);}B;
  Q 11:F(xn,{oC('`');L v=xL[i];W(v){oC(v&0xff);v>>=8;}});B;
  DF:oS("???",3);B;
}}
S V out(A x){oA(x);oC('\n');ofl();}

//main
S V exec(C*x){J(*x=='\\'&&!x[1])exit(0);out(prs(x));}
asm(".globl _start\n_start:pop %rdi\nmov %rsp,%rsi\njmp go");
V go(I ac,C**av){
  mi();
  J(av[1]){I f=open(av[1],0,0);ee("open",f<0);L h[18];L r=fstat(f,h);ee("fstat",r);L n=h[6];ee("empty",!n);
           C*s=(C*)mmap(0,n,3,0x4002,f,0);ee("mmap",s==(V*)-1);r=close(f);ee("close",r);ee("eof",s[n-1]!='\n');
           s[n-1]=0;exec(s);r=munmap(s,n);ee("munmap",r);}
  C b[256];I nb=0,k;
  W((k=read(0,b,256-nb))>0){C*p=b,*q=b+nb,*r=q+k;W(q<r){J(*q=='\n'){*q=0;exec(p);p=q+1;}q++;}mc(b,p,nb=q-p);}
  exit(0);}
