#define B break
#define D default
#define E else
#define F(n) for(L _n=(n),i=0;i<_n;i++)
#define FS(s) for(C c,*_p=s;(c=*_p);_p++)
#define J if
#define Q case
#define R return
#define S static
#define T typedef
#define W while
#define Y switch
#define Z sizeof
T void V;T char C;T int I;T long long L;

//syscalls
#include<syscall.h>
#define STR(x) #x
#define XSTR(x) STR(x)
#define  H(f,...) L f(__VA_ARGS__);asm(#f":               mov $"XSTR(__NR_##f)",%rax\nsyscall\nret");//<4 args
#define H4(f,...) L f(__VA_ARGS__);asm(#f":mov %rcx,%r10\nmov $"XSTR(__NR_##f)",%rax\nsyscall\nret");//4+ args
H(read,I,V*,I)H(write,I,C*,I)H(open,C*,I,I)H(close,I)H(fstat,I,V*)H4(mmap,V*,L,I,I,I,I)H(munmap,V*,L)
#undef STR
#undef XSTR
#undef H
#undef H4
V exit(I);asm("exit:mov $60,%rax\nsyscall");

//utils
#define ee(m,c){J(c){write(2,"'"m"\n",2+Z(m));exit(1);}} //error
S L min(L x,L y){R x>y?x:y;}
S L max(L x,L y){R x>y?x:y;}
S L abs(L x){R x>0?x:-x;}
S L len(C*x){C*p=x;W(*x)x++;R x-p;}
S C hex(L x){      R x+(x> 9 ?'a'-10:'0');}
S L unh(C x){x|=32;R x-(x>'9'?'a'-10:'0');}
S V ps(C*x){write(2,x,len(x));} //for debugging
S V ph(L x){C s[17];s[16]=0;F(16){s[15-i]=hex(x&15);x>>=4;}write(2,s,17);}
#define pv(x) pv1(#x":",(L)(x))
S L pv1(C*s,L x){write(2,s,len(s));ph((L)x);write(2,"\n",1);R x;}
S V pm(V*x,V*y){
  ph((L)x);ps(":");C s[3];*s=' ';
  for(V*p=x;p<y;p++){L v=*(C*)p;s[1]=hex((v>>4)&15);s[2]=hex(v&15);write(2,s,3);
                     J(!((L)p&31)){write(2,"\n",1);}E J(!((L)p&7)){write(2," ",1);}}
  write(2,"\n",1);
}

//array header
T struct SA{L r,t,n,L[0];struct SA*A[0];C C[0];}*A; //r:refcount,t:type,n:length,L A C:pointers to data
#define xr (x->r)
#define yr (y->r)
#define zr (z->r)
#define xn (x->n)
#define yn (y->n)
#define zn (z->n)
#define xt (x->t)
#define yt (y->t)
#define zt (z->t)
#define xL (x->L)
#define yL (y->L)
#define zL (z->L)
#define xA (x->A)
#define yA (y->A)
#define zA (z->A)
#define xC (x->C)
#define yC (y->C)
#define zC (z->C)

//memory manager (simplest possible implementation -- memory never reclaimed)
S V*mp0,*mp;                                                              //pointer to free memory
S V mi(){mp0=mp=(V*)mmap(0,1L<<46,3,0x4022,-1,0);ee("mm",(L)mp<0);}       //init
S V mc(V*x,V*y,L z){C*p=x,*q=y;F(z)*p++=*q++;}                            //memcpy
S V ms(V*x,C y,L z){C*p=x;F(z)*p++=y;}                                    //memset
S L mz(A x){R(max(1,xn)*Z(L));}                                           //array size
S A ma(C t,L n){A x=mp;xr=1;xt=t;xn=n;mp+=Z(*x)+mz(x);R x;}               //allocate
S V mf(A x){J(--xr)R;J(!xt)F(max(1,xn))mf(xA[i]);ms(x,0xaa,Z(*x)+mz(x));} //free
S A mh(A x){xr++;R x;}                                                    //hold (inc refcount)

//constants
S A ca0,cl0,cc0,cy0,cc[256],cv[128][2];
S V ci(){ //init
  cl0=ma( 6,0);*cl0->L=0;       //!0
  cc0=ma(10,0);*cc0->C=' ';     //""
  cy0=ma(11,0);*cy0->L=0;       //0#`
  ca0=ma( 0,0);*ca0->A=mh(cc0); //()
  F(256){A x=cc[i]=ma(-10,1);*xC=i;} //chars
  ms(cv,0,Z(cv));
  FS("!#$%&*+,<=>?@^_|~:.-")F(2){A x=cv[c][i]=ma(107-i,2-i);*xC=c;} //verbs
  FS("'\\/"                )F(2){A x=cv[c][i]=ma(108  ,  1);*xC=c;} //adverbs
}

//parser
S C*s0,*s; //k source
S L ltr(C x){x|=32;R'a'<=x&&x<='z';} //letter?
S L dgt(C x){R'0'<=x&&x<='9';}       //digit?
S L ldg(C x){R ltr(x)||dgt(x);}      //alphanumeric?
S L num(C*x){R dgt(*x)||(*x=='.'&&dgt(x[1]))||((*x=='-'&&(dgt(x[1])))||(x[1]=='.'&&dgt(x[2])));} //is number start?
S C esc(C x){Y(x){Q'\0':R'0';Q'\n':R'n';Q'\r':R'r';Q'\t':R't';Q'"':R'"';D:R 0;}}
S C une(C x){Y(x){Q'0':R'\0';Q'n':R'\n';Q'r':R'\r';Q't':R'\t';D:R x;}}
S A addL(A x,L y){A z=ma(xt,xn+1);mc(zL,xL,mz(x));zL[xn]=y;mf(x);R z;}
S A addC(A x,C y){A z=ma(xt,xn+1);mc(zL,xL,mz(x));zC[xn]=y;mf(x);R z;}
S A a1(A x        ){A r=ma(0,1);*r->A=mh(x);                            R r;} //singleton
S A a2(A x,A y    ){A r=ma(0,2);*r->A=mh(x);r->A[1]=mh(y);              R r;} //pair
S A a3(A x,A y,A z){A r=ma(0,3);*r->A=mh(x);r->A[1]=mh(y);r->A[2]=mh(z);R r;} //triplet
S A mon(A x){R xt==107&&xn==2?cv[*xC][1]:x;} //monadic version of verb, eg + -> +:
S V ep(L x){J(!x)R;C*p=s,*q=s;W(p>s0&&p[-1]!='\n')p--;W(*q&&*q!='\n')q++;write(2,p,q-p);C b[256];*b='\n'; //parse error
            L k=min(s-p,Z(b));F(k)b[i+1]='_';b[k+1]='^';b[k+2]='\n';write(2,b,k+3);ee("parse",1);}
S A prs(){ //parse
  A t[64];L n=0,g=0; //t:sequence of nouns/verbs, n:how many, g:bitset of grammatical categories (0=noun,1=verb)
  W(1){
    A x=0;L gx=0;ep(0x80&*s);W(*s==' ')s++;J(*s=='/')W(*s&&*s!='\n')s++;C c=*s; //skip whitespace and comments
    J(!c)B; //eof?
    E J(ltr(c)){x=ma(-11,1);L v=*s++,h=8;W(ldg(*s)){v|=(L)*s++<<h;h+=8;}*xL=v;}
    E J(c=='`'){x=mh(cy0);W(*s=='`'){s++;L v=0,h=0;J(ltr(*s))W(ldg(*s)){v|=(L)*s++<<h;h+=8;}x=addL(x,v);}}
    E J(c=='"'){x=mh(cc0);s++;W(*s&&*s!='"')J(*s=='\\'){s++;ep(!*s);x=addC(x,une(*s++));}E{x=addC(x,*s++);}
                ep(!*s);s++;J(xn==1)xt=-xt;}
    E J(num(s)&&(*s!='-'||s==s0||(!ldg(s[-1])&&s[-1]!=')'))){
                x=mh(cl0);W(1){I m=*s=='-';s+=m;L v=0;W(dgt(*s))v=10*v+(*s++-'0');
                               x=addL(x,m?-v:v);J(*s!=' '||!num(s+1))B;s++;}
                J(xn==1)xt=-xt;}
    E J(c<127&&cv[c][0]){I u=s[1]==':';x=mh(cv[c][u]);s+=1+u;gx=1;}
    E{ep(1);R 0;}
    J(!x)B;t[n++]=x;g=g<<1|gx;
  }
  if(!n||(g&1)){ee("nyi",1);R 0;}
  A z=t[--n];g>>=1;W(n){J(n>1&&(g&3)==1){z=a3(t[n-1],t[n-2],z);n-=2;g>>=2;}E{z=a2(mon(t[--n]),z);g>>=1;}}R z;
}

//eval&apply
S A apply(A x){
  A y=*xA;
  J(yt==107){
    Y(*yC){
      Q'-':{J(xn!=3){ee("rank",1);R 0;}
            J(abs(xA[1]->t)!=6||abs(xA[2]->t)!=6){ee("type",1);R 0;}
            A z=ma(-6,1);*zL=*xA[1]->L-*xA[2]->L;R z;}
    }
  }
  ee("nyi",1);R 0;
}
S A eval(A x){
  J(xt==-11){ee("nyi-var",1);R 0;}
  J(xt==11&&xn==1){A z=ma(-11,1);*zL=*xL;R z;}
  J(xt||!xn)R mh(x);
  A y=ma(0,xn);F(xn)yA[i]=eval(xA[i]);R apply(y);
}

//output
S C ob[0x400];S L on=0;                    //buffer
S V ofl(){J(on){write(1,ob,on);on=0;}}     //flush
S V oC(C x){J(on==Z(ob))ofl();ob[on++]=x;} //output char
S V oS(C*x,L n){J(n>Z(ob)-on){ofl();write(1,x,n);}E{mc(ob+on,x,n);on+=n;}} //string
S V oL(L x){C b[32],*u=b+31;I m=x<0;J(m)x=-x;do{*u--='0'+x%10;x/=10;}W(x);J(m)*u--='-';oS(u+1,b+31-u);} //output number
S V oA(A x){Y(abs(xt)){                    //output array
  Q 6:J(xn){F(xn){J(i)oC(' ');oL(xL[i]);}}E{oS("!0",2);}B;
  Q 10:{oC('"');F(xn){C c=esc(xC[i]);J(c){oC('\\');oC(c);}E{oC(xC[i]);}}oC('"');B;}
  Q 11:F(xn){oC('`');L v=xL[i];W(v){oC(v&0xff);v>>=8;}}B;
  D:oS("???",3);B;
}}
S V out(A x){oA(x);oC('\n');ofl();}

//main
S V exec(C*x){J(*x=='\\'&&!x[1])exit(0);s=s0=x;A t=prs(),r=eval(t);mf(t);out(r);mf(r);}
asm(".globl _start\n_start:pop %rdi\nmov %rsp,%rsi\njmp go");
V go(I ac,C**av){
  mi();ci();
  J(av[1]){ //file.k
    I f=open(av[1],0,0);ee("open",f<0);L h[18];L r=fstat(f,h);ee("fstat",r);L n=h[6];ee("empty",!n);
    C*s=(C*)mmap(0,n,3,0x4002,f,0);ee("mmap",s==(V*)-1);r=close(f);ee("close",r);ee("eof",s[n-1]!='\n');
    s[n-1]=0;exec(s);r=munmap(s,n);ee("munmap",r);
  }
  C b[256];I nb=0,k; //repl:
  W((k=read(0,b,256-nb))>0){C*p=b,*q=b+nb,*r=q+k;W(q<r){J(*q=='\n'){*q=0;exec(p);p=q+1;}q++;}mc(b,p,nb=q-p);}
  exit(0);
}
