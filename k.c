typedef void V;typedef char C;typedef int I;typedef long long L;
typedef struct SA{L r,t,n,L[0];struct SA*A[0];C C[0];}*A; //r:refcount,t:type,n:length,L A C:pointers to data
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
#define B break
#define D default
#define E else
#define F(n) for(L _n=(n),i=0;i<_n;i++)
#define FA(x,b) {Y((x)->t){Q 0:     F((x)->n){A u=(x)->A[i];b;}B;\
                           Q 6:Q 11:F((x)->n){A u=ma(-(x)->t,1);*(u)->L=(x)->L[i];b;mf(u);}B;\
                           Q 10:    F((x)->n){A u=cc[(x)->C[i]];b;}B;\
                           D:en();}}
#define FS(s) for(C c,*_p=s;(c=*_p);_p++)
#define J if
#define Q case
#define R return
#define S static
#define W while
#define Y switch
#define Z sizeof

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
S L min(L x,L y){R x<y?x:y;}
S L max(L x,L y){R x>y?x:y;}
S L abs(L x){R x>0?x:-x;}
S L len(C*x){C*p=x;W(*x)x++;R x-p;}
S C hex(L x){      R x+(x> 9 ?'a'-10:'0');}
S L unh(C x){x|=32;R x-(x>'9'?'a'-10:'0');}
S V ps(C*x){write(2,x,len(x));} //for debugging
S V ph(L x){C s[17];s[16]=0;F(16){s[15-i]=hex(x&15);x>>=4;}write(2,s,17);}
#define pv(x) pv1(#x":",(L)(x))
S L pv1(C*s,L x){write(2,s,len(s));ph((L)x);write(2,"\n",1);R x;}
S V pm(V*x,V*y){ph((L)x);ps(":");C s[3];*s=' ';
                for(V*p=x;p<y;p++){L v=*(C*)p;s[1]=hex((v>>4)&15);s[2]=hex(v&15);write(2,s,3);
                                   J(!((L)p&31)){write(2,"\n",1);}E J(!((L)p&7)){write(2," ",1);}}
                write(2,"\n",1);}

//error handling
#define er(m){write(1,"'"m"\n",2+Z(m));exit(1);}
#define en() er("nyi")
#define el() er("length")
#define et() er("type")

//memory manager (simplest possible implementation -- memory never reclaimed)
S V*mp0,*mp;                                                              //pointer to free memory
S V mi(){mp0=mp=(V*)mmap(0,1L<<46,3,0x4022,-1,0);J((L)mp<0)er("mm");}     //init
S V mc(V*x,V*y,L z){C*p=x,*q=y;F(z)*p++=*q++;}                            //memcpy
S V ms(V*x,C y,L z){C*p=x;F(z)*p++=y;}                                    //memset
S L mz(A x){R(max(1,xn)*Z(L));}                                           //array size
S A ma(C t,L n){A x=mp;xr=1;xt=t;xn=n;mp+=Z(*x)+mz(x);R x;}               //allocate
S V mf(A x){J(--xr)R;J(!xt)F(max(1,xn))mf(xA[i]);ms(x,0xaa,Z(*x)+mz(x));} //free
S A mh(A x){xr++;R x;}                                                    //hold (inc refcount)

//constants
S A ca0,cl0,cc0,cy0,cd0,cc[256],cv[128][2];
S V ci(){ //init
  A x=cl0=ma( 6,0);*xL=0;       //!0
    x=cc0=ma(10,0);*xC=' ';     //""
    x=cy0=ma(11,0);*xL=0;       //0#`
    x=ca0=ma( 0,0);*xA=mh(cc0); //()
    x=cd0=ma(99,2);*xA=mh(cy0);xA[1]=mh(ca0); //(0#`)!()
  F(256){x=cc[i]=ma(-10,1);*xC=i;} //chars
  ms(cv,0,Z(cv));
  F(2)FS("!#$%&*+,<=>?@^_|~:.-"){x=cv[c][i]=ma(107-i,2-i);*xC=c;} //verbs
  F(2)FS("'\\/"                ){x=cv[c][i]=ma(108  ,  1);*xC=c;} //adverbs
}

//basic array operations
S A a1(A x        ){A r=ma(0,1);*r->A=mh(x);                            R r;} //singleton
S A a2(A x,A y    ){A r=ma(0,2);*r->A=mh(x);r->A[1]=mh(y);              R r;} //pair
S A a3(A x,A y,A z){A r=ma(0,3);*r->A=mh(x);r->A[1]=mh(y);r->A[2]=mh(z);R r;} //triplet
S A addC(A x,C y){A z=ma(xt,xn+1);mc(zC,xC,mz(x));zC[xn]=y;mf(x);R z;}
S A addL(A x,L y){A z=ma(xt,xn+1);mc(zL,xL,mz(x));zL[xn]=y;mf(x);R z;}
S A addA(A x,A y){A z=ma(xt,xn+1);mc(zA,xA,mz(x));zA[xn]=y;F(zn)mh(zA[i]);mf(x);R z;}
S A sqz(A x){J(xt)R x;L t=xA[0]->t;J(t>=0)R x;F(xn)J(t!=xA[i]->t)R x;A z=ma(-t,xn);
             Y(t){Q-6:Q-11:F(xn)zL[i]=*xA[i]->L;R z;
                  Q-10:    F(xn)zC[i]=*xA[i]->C;R z;
                  D:en();R 0;}}
S A amend(A x,L i,A v){Y(xt){Q 0:J(x->r<2){mf(xA[i]);xA[i]=mh(v);R x;}
                                 E{A z=ma(xt,xn);mc(zA,xA,mz(x));zA[i]=v;F(zn)mh(zA[i]);mf(x);R z;}
                             D:en();R 0;}}
S A dget(A d,L k){A x=d->A[0],y=d->A[1];
                  F(xn)J(xL[i]==k)Y(yt){Q 0:R mh(yA[i]);
                                        Q 6:Q 11:{A z=ma(-yt,1);*zL=yL[i];R z;}
                                        Q 10:    {A z=ma(-yt,1);*zC=yC[i];R z;}
                                        D:en();R 0;}
                  R 0;}
S A dput(A d,L k,A v){A x=d->A[0],y=d->A[1];L i=0,n=xn;W(i<n&&xL[i]!=k)i++;
                      A z=i<n?a2(x,amend(mh(y),i,v)):a2(addL(x,k),addA(mh(y),v));zt=99;mf(x);R z;}
S A ext(A x,L n){J(xt>=0)R x;A z=ma(abs(xt),n);L k=mz(x),l=mz(z);
                 mc(zC,xC,k);W(2*k<l){mc(zC+k,zC,k);k*=2;}mc(zC+k,zC,l-k);R z;}

//parser
S C*s0,*s; //k source
S L ltr(C x){x|=32;R'a'<=x&&x<='z';}            //letter?
S L dgt(C x){R'0'<=x&&x<='9';}                  //digit?
S L ldg(C x){R ltr(x)||dgt(x);}                 //alphanumeric?
S L hdg(C x){x|=32;R dgt(x)||('a'<=x&&x<='f');} //hex digit?
S L num(C*x){R dgt(*x)||(*x=='.'&&dgt(x[1]))||((*x=='-'&&(dgt(x[1])))||(x[1]=='.'&&dgt(x[2])));} //is number start?
S C esc(C x){Y(x){Q'\0':R'0';Q'\n':R'n';Q'\r':R'r';Q'\t':R't';Q'"':R'"';D:R 0;}}
S C une(C x){Y(x){Q'0':R'\0';Q'n':R'\n';Q'r':R'\r';Q't':R'\t';D:R x;}}
S A mon(A x){J(xt!=107)R x;C c=*xC;mf(x);R mh(cv[c][1]);} //monadic version of verb, eg + -> +:
S V ep(L x){J(!x)R;C*p=s,*q=s;W(p>s0&&p[-1]!='\n')p--;W(*q&&*q!='\n')q++;write(2,p,q-p);C b[256];*b='\n'; //parse error
            L k=min(s-p,Z(b));F(k)b[i+1]='_';b[k+1]='^';b[k+2]='\n';write(2,b,k+3);er("parse");}
S A prs(C l){ //parse
  A z=a1(cc[l]);W(1){ //z:result of parsing expression in () [] {} or at top level
    A y=0,t[64];L n=0,g=0;W(1){ //t:sequence of nouns/verbs, n:count, g:bitset of grammatical categories (0=noun,1=verb)
      A x=0;L gx=0;ep(0x80&*s);W(*s==' '){s++;J(*s=='/')W(*s&&*s!='\n')s++;}C c=*s;
      J(!c)B; //eof?
      E J(ltr(c)){x=ma(-11,1);L v=*s++,h=8;W(ldg(*s)){v|=(L)*s++<<h;h+=8;}*xL=v;}
      E J(c=='`'){x=mh(cy0);W(*s=='`'){s++;L v=0,h=0;J(ltr(*s))W(ldg(*s)){v|=(L)*s++<<h;h+=8;}x=addL(x,v);}}
      E J(c=='"'){x=mh(cc0);s++;W(*s&&*s!='"')J(*s=='\\'){s++;ep(!*s);x=addC(x,une(*s++));}E{x=addC(x,*s++);}
                  ep(!*s);s++;J(xn==1)xt=-xt;}
      E J(c=='0'&&s[1]=='x'){x=mh(cc0);s+=2;W(hdg(*s)&&hdg(s[1])){x=addC(x,unh(*s)<<4|unh(s[1]));s+=2;}J(xn==1)xt=-xt;}
      E J(num(s)&&(*s!='-'||s==s0||(!ldg(s[-1])&&s[-1]!=')'))){
                  x=mh(cl0);W(1){I m=*s=='-';s+=m;L v=0;W(dgt(*s))v=10*v+(*s++-'0');
                                 x=addL(x,m?-v:v);J(*s!=' '||!num(s+1))B;s++;}
                  J(xn==1)xt=-xt;}
      E J(c<127&&cv[c][0]){I u=s[1]==':';x=mh(cv[c][u]);s+=1+u;gx=1;}
      E J(c=='('&&s[1]==')'){s+=2;x=mh(ca0);}
      E J(c=='('||c=='['||c=='{'){s++;x=prs(s[-1]);ep(*s!=')'&&*s!=']'&&*s!='}');s++;
                                  J(xn==2&&c=='('){A y=x;x=mh(xA[1]);mf(y);}}
      E J(c!=')'&&c!=']'&&c!='}'&&c!=';'&&c!='\n'&&c){ep(1);R 0;}
      J(!x)B;
      C m=1;W(m)Y(*s){Q'\\':Q'/':Q'\'':{C c=*s++,u=*s==':';s+=u;x=a2(cv[c][u],x);gx=1;B;}
                      Q'[':{s++;A u=x;x=prs('[');*xA=u;ep(*s!=']');s++;gx=0;B;}
                      D:m=0;B;}
      t[n++]=x;g=g<<1|gx;
    }
    J(!n){y=cv[':'][1];}
    E J(!(g&1)){y=t[--n];g>>=1;W(n){J(n>1&&(g&3)==1){y=a3(t[n-1],t[n-2],y);n-=2;g>>=2;}E{y=a2(mon(t[--n]),y);g>>=1;}}}
    E J(g&1){W(n){A x;J(n>1&&(g&3)==1){x=a2(t[n-1],t[n-2]);n-=2;g>>=2;xt=103;}E{x=t[--n];}
                      J(y){y=a2(mon(x),y);yt=104;yn=yA[1]->t;}E{y=x;}}}
    z=addA(z,y);J(*s!=';'&&*s!='\n')B;s++;
  }
  J(l!='('&&zn==1){mf(z);z=mh(ca0);}
  E J(l==';'){A y=zA[zn-1];J(!yt&&*yA==cv[':'][0]){z=addA(z,cv[':'][1]);}J(zn==2){A u=z;z=mh(zA[1]);mf(u);}}
  R z;
}

//evaluation
S A pen1(C f,A x){ //penetrate
  J(!xt){A z=ma(0,xn),*p=xA,*r=zA,*r1=r+xn;W(r<r1)*r++=pen1(f,*p++);R z;}
  J(abs(xt)==6){
    A z=ma(xt,xn);L*p=xL,*r=zL,*r1=r+xn;
    #define H(h,e) Q h:W(r<r1){L u=*p;*r++=e;p++;}R z;
      Y(f){H('-',-u)D:en();R 0;}
    #undef H
  }
  en();R 0;
}
S A pen2(C f,A x,A y){
  L n=max(xn,yn);J(xt>=0&&yt>=0&&xn!=yn){el();R 0;}
  J(!xt||!yt){
    A z=ma(0,n);
    J  (xt<0)FA(y,{zA[i]=pen2(f,x    ,u);})
    E J(xt>0)FA(y,{zA[i]=pen2(f,xA[i],u);})
    E J(yt<0)FA(x,{zA[i]=pen2(f,u,y    );})
    E        FA(x,{zA[i]=pen2(f,u,yA[i]);})
    R sqz(z);
  }E J(abs(xt)==6&&abs(yt)==6){
    A z=ma(max(xt,yt),n);J(xt>0&&yt>0&&xn!=yn){el();R 0;}L*p=xL,*q=yL,*r=zL,*r1=r+n,dp=xt>0,dq=yt>0;
    #define H(h,e) Q h:W(r<r1){L u=*p,v=*q;*r++=e;p+=dp;q+=dq;}R z;
    Y(f){H('+',u+v)H('-',u-v)H('*',u*v)H('%',u/v)H('&',min(u,v))H('|',max(u,v))H('=',u==v)H('<',u<v)H('>',u>v)D:en();R 0;}
    #undef H
  }
  en();R 0;
}
S A apply(A a){
  A f=*a->A;L ft=f->t;
  Y(ft){Q 105:{A x=a->A[1];
               Y(f->A[0]->C[0]){
                 Q'/':{A z=0,ff=f->A[1];FA(x,{J(z){A h=a3(ff,z,u);A r=apply(h);mf(z);z=r;mf(h);}E{z=mh(u);}});R z;}}
               B;}
        Q 106:{J(a->n!=2){er("rank");R 0;}A x=a->A[1];
               Y(*f->C){Q'-':R pen1(*f->C,x);
                        Q'#':{A z=ma(-6,1);*zL=xn;R z;}
                        Q'@':{A z=ma(-6,1);*zL=xt;R z;}
                        Q',':J(xt<0){A z=ma(-xt,1);mc(zC,xC,mz(z));R z;}E{R a1(x);}
                        Q'*':Y(abs(xt)){Q 0:R mh(*xA);
                                        Q 6:Q 10:Q 11:{A z=ma(-abs(xt),1);xt==10?(*zC=*xC):(*zL=*xL);R z;}
                                        D:en();}
                        Q'!':{J(xt!=-6){en();R 0;}
                              L n=*xL;A z=ma(6,abs(n));J(n<0){F(-n)zL[i]=i+n;}E{F(n)zL[i]=i;}R z;}}
               B;}
        Q 107:{J(a->n!=3){er("rank");R 0;}A x=a->A[1],y=a->A[2];
               Y(*f->C){Q'+':Q'-':Q'*':Q'%':Q'&':Q'|':Q'<':Q'=':Q'>':R pen2(*f->C,x,y);
                        Q'!':{J(abs(xt)!=11){et();R 0;}J(xt>=0&&yt>=0&&xn!=yn){el();R 0;}
                              J(xt<0){x=ext(x,1);y=a1(y);}E{mh(x);y=ext(y,xn);}
                              A z=a2(x,y);mf(x);mf(y);zt=99;R z;}}
               B;}
        Q 108:{A z=ma(105,2);*zA=f;zA[1]=a->A[1];R z;B;}}
  en();R 0;
}
S A eval(A x,A*l,A*g){
  J(xt==-11){A z=dget(*l,*xL);J(!z)z=dget(*g,*xL);J(!z){er("value");R 0;};R mh(z);}
  J(xt==11&&xn==1){A z=ma(-11,1);*zL=*xL;R z;}
  J(xt||!xn)R mh(x);
  J(*xA==cc[';']){F(xn-2)mf(eval(xA[i+1],l,g));R eval(xA[xn-1],l,g);}
  J(*xA==cc['(']){A z=ma(0,xn-1);F(xn-1)zA[i]=eval(xA[i+1],l,g);R sqz(z);}
  J(*xA==cv[':'][0]&&xn==3){A y=mh(xA[1]);J(yt==-11){A z=eval(xA[2],l,g);*l=dput(*l,*yL,z);R z;}}//assignment
  A y=ma(0,xn);F(xn)yA[i]=eval(xA[i],l,g);R apply(y);
}

//output
S C ob[0x400];S L on=0;                    //buffer
S V ofl(){J(on){write(1,ob,on);on=0;}}     //flush
S V oC(C x){J(on==Z(ob))ofl();ob[on++]=x;} //output char
S V oS(C*x,L n){J(n>Z(ob)-on){ofl();write(1,x,n);}E{mc(ob+on,x,n);on+=n;}} //output string
S V oL(L x){C b[32],*u=b+31;I m=x<0;J(m)x=-x;do{*u--='0'+x%10;x/=10;}W(x);J(m)*u--='-';oS(u+1,b+31-u);} //output number
S V oA(A x){ //output array
  J(xn==1&&0<=xt&&xt<100)oC(',');
  Y(abs(xt)){
    Q 0:J(xn==1){oA(*xA);}E{oC('(');F(xn){J(i)oC(';');oA(xA[i]);}oC(')');}B;
    Q 6:J(xn){F(xn){J(i)oC(' ');oL(xL[i]);}}E{oS("!0",2);}B;
    Q 10:{I h=0;F(xn)J((xC[i]<32||126<xC[i])&&!esc(xC[i])){h=1;B;}
          J(h){oS("0x",2);F(xn){oC(hex(xC[i]>>4&15));oC(hex(xC[i]&15));}}
          E{oC('"');F(xn){C c=esc(xC[i]);J(c){oC('\\');oC(c);}E{oC(xC[i]);}}oC('"');}
          B;}
    Q 11:F(xn){oC('`');L v=xL[i];W(v){oC(v&0xff);v>>=8;}}B;
    Q 99:J(xA[0]->n>1){oA(*xA);oC('!');oA(xA[1]);}E{oC('(');oA(*xA);oS(")!",2);oA(xA[1]);}B;
    Q 103:oA(*xA);oC('[');oA(xA[1]);oC(']');B;
    Q 104:oA(*xA);oA(xA[1]);B;
    Q 105:oA(xA[1]);oA(*xA);B;
    Q 106:Q 107:Q 108:oC(*xC);J(x==cv[*xC][1])oC(':');B;
    D:oS("???",3);B;
  }
}
S V out(A x){J(x!=cv[':'][1]){oA(x);oC('\n');ofl();}}

//main
S V exec(C*x,A*l,A*g){
  J(*x=='\\')Y(x[1]){
    Q'a':s=s0=x+2;A t=prs(';');out(t);mf(t);R;
    Q 0:exit(0);
    D:er("syscmd");R;
  }
  s=s0=x;A t=prs(';'),r=eval(t,l,g);mf(t);out(r);mf(r);
}
asm(".globl _start\n_start:pop %rdi\nmov %rsp,%rsi\njmp go");
V go(I ac,C**av){
  mi();ci();A l=mh(cd0),g=mh(cd0);
  J(av[1]){ //file.k
    I f=open(av[1],0,0);J(f<0)er("open");L h[18];L r=fstat(f,h);J(r)er("fstat");L n=h[6];J(!n)er("empty");
    C*s=(C*)mmap(0,n,3,0x4002,f,0);J(s==(V*)-1)er("mmap");r=close(f);J(r)er("close");J(s[n-1]!='\n')er("eof");
    s[n-1]=0;exec(s,&l,&g);r=munmap(s,n);J(r)er("munmap");
  }
  C b[256];I nb=0,k; //repl:
  W((k=read(0,b,256-nb))>0){C*p=b,*q=b+nb,*r=q+k;W(q<r){J(*q=='\n'){*q=0;exec(p,&l,&g);p=q+1;}q++;}mc(b,p,nb=q-p);}
  exit(0);
}
