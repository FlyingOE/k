typedef void V;typedef char C;typedef int I;typedef long long L;typedef double D;
typedef struct SA{L c:8,t:8,r:48,n,L[0];struct SA*A[0];C C[0];}*A; //c:log(capacity),r:refcnt,t:type,n:count,L A C:data
#define xn (x->n)
#define yn (y->n)
#define zn (z->n)
#define xt (x->t)
#define yt (y->t)
#define zt (z->t)
#define xL (x->L)
#define yL (y->L)
#define zL (z->L)
#define xD (x->D)
#define yD (y->D)
#define zD (z->D)
#define xA (x->A)
#define yA (y->A)
#define zA (z->A)
#define xC (x->C)
#define yC (y->C)
#define zC (z->C)
#define B break
#define E else
#define J if
#define Q case
#define R return
#define S static
#define U default
#define W while
#define Y switch
#define Z sizeof
#define PG 0x1000 //page size

//syscalls
#include<syscall.h>
#define STR(x) #x
#define XSTR(x) STR(x)
#define  H(f,...) L f(__VA_ARGS__);asm(#f":               mov $"XSTR(__NR_##f)",%rax\nsyscall\nret");//<4 args
#define H4(f,...) L f(__VA_ARGS__);asm(#f":mov %rcx,%r10\nmov $"XSTR(__NR_##f)",%rax\nsyscall\nret");//4+ args
H(read,I,V*,I)H(write,I,C*,I)H(open,C*,I,I)H(close,I)H(fstat,I,V*)H4(mmap,V*,L,I,I,I,I)H(munmap,V*,L)
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
#define F(n) for(L _n=(n),i=0;i<_n;i++)
#define FC(x) for(C c,*_p=(x);(c=*_p);_p++)
#define FA(x,b) {Y((x)->t){Q 0:     F((x)->n){A a=(x)->A[i];b;}B;\
                           Q 6:Q 11:F((x)->n){A a=ma(-(x)->t,1);*(a)->L=(x)->L[i];b;mf(a);}B;\
                           Q 10:    F((x)->n){A a=cc[(x)->C[i]];b;}B;\
                           U:en();}}
S V ps(C*x){write(2,x,len(x));} //for debugging
S V ph(L x){C s[17];s[16]=0;F(16){s[15-i]=hex(x&15);x>>=4;}write(2,s,17);}
S V pd(L x){C b[32],*u=b+31;I m=x<0;J(m)x=-x;do{*u--='0'+x%10;x/=10;}W(x);J(m)*u--='-';write(2,u+1,b+31-u);}
#define pv(x) pv1(#x,(L)(x))
S L pv1(C*s,L x){write(2,s,len(s));write(2,"           ",max(1,10-len(s)));ph((L)x);write(2,"\n",1);R x;}
S V pA(A x){ps("t");pd(xt);ps("n");pd(xn);ps("r");pd(x->r);ps("c");pd(x->c);}

//error handling
#define e(m){write(1,"'"m"\n",2+Z(m));exit(1);}
#define en() e("nyi")
#define el() e("length")
#define et() e("type")
#define ed() e("domain")
#define er() e("rank")
#define dbg(x) x
#define ea(x) dbg(J(!(x)){e(XSTR(__LINE__)": "XSTR(x));})

//memory manager - buddy system
#define mN 45 //heap size is 2^mN
#define mF 123 //type of a free chunk
S V*mp,*mb[mN+1]; //mp:heap start, mb[i]:doubly linked list of chunks of size 2^i (xA[0],xA[1] reused for prev,next)
S V mc(V*x,V*y,L z){C*p=x,*q=y;F(z)*p++=*q++;} //memcpy
S V ms(V*x,C y,L z){C*p=x;F(z)*p++=y;} //memset
S V mi(){A x=mp=mb[mN]=(V*)mmap(0,1L<<mN,3,0x4022,-1,0);x->c=mN;xt=mF;x->r=0;xA[0]=xA[1]=x;ea((L)mp>0);} //init
S L mz0(C t,L n){ea(n>=0);t=abs(t);ea(t>=0||n==1);R t==10?n*Z(C):t==6||t==11?n*Z(L):max(1,n)*Z(A);} //array size
S L mz(A x){R mz0(xt,xn);}
S A ma(C t,L n){ //allocate
  L k=Z(struct SA)+mz0(t,n),i=5;W(1L<<i<k)i++;L j=i;W(!mb[j])j++;
  A x=mb[j];J(x==*xA){mb[j]=0;}E{A y=xA[0],z=xA[1];yA[1]=z;mb[j]=zA[0]=y;}
  W(j>i){j--;A y=(V*)x+(1L<<j);y->c=j;yt=mF;y->r=0;yA[0]=yA[1]=y;ea(!mb[j]);mb[j]=y;}
  x->c=i;xt=t;x->r=1;xn=n;R x;
}
S V mf(A x){ //free
  ea(x->r>0);J(--x->r)R;J(!xt||(99<=xt&&xt<=106))F(max(1,xn))mf(xA[i]);dbg(ms(xC,0xab,mz(x)));L dx=(V*)x-mp;
  J(0<=dx&&dx<1L<<mN){
    L i=x->c;W(1){A y=mp+(dx^1L<<i);J(yt!=mF||y->c!=i)B;
                  J(y==*yA){mb[i]=0;}E{A u=yA[0],v=yA[1];u->A[1]=v;mb[i]=v->A[0]=u;}
                  x=(A)((L)x&~(1L<<i));i++;}
    A y=mb[i];mb[i]=x;x->c=i;J(y){A z=*yA;xA[0]=z;zA[1]=x;xA[1]=y;yA[0]=x;}E{xA[0]=xA[1]=x;}
    dbg(xt=mF;xn=-1;)
  }E{
    ea(xt==10);L n=mz(x);V*p=(V*)((L)x-(L)x%PG);L r=munmap(p+PG,n);ea(!r);r=munmap(p,PG+n);ea(!r);
  }
}
S A mh(A x){x->r++;R x;} //hold (inc refcount)
S A mr(A x,L n){ //resize
  L t=xt;ea(t>=0);ea(t<98);ea(n>=xn);
  J(x->r==1&&Z(struct SA)+mz0(t,n)<=1L<<x->c){J(!t&&n<xn){for(L i=n;i<xn;i++){mf(xA[i]);dbg(xA[i]=0);}xn=n;R x;}}
  A z=ma(t,n);mc(zC,xC,mz(x));J(!t)F(xn)mh(xA[i]);mf(x);R z;
}
S V out(A);
S V pm0(A x,L c){J(x->c<c){F(2)pm0((V*)x+i*(1L<<(c-1)),c-1);R;}J(xt!=mF){pA(x);ps(" ");out(x);}}
S V pm(){pm0(mp,mN);}

//constants
S A cl0,cc0,cy0,cae,cle,cce,cye,cde,cc[256],cv[128][2],coxyz[3];
S V ci(){ //init
  A x=cl0=ma( -6,1);*xL=0;                     //0
    x=cc0=ma(-10,1);*xC=' ';                   //' '
    x=cy0=ma(-11,1);*xL=0;                     //`
    x=cle=ma(  6,0);                           //!0
    x=cce=ma( 10,0);                           //""
    x=cye=ma( 11,0);                           //0#`
    x=cae=ma(  0,0);*xA=mh(cce);               //()
    x=cde=ma( 99,2);*xA=mh(cye);xA[1]=mh(cae); //(0#`)!()
  F(256){x=cc[i]=ma(-10,1);*xC=i;} //chars
  F(2)FC("!#$%&*+,<=>?@^_|~:.-0123456789"){x=cv[c][i]=ma(107-i,1);*xC=c;} //verbs
  F(2)FC("'\\/"                          ){x=cv[c][i]=ma(108  ,1);*xC=c;} //adverbs
  F(3){x=coxyz[i]=ma(11,i+2);*xL='o';for(L j=0;j<=i;j++)xL[j+1]='x'+j;}
}

//basic array operations
S A a1(A x        ){A r=ma(0,1);*r->A=x;                    R r;} //singleton
S A a2(A x,A y    ){A r=ma(0,2);*r->A=x;r->A[1]=y;          R r;} //pair
S A a3(A x,A y,A z){A r=ma(0,3);*r->A=x;r->A[1]=y;r->A[2]=z;R r;} //triplet
S A addC(A x,C y){L n=xn;x=mr(x,n+1);xC[n]=y;R x;}
S A addL(A x,L y){L n=xn;x=mr(x,n+1);xL[n]=y;R x;}
S A addA(A x,A y){L n=xn;x=mr(x,n+1);xA[n]=y;R x;}
S A sqz(A x){J(xt)R x;L t=xA[0]->t;J(t>=0)R x;F(xn)J(t!=xA[i]->t)R x;
             A z=ma(-t,xn);Y(t){Q-6:Q-11:F(xn)zL[i]=*xA[i]->L;mf(x);R z;
                                Q-10:    F(xn)zC[i]=*xA[i]->C;mf(x);R z;
                                U:en();R 0;}}
S A amend(A x,L j,A v){
  ea(!xt);J(x->r<2){mf(xA[j]);xA[j]=v;R x;}E{A z=ma(xt,xn);mc(zA,xA,mz(x));zA[j]=v;F(zn)J(i!=j)mh(zA[i]);mf(x);R z;}
}
S A dget(A d,L k){
  ea(d->t==99);A x=d->A[0],y=d->A[1];
  F(xn)J(xL[i]==k)Y(yt){Q 0:R mh(yA[i]);
                        Q 6:Q 11:{A z=ma(-yt,1);*zL=yL[i];R z;}
                        Q 10:    {A z=ma(-yt,1);*zC=yC[i];R z;}
                        U:en();}
  R 0;
}
S A dput(A d,L k,A v){
  ea(d->t==99);A x=mh(d->A[0]),y=mh(d->A[1]);mf(d);L i=0,n=xn;ea(yn==n);W(i<n&&xL[i]!=k)i++;
  A z=i<n?a2(x,amend(y,i,v)):a2(addL(x,k),addA(y,v));zt=99;R z;
}
S A ext(A x,L n){ //extend atom to list
  J(xt>=0)R x;A z=ma(abs(xt),n);L k=mz(x),l=mz(z);mc(zC,xC,k);W(2*k<l){mc(zC+k,zC,k);k*=2;}mc(zC+k,zC,l-k);R z;
}
S A nil(A x){L t=xt;Y(t){Q 6:Q 10:Q 11:{A z=ma(t,xn);ms(zC,t==10?' ':0,mz(z));R z;}
                         Q 0:J(xn){A z=ma(0,xn);F(xn)zA[i]=nil(xA[i]);R z;}E{R x;}
                         Q-6:R mh(cl0);Q-10:R mh(cc0);Q-11:R mh(cy0);U:en();R 0;}}

//parser
S C*s0,*s; //k source
S L ltr(C x){x|=32;R'a'<=x&&x<='z';}            //letter?
S L dgt(C x){R'0'<=x&&x<='9';}                  //digit?
S L ldg(C x){R ltr(x)||dgt(x);}                 //alphanumeric?
S L hdg(C x){x|=32;R dgt(x)||('a'<=x&&x<='f');} //hex digit?
S L num(C*x){R dgt(*x)||(*x=='.'&&dgt(x[1]))||((*x=='-'&&(dgt(x[1])))||(x[1]=='.'&&dgt(x[2])));} //is number start?
S C esc(C x){Y(x){Q'\0':R'0';Q'\n':R'n';Q'\r':R'r';Q'\t':R't';Q'"':R'"';U:R 0;}}
S C une(C x){Y(x){Q'0':R'\0';Q'n':R'\n';Q'r':R'\r';Q't':R'\t';U:R x;}}
S A mon(A x){J(xt!=107)R x;C c=*xC;mf(x);R mh(cv[c][1]);} //monadic version of verb, eg + -> +:
S V ep(L x){J(!x)R;C*p=s,*q=s;W(p>s0&&p[-1]!='\n')p--;W(*q&&*q!='\n')q++;write(1,p,q-p);C b[256];*b='\n'; //parse error
            L k=min(s-p,Z(b));F(k)b[i+1]='_';b[k+1]='^';b[k+2]='\n';write(1,b,k+3);e("parse");}
S A prs(C l,L*nargs){ //parse
  A z=a1(mh(cc[l]));W(1){ //z:result of parsing expression in () [] {} or at top level
    A y=0,t[64];L n=0,g=0;W(1){ //t:sequence of nouns/verbs, n:count, g:bitset of grammatical categories (0=noun,1=verb)
      A x=0;L gx=0;ep(0x80&*s);W(*s==' '){s++;J(*s=='/')W(*s&&*s!='\n')s++;}C c=*s;
      J(!c)B; //eof?
      E J(ltr(c)){x=ma(-11,1);L v=*s++,h=8;W(ldg(*s)){v|=(L)*s++<<h;h+=8;}*xL=v;
                  J(v=='y'||v=='z')*nargs=max(v-'w',*nargs);}
      E J(c=='`'){x=mh(cye);W(*s=='`'){s++;L v=0,h=0;J(ltr(*s))W(ldg(*s)){v|=(L)*s++<<h;h+=8;}x=addL(x,v);}}
      E J(c=='"'){x=mh(cce);s++;W(*s&&*s!='"')J(*s=='\\'){s++;ep(!*s);x=addC(x,une(*s++));}E{x=addC(x,*s++);}
                  ep(!*s);s++;J(xn==1)xt=-xt;}
      E J(c=='0'&&s[1]=='x'){x=mh(cce);s+=2;W(hdg(*s)&&hdg(s[1])){x=addC(x,unh(*s)<<4|unh(s[1]));s+=2;}J(xn==1)xt=-xt;}
      E J(dgt(c)&&s[1]==':'){I u=s[2]==':';s+=2+u;x=mh(cv[c][u]);gx=1;}
      E J(num(s)&&(*s!='-'||s==s0||(!ldg(s[-1])&&s[-1]!=')'))){
                  x=mh(cle);W(1){I m=*s=='-';s+=m;L v=0;W(dgt(*s))v=10*v+(*s++-'0');
                                 x=addL(x,m?-v:v);J(*s!=' '||!num(s+1))B;s++;}
                  J(xn==1)xt=-xt;}
      E J(c<127&&cv[c][0]){I u=s[1]==':';x=mh(cv[c][u]);s+=1+u;gx=1;}
      E J(c=='('&&s[1]==')'){s+=2;x=mh(cae);}
      E J(c=='('){s++;x=prs(s[-1],nargs);ep(*s!=')');s++;J(xn==2){A y=x;x=mh(xA[1]);mf(y);}}
      E J(c=='{'){C*s1=s;s++;L k=1;A u=prs(';',&k);ep(*s!='}');s++;
                  x=ma(102,3);xA[0]=mh(coxyz[k-1]);xA[1]=u;xA[2]=ma(10,s-s1);mc(xA[2]->C,s1,s-s1);}
      E J(c!=')'&&c!=']'&&c!='}'&&c!=';'&&c!='\n'&&c){ep(1);}
      J(!x)B;
      C m=1;W(m)Y(*s){Q'\\':Q'/':Q'\'':{C c=*s++,u=*s==':';s+=u;x=a2(mh(cv[c][u]),x);gx=1;B;}
                      Q'[':{s++;A u=x;x=prs('[',nargs);*xA=u;ep(*s!=']');s++;gx=0;B;}
                      U:m=0;B;}
      t[n++]=x;g=g<<1|gx;
    }
    J(!n){y=mh(cv[':'][1]);}
    E J(!(g&1)){y=t[--n];g>>=1;W(n){J(n>1&&(g&3)==1){y=a3(t[n-1],t[n-2],y);n-=2;g>>=2;}E{y=a2(mon(t[--n]),y);g>>=1;}}}
    E{W(n){A x;J(n>1&&(g&3)==1){x=a2(t[n-1],t[n-2]);xt=103;n-=2;g>>=2;}E{x=t[--n];}
               J(y){y=a2(mon(x),mh(y));yt=104;}E{y=x;}}}
    z=addA(z,y);J(*s!=';'&&*s!='\n')B;s++;
  }
  J(l!='('&&zn==1){mf(z);z=mh(cae);}
  E J(l==';'){A y=zA[zn-1];J(!yt&&*yA==cv[':'][0]){z=addA(z,mh(cv[':'][1]));}J(zn==2){A u=z;z=mh(zA[1]);mf(u);}}
  R z;
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
    Q 0:{J(!xn&&(xA[0]->n||xA[0]->t!=10)){oS("0#,",3);oA(*xA);}
         E J(xn==1){oA(*xA);}
         E{oC('(');F(xn){J(i)oC(';');oA(xA[i]);}oC(')');}
         B;}
    Q 6:J(xn){F(xn){J(i)oC(' ');oL(xL[i]);}}E{oS("!0",2);}B;
    Q 10:{I h=0;F(xn)J((xC[i]<32||126<xC[i])&&!esc(xC[i])){h=1;B;}
          J(h){oS("0x",2);F(xn){oC(hex(xC[i]>>4&15));oC(hex(xC[i]&15));}}
          E{oC('"');F(xn){C c=esc(xC[i]);J(c){oC('\\');oC(c);}E{oC(xC[i]);}}oC('"');}
          B;}
    Q 11:J(xn){F(xn){oC('`');L v=xL[i];W(v){oC(v&0xff);v>>=8;}}}E{oS("0#`",3);}B;
    Q 99:J(xA[0]->n>1){oA(*xA);oC('!');oA(xA[1]);}E{oC('(');oA(*xA);oS(")!",2);oA(xA[1]);}B;
    Q 102:oS(xA[2]->C,xA[2]->n);B;
    Q 103:{oA(*xA);for(L i=1,n=xn;i<n;i++){oC(i==1?'[':';');J(xA[i]!=cv[':'][1])oA(xA[i]);}oC(']');B;}
    Q 104:oA(*xA);oA(xA[1]);B;
    Q 105:oA(xA[1]);oA(*xA);B;
    Q 106:Q 107:Q 108:{C c=*xC;oC(c);J('0'<=c&&c<='9')oC(':');J(x==cv[c][1])oC(':');B;}
    U:oS("???",3);B;
  }
}
S V out(A x){J(x!=cv[':'][1]){oA(x);oC('\n');ofl();}}

//evaluation
S A pen1(C f,A x){ //penetrate
  J(!xt){A z=ma(0,xn),*p=xA,*r=zA,*r1=r+xn;W(r<r1)*r++=pen1(f,*p++);R z;}
  Y(abs(xt)){
    Q 6:{A z=ma(xt,xn);L*p=xL,*r=zL,*r1=r+xn;
         #define H(h,w) Q h:W(r<r1){L u=*p;*r++=w;p++;}R z;
           Y(f){H('-',-u)}
         #undef H
         B;}
    Q 10:{A z=ma(xt,xn);F(xn){C c=xC[i];zC[i]='A'<=c&&c<='Z'?c+('a'-'A'):c;};R z;}
  }
  en();R 0;
}
S A pen2(C f,A x,A y){
  L n=max(xn,yn);J(xt>=0&&yt>=0&&xn!=yn)el();
  J(!xt||!yt){
    A z=ma(0,n);
    J  (xt<0)FA(y,{zA[i]=pen2(f,x    ,a);})
    E J(xt>0)FA(y,{zA[i]=pen2(f,xA[i],a);})
    E J(yt<0)FA(x,{zA[i]=pen2(f,a,y    );})
    E        FA(x,{zA[i]=pen2(f,a,yA[i]);})
    R sqz(z);
  }E J(abs(xt)==6&&abs(yt)==6){
    A z=ma(max(xt,yt),n);J(xt>0&&yt>0&&xn!=yn)el();L*p=xL,*q=yL,*r=zL,*r1=r+n,dp=xt>0,dq=yt>0;
    #define H(h,w) Q h:W(r<r1){L u=*p,v=*q;*r++=w;p+=dp;q+=dq;}R z;
    Y(f){H('+',u+v)H('-',u-v)H('*',u*v)H('%',u/v)H('&',min(u,v))H('|',max(u,v))H('=',u==v)H('<',u<v)H('>',u>v)U:en();}
    #undef H
  }
  en();R 0;
}
S A eval(A,A*,A*);
S A apply(A*a,I na,A*l,A*g){
  A f=*a,x=a[1],y=na>2?a[2]:0;
  Y(f->t){
    Q 102:{
      A k=*f->A;J(k->n!=na)er();A v=ma(0,na);F(na)v->A[i]=mh(a[i]);A d=a2(mh(k),v);d->t=99;
      A z=eval(f->A[1],&d,g);mf(d);R z;
    }
    Q 103:{
      A z=ma(0,f->n+na);L j=1,ok=1;
      F(f->n){zA[i]=mh(f->A[i]==cv[':'][1]&&j<na?a[j++]:f->A[i]);ok=ok&&(zA[i]!=cv[':'][1]);}
      J(ok){L k=f->n;W(j<na){zA[k]=mh(a[j]);k++;j++;}zn=k;A r=apply(zA,k,l,g);mf(z);R r;}E{zn=f->n;zt=103;R z;}
    }
    Q 105:{
      Y(f->A[0]->C[0]){
        Q'/':{A z=0,h[]={f->A[1],0,0};FA(x,{J(z){h[1]=z;h[2]=a;A r=apply(h,3,l,g);mf(z);z=r;}E{z=mh(a);}});R z;}
      }
      B;
    }
    Q 106:{
      J(na!=2)er();
      Y(*f->C){
        Q'-':Q'_':R pen1(*f->C,x);
        Q'#':{A z=ma(-6,1);*zL=xn;R z;}
        Q'@':{A z=ma(-6,1);*zL=xt;R z;}
        Q',':J(xt<0){A z=ma(-xt,1);mc(zC,xC,mz(z));R z;}E{R a1(mh(x));}
        Q'*':Y(abs(xt)){Q 0:R mh(*xA);
                        Q 6:Q 10:Q 11:{A z=ma(-abs(xt),1);xt==10?(*zC=xn?*xC:' '):(*zL=xn?*xL:0);R z;}
                        U:en();}
        Q'!':{J(xt!=-6)en();L n=*xL;A z=ma(6,abs(n));J(n<0){F(-n)zL[i]=i+n;}E{F(n)zL[i]=i;}R z;}
        Q'1':{C s[256];J(xt<0)er();J(xt!=10)et();J(xn>=Z(s))el();mc(s,xC,xn);s[xn]=0;
              A z;L fd=open(s,0,0);J(fd<0)e("open");L h[18];L r=fstat(fd,h);J(r)e("fstat");L n=h[6];
              J(!n){z=mh(cce);}E{V*z0=(V*)mmap(0,PG+n,3,0x4022,-1,0);J((L)z0<0)e("mmap");z=z0+(PG-Z(*z));
                                 V*u=(V*)mmap(zC,n,3,0x4012,fd,0);J((L)u<0)e("mmapfile");
                                 zn=n;zt=10;z->r=1;}
              r=close(fd);J(r)e("close");R z;}
      }
      B;
    }
    Q 107:{
      J(na!=3)er();
      Y(*f->C){
        Q'+':Q'-':Q'*':Q'%':Q'&':Q'|':Q'<':Q'=':Q'>':R pen2(*f->C,x,y);
        Q'!':Y(xt){
          Q 11:Q-11:{J(xt>=0&&yt>=0&&xn!=yn)el();
                     mh(x);mh(y);J(xt<0){x=ext(x,1);y=a1(y);}E{y=ext(y,xn);} A z=a2(x,y);zt=99;R z;}
          Q-6:Y(*xL){
            Q-16:{A z=ma(-6,1);*zL=y->r;R z;}
            U:ed();
          }
          U:et();
        }
        Q'#':Y(xt){
          Q-6:{L n=*xL,t=abs(yt);J(n<0)el();A z=ma(t,n);J(!yn)y=t==6?cl0:t==10?cc0:t==11?cy0:y;
               J(n){L k=mz(z),l=min(mz(y),k);mc(zC,yC,l);W(2*l<k){mc(zC+l,zC,l);l*=2;}J(l<k)mc(zC+l,zC,k-l);}
               J(!t){J(n){F(n)mh(zA[i]);z=sqz(z);}E{*zA=nil(*yA);}} R z;}
          U:ed();
        }
      }
      B;
    }
    Q 108:{A z=a2(mh(f),mh(x));zt=105;R z;}
    B;
  }
  en();R 0;
}
S L truthy(A x){J(!xn)R 0;J(xt>99)R x!=cv[':'][1];Y(abs(xt)){Q 6:Q 11:R!!*xL;Q 10:R!!*xC;U:en();R 0;}}
S A eval(A x,A*l,A*g){L n=xn,t=xt;
  J(t==-11){A z=dget(*l,*xL);J(!z)z=dget(*g,*xL);J(!z)e("value");R z;}
  J(t==11&&n==1){A z=ma(-11,1);*zL=*xL;R z;}
  J(t||!n)R mh(x);
  J(*xA==cc[';']){F(n-2)mf(eval(xA[i+1],l,g));R eval(xA[n-1],l,g);}
  J(*xA==cc['(']){A z=ma(0,n-1);F(n-1)zA[i]=eval(xA[i+1],l,g);R sqz(z);}
  J(*xA==cv[':'][0]&&n==3){A y=xA[1];J(yt==-11){A z=eval(xA[2],l,g);*l=dput(*l,*yL,mh(z));R z;}} //assignment
  J(*xA==cv['$'][0]&&n>3){for(L i=2;i<n;i+=2){A y=eval(xA[i-1],l,g);L r=truthy(y);mf(y);J(r)R eval(xA[i],l,g);}
                          R n&1?mh(cv[':'][1]):eval(xA[n-1],l,g);}
  A v[5];J(n>Z(v))er();F(n)v[i]=eval(xA[i],l,g);A z=apply(v,n,l,g);F(n)mf(v[i]);R z;
}
S V exec(C*x,A*l,A*g){L k=1;
  J(*x!='\\'){s=s0=x;A t=prs(';',&k),r=eval(t,l,g);mf(t);out(r);mf(r);R;}
  Y(x[1]){Q 0:exit(0);B;
          Q'a':s=s0=x+2;A t=prs(';',&k);out(t);mf(t);B;
          Q'm':pm();B;
          U:e("syscmd");B;}
}
asm(".globl _start\n_start:pop %rdi\nmov %rsp,%rsi\njmp main");
V main(I ac,C**av){
  mi();ci();A l=mh(cde);
  J(av[1]){I f=open(av[1],0,0);J(f<0)e("open");L h[18];L r=fstat(f,h);J(r)e("fstat");L n=h[6];J(!n)e("empty");
           C*s=(C*)mmap(0,n,3,0x4002,f,0);J(s==(V*)-1)e("mmap");r=close(f);J(r)e("close");J(s[n-1]!='\n')e("eof");
           s[n-1]=0;exec(s,&l,&l);r=munmap(s,n);J(r)e("munmap");}
  C b[256];I nb=0,k; //repl:
  W((k=read(0,b,256-nb))>0){C*p=b,*q=b+nb,*r=q+k;W(q<r){J(*q=='\n'){*q=0;exec(p,&l,&l);p=q+1;}q++;}mc(b,p,nb=q-p);}
  exit(0);
}
