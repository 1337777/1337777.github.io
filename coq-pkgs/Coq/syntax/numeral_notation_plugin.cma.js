function(a7){"use strict";var
F="(",D="after",o=")",C="Instead of Decimal.int, the types Decimal.uint or Z or Int63.int or Decimal.decimal could be used (you may need to require BinNums or Decimal or Int63 first).",h=a7.jsoo_runtime,c=h.caml_new_string,r=h.caml_register_global,a6=h.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):h.caml_call_gen(a,[b])}function
e(a,b,c){return a.length==2?a(b,c):h.caml_call_gen(a,[b,c])}function
K(a,b,c,d){return a.length==3?a(b,c,d):h.caml_call_gen(a,[b,c,d])}function
s(a,b,c,d,e){return a.length==4?a(b,c,d,e):h.caml_call_gen(a,[b,c,d,e])}var
d=h.caml_get_global_data(),al=c("num.int63.type"),ae=c("num.int.type"),af=c("num.uint.type"),ag=c("num.decimal.type"),ab=c("num.Z.type"),ac=c("num.pos.type"),v=c("numeral_notation_plugin"),k=d.Constrexpr_ops,G=d.Smartlocate,x=d.Nametab,b=d.Pp,q=d.Libnames,am=d.CErrors,n=d.Coqlib,t=d.Names,E=d.Global,aa=d.Util,B=d.Vernacextend,A=d.Attributes,g=d.CLexer,w=d.Pcoq,i=d.Genarg,l=d.Stdarg,bC=d.CAst,bE=d.Notation,T=d.Constrintern,S=d.Pretype_errors,R=d.Not_found,L=d.Termops,Q=d.CWarnings,aN=d.Pfedit,aQ=d.Evd,aP=d.Locality,U=d.Mltop,aJ=d.Ltac_plugin__Tacentries;r(43,[0,0,0],"Numeral_notation_plugin");var
bB=[0,0],bH=[0,0,1],bI=[0,0,0],bF=[0,0,1],bG=[0,0,0],bj=c(C),bm=c(" to Decimal.int or (option Decimal.int)."),bp=c(" should go from "),a2=c(C),a5=c(")."),a8=c(" or (option "),a$=c(" should go from Decimal.int to "),aT=c("core.option.type"),I=c("option type."),J=c(") targets an "),M=c("the parsing function ("),N=c("The 'abstract after' directive has no effect when "),O=c("numbers"),P=c("abstract-large-number-no-op"),V=c(o),W=c("(warning after "),X=c(o),Y=c("(abstract after "),ap=c(o),as=c(D),au=c("warning"),aw=c(F),az=c(o),aC=c(D),aE=c("abstract"),aG=c(F),aI=c("numnotoption"),aU=c(":"),aY=c("Notation"),aZ=c("Numeral"),a4=c("NumeralNotation");function
H(c){var
d=a(b[22],I),f=a(b[22],J),g=a(E[2],0),h=a(L[82],g),i=e(x[47],h,c),j=a(b[22],M),k=a(b[22],N),l=e(b[12],k,j),m=e(b[12],l,i),n=e(b[12],m,f);return e(b[12],n,d)}var
aO=s(Q[1],P,O,0,H);function
m(b){var
c=a(n[2],b);return K(x[48],0,t[1][10][1],c)}function
p(c){var
b=a(x[13],c);if(2===b[0])return b[1];throw R}function
f(e,d,c,b){var
f=[0,a(k[10],c),[0,b]],g=a(k[11],f);try{s(T[10],e,d,0,g);var
h=1;return h}catch(a){a=a6(a);if(a[1]===S[1])return 0;throw a}}function
u(d,c,by,o,j,i,bx,F){if(a(n[3],ae))if(a(n[3],af))if(a(n[3],ag))var
ah=m(ae),ai=m(af),aj=m(ag),aX=p(ah),ak=[0,p(ai),aX],aY=[0,ak,p(aj)],aZ=a(k[10],aj),a0=a(k[10],ai),r=[0,[0,ak,a(k[10],ah),a0,aY,aZ]],B=1;else
var
B=0;else
var
B=0;else
var
B=0;if(!B)var
r=0;if(a(n[3],ab))if(a(n[3],ac))var
ad=m(ab),aU=m(ac),aV=a(k[10],ad),aW=p(aU),s=[0,[0,[0,p(ad),aW],aV]],L=1;else
var
L=0;else
var
L=0;if(!L)var
s=0;if(a(n[3],al))var
a1=m(al),t=[0,a(k[10],a1)];else
var
t=0;var
H=a(G[4],o),bz=e(G[3],0,j),bA=e(G[3],0,i),g=a(k[10],o);function
h(c,b){var
d=[0,[0,e(bC[1],0,0),0],bB,c,b];return a(k[14],d)}function
l(d){var
b=m(aT),c=[0,a(k[10],b),[0,d,0]];return a(k[15],c)}var
aP=a(E[31],H)[2][4];function
aQ(a,b){return[3,[0,H,a+1|0]]}var
aR=e(aa[20][16],aQ,aP),aS=a(aa[20][11],aR);if(r){var
u=r[1],an=u[5],ao=u[4],ap=u[3],aq=u[2],y=u[1];if(f(d,c,j,h(aq,g)))var
ar=[0,[0,y],1],N=1;else{if(f(d,c,j,h(aq,l(g))))var
aG=[0,[0,y],0],O=1;else{if(f(d,c,j,h(ap,g)))var
aH=[0,[1,y[1]],1],P=1;else{if(f(d,c,j,h(ap,l(g))))var
J=[0,[1,y[1]],0],C=1;else
if(f(d,c,j,h(an,g)))var
J=[0,[3,ao],1],C=1;else
if(f(d,c,j,h(an,l(g))))var
J=[0,[3,ao],0],C=1;else
var
M=0,N=0,O=0,P=0,C=0;if(C)var
aH=J,P=1}if(P)var
aG=aH,O=1}if(O)var
ar=aG,N=1}if(N)var
v=ar,M=1}else
var
M=0;if(!M){if(s){var
aI=s[1],aJ=aI[2],aK=aI[1];if(f(d,c,j,h(aJ,g)))var
aL=[0,[2,aK],1],R=1;else
if(f(d,c,j,h(aJ,l(g))))var
aL=[0,[2,aK],0],R=1;else
var
Q=0,R=0;if(R)var
v=aL,Q=1}else
var
Q=0;if(!Q){if(t){var
aM=t[1];if(f(d,c,j,h(aM,g)))var
aN=bH,T=1;else
if(f(d,c,j,h(aM,l(g))))var
aN=bI,T=1;else
var
S=0,T=0;if(T)var
v=aN,S=1}else
var
S=0;if(!S)var
a3=a(b[3],a2),a4=a(b[5],0),a6=a(b[3],a5),a7=a(q[27],o),a9=a(b[3],a8),a_=a(q[27],o),ba=a(b[3],a$),bb=a(q[27],j),bc=e(b[12],bb,ba),bd=e(b[12],bc,a_),be=e(b[12],bd,a9),bf=e(b[12],be,a7),bg=e(b[12],bf,a6),bh=e(b[12],bg,a4),bi=e(b[12],bh,a3),v=K(am[6],0,0,bi)}}if(r){var
w=r[1],as=w[5],at=w[4],au=w[3],av=w[2],z=w[1];if(f(d,c,i,h(g,av)))var
aw=[0,[0,z],1],V=1;else{if(f(d,c,i,h(g,l(av))))var
ay=[0,[0,z],0],W=1;else{if(f(d,c,i,h(g,au)))var
az=[0,[1,z[1]],1],X=1;else{if(f(d,c,i,h(g,l(au))))var
I=[0,[1,z[1]],0],D=1;else
if(f(d,c,i,h(g,as)))var
I=[0,[3,at],1],D=1;else
if(f(d,c,i,h(g,l(as))))var
I=[0,[3,at],0],D=1;else
var
U=0,V=0,W=0,X=0,D=0;if(D)var
az=I,X=1}if(X)var
ay=az,W=1}if(W)var
aw=ay,V=1}if(V)var
A=aw,U=1}else
var
U=0;if(!U){if(s){var
aA=s[1],aB=aA[2],aC=aA[1];if(f(d,c,i,h(g,aB)))var
aD=[0,[2,aC],1],Z=1;else
if(f(d,c,i,h(g,l(aB))))var
aD=[0,[2,aC],0],Z=1;else
var
Y=0,Z=0;if(Z)var
A=aD,Y=1}else
var
Y=0;if(!Y){if(t){var
aE=t[1];if(f(d,c,i,h(g,aE)))var
aF=bF,$=1;else
if(f(d,c,i,h(g,l(aE))))var
aF=bG,$=1;else
var
_=0,$=0;if($)var
A=aF,_=1}else
var
_=0;if(!_)var
bk=a(b[3],bj),bl=a(b[5],0),bn=a(b[3],bm),bo=a(q[27],o),bq=a(b[3],bp),br=a(q[27],i),bs=e(b[12],br,bq),bt=e(b[12],bs,bo),bu=e(b[12],bt,bn),bv=e(b[12],bu,bl),bw=e(b[12],bv,bk),A=K(am[6],0,0,bw)}}var
ax=[0,v,bz,A,bA,o,F],bJ=typeof
F==="number"?0:1===F[0]?0===v[2]?(e(aO,0,ax[2]),1):0:0,bD=[0,by,bx,[1,ax],[0,a(x[41],[2,H]),0],aS,1];return a(bE[23],bD)}r(61,[0,u],"Numeral_notation_plugin__Numeral");a(U[9],v);function
j(q,p,o,c){if(typeof
c==="number")return a(b[7],0);else{if(0===c[0]){var
d=c[1],f=a(b[3],V),g=a(b[3],d),h=a(b[3],W),i=e(b[12],h,g);return e(b[12],i,f)}var
j=c[1],k=a(b[3],X),l=a(b[3],j),m=a(b[3],Y),n=e(b[12],m,l);return e(b[12],n,k)}}function
Z(b,a){return j}function
_(b,a){return j}var
$=[0,function(b,a){return j},_,Z],ad=0,ah=[0,function(b,a){return a}],ai=[0,function(b,a){return[0,b,a]}],aj=0,ak=0,an=[0,[0,0,function(a){return 0}],ak];function
ao(f,a,e,d,c,b){return[0,a]}var
aq=[0,a(g[10],ap)],ar=[6,w[15][11]],at=[0,a(g[10],as)],av=[0,a(g[10],au)],ax=[0,[0,[0,[0,[0,[0,[0,0,[0,a(g[10],aw)]],av],at],ar],aq],ao],an];function
ay(f,a,e,d,c,b){return[1,a]}var
aA=[0,a(g[10],az)],aB=[6,w[15][11]],aD=[0,a(g[10],aC)],aF=[0,a(g[10],aE)],aH=[0,[1,[0,[0,[0,[0,[0,[0,[0,0,[0,a(g[10],aG)]],aF],aD],aB],aA],ay],ax]],aj,ai,ah,ad,$],y=e(aJ[9],aI,aH),z=y[1],aK=y[2],aL=0,aM=0;function
aR(o,n,m,l,k,j,b){var
p=e(A[1],A[8],j),c=b[3];if(c)var
d=a(aN[4],c[1]),g=d[1],f=d[2];else
var
h=a(E[2],0),g=a(aQ[17],h),f=h;var
i=a(t[1][8],l);u(f,g,a(aP[7],p),o,n,m,i,k);return[0,b[1],b[2],c,b[4]]}var
aS=[1,[5,a(i[16],z)],0],aV=[0,aU,[1,[5,a(i[16],l[7])],aS]],aW=[1,[5,a(i[16],l[17])],aV],aX=[1,[5,a(i[16],l[17])],aW],a0=[0,[0,0,[0,aZ,[0,aY,[1,[5,a(i[16],l[17])],aX]]],aR,aM],aL],a1=0,a3=[0,function(a){return B[6]}];s(B[2],a4,a3,a1,a0);r(73,[0,v,j,z,aK],"Numeral_notation_plugin__G_numeral");return}
