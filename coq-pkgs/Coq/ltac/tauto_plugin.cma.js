function(ba){"use strict";var
C="f",x="X2",Q="Logic",g="X1",l="tauto_flags",w="id",R="Coq",k=ba.jsoo_runtime,a=k.caml_new_string,O=k.caml_register_global,a$=k.caml_wrap_exception;function
d(a,b){return a.length==1?a(b):k.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):k.caml_call_gen(a,[b,c])}function
p(a,b,c,d){return a.length==3?a(b,c,d):k.caml_call_gen(a,[b,c,d])}function
P(a,b,c,d,e){return a.length==4?a(b,c,d,e):k.caml_call_gen(a,[b,c,d,e])}function
u(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):k.caml_call_gen(a,[b,c,d,e,f])}var
e=k.caml_get_global_data(),y=a("tauto_plugin"),f=e.Names,N=e.Ltac_plugin__Tacenv,r=e.Util,s=e.CAst,D=e.Mltop,z=e.Ltac_plugin__Tacinterp,j=e.Tacticals,c=e.Proofview,G=e.Pp,o=e.EConstr,m=e.Tactics,n=e.Hipattern,E=e.Geninterp,aO=e.Nametab,aN=e.Not_found,aD=e.Locusops,aj=e.Global,X=e.Assert_failure,T=e.Stdlib,aa=e.Goptions,aK=e.Libnames;O(46,[0,0],"Tauto_plugin");d(D[9],y);var
aR=a(C),aS=a("x"),aE=[22,0],aw=[0,a(Q),[0,a("Init"),[0,a(R),0]]],at=a(g),au=a(x),av=a(w),ar=a(g),an=a(g),ao=a(x),ap=a(w),al=a(g),ai=a(g),ag=a(g),V=a(l),W=[0,a("coq-external/coq-v8.10+32bit/plugins/ltac/tauto.ml"),61,12],S=a("tauto: anomaly"),U=a(l),Z=[0,a("Intuition"),[0,a("Negation"),[0,a("Unfolding"),0]]],_=a("unfolding of not in intuition"),ad=[0,0,0],aB=a("not"),aF=[0,a("Classical_Prop"),[0,a(Q),[0,a(R),0]]],aH=a("NNPP"),aP=[0,1,0,1,1,0],aQ=[0,0,0,0,0,0],aT=[0,a(l),[0,a(g),0]],aU=a("is_empty"),aV=[0,a(l),[0,a(g),0]],aW=a("is_unit_or_eq"),aX=[0,a(l),[0,a(g),0]],aY=a("is_disj"),aZ=[0,a(l),[0,a(g),0]],a0=a("is_conj"),a1=[0,a(l),[0,a(g),[0,a(x),[0,a(w),0]]]],a2=a("flatten_contravariant_disj"),a3=[0,a(l),[0,a(g),[0,a(x),[0,a(w),0]]]],a4=a("flatten_contravariant_conj"),a5=a("apply_nnpp"),a6=a("reduction_not_iff"),a7=[0,a(C),0],a8=a("with_uniform_flags"),a9=[0,a(C),0],a_=a("with_power_flags");function
h(e,c){var
g=c[1],h=d(f[1][6],e),i=b(f[1][11][22],h,g),a=d(z[2][2],i);return a?a[1]:d(T[3],S)}var
F=d(E[1][1],U);function
t(c){var
e=c[1],g=d(f[1][6],V),a=b(f[1][11][22],g,e),h=a[2];if(b(E[1][2],a[1],F))return h;throw[0,X,W]}var
A=[0,1];function
Y(a){A[1]=a;return 0}var
$=[0,0,_,Z,function(a){return A[1]},Y];b(aa[4],0,$);var
v=d(c[16],0),ab=d(G[7],0),ac=b(j[65][4],0,ab),q=d(c[40],ac),H=m[16];function
I(a,b){var
e=a?[0,[0,a[1]]]:0,f=P(m[143],1,e,0,b);return d(c[40],f)}function
B(a){return d(m[87],a)}function
J(a){return d(m[76],[0,a,0])}var
K=m[42],ae=b(m[117],0,ad);function
af(e,a){function
d(d){function
e(b){var
c=h(ag,a);return p(n[12],d,b,c)?v:q}return b(c[74][1],c[55],e)}return b(c[74][1],c[56],d)}function
ah(e,a){function
d(d){function
e(b){var
c=t(a)[5]?n[15]:n[14];return p(c,d,b,h(ai,a))?v:q}return b(c[74][1],c[55],e)}return b(c[74][1],c[56],d)}function
L(a,c){var
e=b(o[58],a,c);if(e){var
g=b(o[91],a,c)[1],f=b(o[3],a,g);return 11===f[0]?2===d(aj[31],f[1][1])[1][6]?1:0:0}return e}function
ak(e,d){function
a(e){function
a(b){var
a=t(d),c=h(al,d),f=a[2]?L(b,c)?0:1:0;if(!f)if(u(n[6],[0,a[4]],[0,a[1]],e,b,c))return v;return q}return b(c[74][1],c[55],a)}return b(c[74][1],c[56],a)}function
am(f,a){function
e(k){function
e(c){var
e=t(a),l=h(an,a),m=h(ao,a),f=h(ap,a),g=u(n[5],[0,e[3]],[0,e[1]],k,c,l);if(g){var
i=g[1][2],s=function(b,a){return p(o[35],b,0,a)},v=p(r[18][16],s,i,m),w=function(a){return H},x=b(j[65][23],w,i),y=[0,x,[0,B(f),[0,ae,[0,K,0]]]],z=d(j[65][22],y),A=[0,J(b(o[75],c,f)),0],C=[0,I([0,z],v),A];return d(j[65][22],C)}return q}return b(c[74][1],c[55],e)}return b(c[74][1],c[56],e)}function
aq(e,d){function
a(e){function
a(b){var
a=t(d),c=h(ar,d),f=a[2]?L(b,c)?0:1:0;if(!f)if(u(n[4],[0,a[4]],[0,a[1]],e,b,c))return v;return q}return b(c[74][1],c[55],a)}return b(c[74][1],c[56],a)}function
as(f,a){function
e(i){function
e(c){var
e=t(a),k=h(at,a),l=h(au,a),f=h(av,a),g=u(n[3],[0,e[3]],[0,e[1]],i,c,k);if(g){var
s=g[1][2],v=function(b,a){var
c=p(o[35],a,0,l),e=[0,P(m[109],0,0,b+1|0,0),[0,K,0]],g=[0,H,[0,B(f),e]];return I([0,d(j[65][22],g)],c)},w=b(r[18][13],v,s),x=J(b(o[75],c,f)),y=d(j[65][22],w);return b(j[65][3],y,x)}return q}return b(c[74][1],c[55],e)}return b(c[74][1],c[56],e)}var
ax=b(r[18][68],f[1][6],aw),ay=d(f[5][4],ax),az=d(f[6][4],aB),aA=[0,0,[0,[0,[1,b(f[17][3],[0,ay],az)],0]]];function
aC(d,a){var
c=0===A[1]?aE:[0,b(s[1],0,[10,[5,[0,aA,0]],aD[5]])];return b(z[23],a,c)}var
aG=b(r[18][68],f[1][6],aF),aI=d(f[1][6],aH),aJ=d(f[5][4],aG),aL=b(aK[15],aJ,aI);function
aM(g,f){function
a(h){try{var
a=d(aO[29],aL),f=d(j[65][61],a),g=b(c[74][1],f,B);return g}catch(a){a=a$(a);if(a===aN){var
e=d(G[7],0);return b(j[65][4],0,e)}throw a}}var
e=d(c[16],0);return b(c[17],e,a)}function
M(e,o,a){var
g=d(f[1][6],aR),h=b(s[1],0,g),i=d(f[1][6],aS),c=b(s[1],0,i),j=a[3],k=a[2],l=[0,p(f[1][11][4],c[1],[0,F,e],a[1]),k,j],m=[3,b(s[1],0,[0,[1,h],[0,[2,[1,c]],0]])],n=[29,b(s[1],0,m)];return b(z[23],l,n)}function
i(g,a,e){function
h(a){return d(f[1][6],a)}var
i=b(r[18][68],h,e);function
j(a){return[0,a]}var
c=[0,y,a],k=b(r[18][68],j,i);p(N[16],0,c,[0,g]);var
l=[28,[0,k,[31,b(s[1],0,[0,[0,c,0],0])]]];function
m(c){var
b=d(f[1][6],a);return u(N[10],1,1,0,b,l)}return b(D[13],m,y)}i(af,aU,aT);i(ah,aW,aV);i(aq,aY,aX);i(ak,a0,aZ);i(as,a2,a1);i(am,a4,a3);i(aM,a5,0);i(aC,a6,0);i(function(a,b){return M(aP,a,b)},a8,a7);i(function(a,b){return M(aQ,a,b)},a_,a9);O(68,[0],"Tauto_plugin__Tauto");return}
