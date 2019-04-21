function(ac){"use strict";var
y=141,w="Derive",x="coq-external/coq-v8.10+32bit/plugins/derive/derive.ml",c=ac.jsoo_runtime,d=c.caml_new_string,l=c.caml_register_global;function
b(a,b){return a.length==1?a(b):c.caml_call_gen(a,[b])}function
g(a,b,d){return a.length==2?a(b,d):c.caml_call_gen(a,[b,d])}function
h(a,b,d,e){return a.length==3?a(b,d,e):c.caml_call_gen(a,[b,d,e])}function
v(a,b,d,e,f){return a.length==4?a(b,d,e,f):c.caml_call_gen(a,[b,d,e,f])}function
m(a,b,d,e,f,g){return a.length==5?a(b,d,e,f,g):c.caml_call_gen(a,[b,d,e,f,g])}function
ab(a,b,d,e,f,g,h){return a.length==6?a(b,d,e,f,g,h):c.caml_call_gen(a,[b,d,e,f,g,h])}var
a=c.caml_get_global_data(),t=d("derive_plugin"),r=a.Proofview,o=a.Pp,p=a.CErrors,s=a.Assert_failure,q=a.Declare,f=a.EConstr,e=a.Evd,j=a.Proof_global,i=a.Stdarg,k=a.Genarg,Q=a.Proof,J=a.Vars,I=a.Constr,C=a.Context,D=a.Environ,F=a.Constrintern,A=a.Global,z=a.Future,U=a.Attributes,O=a.Mltop,aa=a.Vernacextend;l(14,[0,0,0],"Derive_plugin");var
G=d("Admitted isn't supported in Derive."),M=d("Cannot save a proof of Derive with an explicit name."),N=[0,d(x),66,19],H=[1,0],L=[0,d(x),83,18],K=[2,5],E=[0,0],B=[0,2,0,[0,0]],P=[0,[0,[0,1,0]],1],V=d("As"),X=d("SuchThat"),Z=d(w),$=d(w);function
n(i,k,l){var
a=b(A[2],0),d=b(e[17],a),c=v(e[132],0,0,e[126],d),n=c[1],t=b(f[14],c[2]),u=[1,a,n,t,function(d,c){return[1,a,d,c,function(j,h){var
l=b(f[y][1],c),n=b(f[y][1],h),o=[1,b(C[7],i),n,l],d=g(D[37],o,a),e=m(F[16],E,d,j,0,k),p=e[2],q=e[1];return[1,d,q,p,function(a,b){return[0,a]}]}]}];function
w(e){if(0===e[0])var
x=b(o[3],G),f=h(p[6],0,0,x);else{var
E=e[1];if(e[2])var
F=b(o[3],M),t=h(p[6],0,0,F);else{var
u=e[3][2];if(u){var
j=u[2];if(j){var
k=j[2];if(k)if(k[2])var
d=0;else
var
t=[0,1!==E?1:0,j[1],k[1]],d=1;else
var
d=0}else
var
d=0}else
var
d=0;if(!d)throw[0,s,N]}var
f=t}var
a=f[3],c=f[2],y=f[1],A=m(q[3],0,0,i,0,[0,[0,[0,c[1],c[2],c[3],c[4],c[5],0,c[7]]],H]),B=b(I[17],A);function
n(a){return g(J[18],[0,[0,i,B],0],a)}var
r=a[4];if(r){var
C=n(r[1]),D=a[1],v=function(a){var
b=a[1],c=a[2],d=b[2];return[0,[0,n(b[1]),d],c]},w=g(z[16],D,v);m(q[3],0,0,l,0,[0,[0,[0,w,a[2],a[3],[0,C],a[5],y,a[7]]],K]);return 0}throw[0,s,L]}var
x=b(j[8],w),O=ab(j[11],0,l,0,B,u,x);function
P(d,b){var
c=h(r[32],1,2,r[42]);return h(Q[28],a,c,b)}return g(j[19],P,O)[1]}l(31,[0,n],"Derive_plugin__Derive");b(O[9],t);function
u(a){return P}var
R=0,S=0;function
T(g,f,e,d,a){b(U[2],d);var
c=[0,n(g,f,e)];return[0,a[1],a[2],c,a[4]]}var
W=[0,V,[1,[5,b(k[16],i[7])],0]],Y=[0,X,[1,[5,b(k[16],i[11])],W]],_=[0,[0,0,[0,Z,[1,[5,b(k[16],i[7])],Y]],T,S],R];v(aa[2],$,[0,u],0,_);l(37,[0,t,u],"Derive_plugin__G_derive");return}
