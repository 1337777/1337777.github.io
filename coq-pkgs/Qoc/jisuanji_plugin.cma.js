function(MC){"use strict";var
ck=1032426924,b4=";",dU="Rec",cf="\xe7\xac\xa6\xe5\x8f\xb7",ag="\xe6\xa8\xa1\xe5\x9d\x97",cg="debug",dT="CoFixpoint",ec="\xe8\xbf\x9b\xe5\x8f\xa3",b$=-560228223,eb="Section",t=",",dv="=",b3="unify",ce="custom",r="(",b2="Canonical",aw="pattern",aR="!",P="|",du="Set",dt="\xe5\x8f\x82\xe6\x95\xb0",a4="LoadPath",ds="local",ea="Program",bh="State",w="with",Q="]",d$="info_trivial",Z="Print",b_="\xe5\x9c\xa8",dS="\xe5\x8c\x85\xe6\x8b\xac",cj="Remove",aO="\xe8\xae\xa9",b9="\xe7\xb1\xbb\xe5\x9e\x8b",cd="Dependencies",d_="Parameters",b1="and",dR="program",a_="Implicit",aQ=">",bc="Hint",av="Declare",dr="Require",eA="Strategy",eB="\xe5\xae\x9a\xe4\xb9\x89",dP="200",dQ='The "/" modifier can occur only once',aK="core",ez="Ltac",bg="scopes",d9="constr",cc="inline",ey="info_eauto",ex="\xe8\xa6\x81\xe6\xb1\x82",Y="[",ew="SearchAbout",bf="\xe5\x87\xba\xe5\x8f\xa3",b0="global",d8="get_compat_version",dO="ident",ev="prolog",ac="Modular",dN="@{",aN="Coercion",d7="simpl",d6="Test",dM="Hypotheses",b8="-",d5="Transparent",dq="Variable",dL="<+",X="Module",W="level",au="Add",dK="<:",dp="\xe5\x85\xb6\xe4\xb8\xad",a9="implicits",et="scope",eu="convert_concl_no_check",dn="no",aM="\xe8\xaf\x8d\xe5\xa4\xb4\xe4\xba\x86",V="Type",d3="Reset",d4="Include",R="*",es="parsing",dm="\xe5\x8f\x8d\xe5\x9b\xba\xe5\xae\x9a\xe7\x82\xb9",dJ="Restore",ci="All",S="}",d2="Parameter",at="in",dl="Cd",a8="Export",dI="\xe5\xb0\x86\xe5\xb1\x95\xe5\xbc\x80",be="auto",aJ="Debug",er="\xe8\xae\xbe\xe7\xbd\xae",d1="Class",eq=":>",bd="clear",a7=">->",aI="ML",ar="for",ep="Arguments",bZ="autounfold_one",dH="Register",as="{",a6="Let",eo="eexact",en="info_auto",d0="Instances",b7="Inline",bb=16379,dk="Axioms",em="Fixpoint",el="Opaque",cb="Instance",dZ="\xe9\x94\xae\xe5\x85\xa5",M="at",dj="Unset",dF="Modules",dG="Eval",dY="Printing",q=".",di="Write",b6="+",a5="Notation",dE="Conjectures",ek="autounfold",bY="trivial",ei="\xe5\x9b\xba\xe5\xae\x9a\xe7\x82\xb9",ej="\xe8\xb7\x9f",eg=153,ch="Existing",eh="Scheme",aL="Prefixed",dX="binder",ba="Sort",bX="associativity",ae="Scope",dD="\xe6\x9c\xaa\xe8\xae\xbe\xe7\xbd\xae",bW="Structure",bV="Path",ca="scope declared twice",p=")",ad="\xe6\x89\x93\xe5\x8d\xb0",dC="jisuanji_plugin",b5="only",j=":",a$="eauto",dB="eassumption",bU="strict",dA="Definition",dW="Term",dh="Import",ef="Infix",dz="Back",ee="Universes",dy="Reserved",ed="compat",dx="%",l=":=",aP="Variables",af="as",dV="where",dw="\xe8\xaf\x84\xe4\xbc\xb0",E=MC.jsoo_runtime,a=E.caml_new_string,bT=E.caml_register_global,B=E.caml_string_notequal,MB=E.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):E.caml_call_gen(a,[b])}function
e(a,b,c){return a.length==2?a(b,c):E.caml_call_gen(a,[b,c])}function
d(a,b,c,d){return a.length==3?a(b,c,d):E.caml_call_gen(a,[b,c,d])}function
I(a,b,c,d,e){return a.length==4?a(b,c,d,e):E.caml_call_gen(a,[b,c,d,e])}function
u(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):E.caml_call_gen(a,[b,c,d,e,f])}var
g=E.caml_get_global_data(),s=a(dC),cF=a(dC),cD=g.Vernacextend,ah=g.Attributes,L=g.Hints,ct=g.Libnames,bj=g.Printer,bl=g.Tactics,n=g.Pp,v=g.Eauto,T=g.Auto,aS=g.Ltac_plugin__Pptactic,cl=g.Mltop,h=g.Stdarg,m=g.Ltac_plugin__Tacentries,f=g.Genarg,bi=g.Geninterp,y=g.CLexer,b=g.Pcoq,k=g.CAst,df=g.Stdlib,U=g.CErrors,c$=g.Conv_oracle,o=g.Util,K=g.Option,G=g.Constrexpr_ops,i=g.Pvernac,cN=g.CWarnings,iJ=g.Locality,hn=g.Not_found,hq=g.Tacticals,fd=g.Ppconstr,fb=g.Ltac_plugin__Tacinterp,fc=g.Stdlib__list,fq=g.Ltac_plugin__Pltac,tY=g.Stdlib__stream,ls=g.Stdlib__printf,ka=g.Names,i_=g.Assert_failure;c(cl[9],s);var
cm=h[8],iK=[0,a(aK),0],ho=a(" not found"),hp=a("Hint table "),he=a(aK),hf=[0,a(aK),0],g$=a(aK),ha=[0,a(aK),0],gO=[0,1],gE=[0,0],f9=[0,0],f1=[0,1],fM=[0,0],fF=[0,1],fa=[0,0,1,0,1,0,0],e8=[0,0],eD=[0,a("\xe5\xa4\x96\xe5\x81\x87\xe8\xae\xbe"),0],eF=[0,a(dB),0],eH=a(dB),eK=a("\xe5\xa4\x96\xe7\xa1\xae\xe5\x88\x87"),eN=a(eo),eP=a(eo),e0=a(R),e2=a(w),e6=a(w),e_=a("jisuanji_hintbases"),fo=a(t),fs=a("using"),fv=a("jisuanji_auto_using"),fA=a(bY),fC=a(bY),fH=a(d$),fJ=a(d$),fO=a(bY),fP=a(cg),fR=a("debug_trivial"),fW=a(be),fY=a(be),f4=a(en),f6=a(en),ga=a(be),gb=a(cg),gd=a("debug_auto"),gg=a(Q),gi=a(Y),gj=a(ev),gl=a(ev),gr=a(a$),gt=a(a$),gy=a(be),gz=a("new"),gB=a("new_eauto"),gI=a(a$),gJ=a(cg),gL=a("debug_eauto"),gS=a(ey),gU=a(ey),gZ=a(a$),g0=a("dfs"),g2=a("dfs_eauto"),g6=a(ek),g8=a(ek),hb=a(bZ),hg=a(at),hi=a(bZ),hk=a(bZ),hr=a(w),hu=a(b3),hy=a(b3),hA=a(b3),hD=a(eu),hF=a(eu),hR=a("_"),hT=a("jisuanji_hints_path_atom"),h4=a(p),h6=a(r),h9=a(R),h_=[0,0,0],ib=a("emp"),ie=a("eps"),ii=a(P),ij=[0,0,0],im=[0,[0,0,0],0],ip=a("jisuanji_hints_path"),iC=a(j),iF=a("jisuanji_opthints"),iM=a(Q),iO=a(Y),iP=a("Cut"),iQ=a(bc),iU=a("HintCut"),Mk=[0,0],Me=[2,1,0],L_=[2,0,0],L5=[1,0,0,0],L2=[0,1],Lw=a("text"),Lm=[3,0],Lj=[3,1],Lg=[3,2],KV=[0,2],KB=a("' y"),KC=a("x '"),Jp=[59,0,[0,a(ez),[0,a(aJ),0]],0],Jm=[59,0,[0,a(ez),[0,a(aJ),0]],1],Jd=[48,1],IR=[0,0,[1,0]],Iq=[1,0],H9=[0,a(dY),[0,a("Notations"),0]],H6=[0,a(a_),[0,a(ep),0]],Hh=[18,0],Ga=a("Print Modules is obsolete; use Print Libraries instead"),Fa=[0,[9,0]],E7=[0,[9,0]],EI=[0,a(dY),[0,a(a6),0]],C9=[44,0],C6=[44,0],Cb=[0,0,0],BE=a(ca),BA=a(ca),Bw=a(ca),Bt=[0,300888093,0],Bf=[0,b$,[0,ck,0]],Bc=[0,b$,[0,ck,0]],A$=[0,676717295,0],A8=[0,9943782,0],A5=[0,968597406,0],A2=[0,ck,0],AZ=[0,b$,0],AW=[0,-751570227,0],AT=[0,1012736563,0],AQ=[0,-1020251784,0],AB=[0,0],Aw=a("Implicit Types"),Ah=a(dQ),z5=a(dQ),ze=[0,1,1],y0=[0,1,3],x8=[4,1],x5=[4,1],vZ=[0,1],vW=[0,1],vT=[0,0],vQ=[0,0],t5=a("Include Type is deprecated; use Include instead"),tZ=a(p),t0=a(R),t1=a(q),t2=a(V),tS=[0,1],tP=[0,0],tM=[0,1],tJ=[0,0],tG=[0,0],tz=[12,0,0,0],te=[12,0,0,0],so=[12,0,0,0],r0=[0,0],ry=[1,0,0],qC=[0,[0,1],2],qz=[0,4,2],qw=[0,3,2],qt=[0,2,2],qq=[0,1,1],qn=[0,1,1],qk=[0,0,0],qh=[0,0,0],po=[0,a(dE),[0,1,2]],pl=[0,a(d_),[0,1,0]],pi=[0,a(dk),[0,1,1]],pf=[0,a(aP),[0,0,0]],pc=[0,a(dM),[0,0,1]],o9=[0,1,2],o6=[0,1,0],o3=[0,1,0],o0=[0,1,1],oX=[0,0,0],oU=[0,0,0],oR=[0,0,1],oM=[0,1,2],oJ=[0,1,4],oG=[0,1,0],oD=[0,1,0],mo=[0,0,12],mk=[0,0,12],lr=[0,[11,a('Command "'),[2,0,[11,a('" expects more than one assumption.'),0]]],a('Command "%s" expects more than one assumption.')],li=[78,0],kS=[0,[0,a(dR),0],0],kN=[0,[0,a(dR),0],0],kv=[0,a(b0),0],ks=[0,a(b0),0],kp=[0,a(ds),0],km=[0,a(ds),0],i$=a("8.4"),jl=a("8.0"),jm=a("8.1"),jn=a("8.10"),jo=a("8.2"),jp=a("8.3"),ja=a("8.5"),jb=a("8.6"),jc=a("8.7"),jd=a("8.8"),je=a("8.9"),jf=a('".'),jg=a('Unknown compatibility version "'),jh=[0,a(d8)],ji=a(" not supported."),jj=a("Compatibility with version "),jk=[0,a(d8)],i9=[0,a("src/g_jisuanji_vernac.mlg"),70,9],iV=[0,a("\xe8\xbf\x91"),[0,a(dp),0]],iW=[0,a(b4),[0,a(t),[0,a(a7),[0,a(":<"),[0,a(dK),[0,a(dV),[0,a(M),0]]]]]]],iZ=a("vernac:query_command"),i0=a("vernac:subprf"),i1=a("vernac:class_rawexpr"),i2=a("vernac:thm_token"),i3=a("vernac:def_body"),i4=a("vernac:decl_notation"),i5=a("vernac:record_field"),i6=a("vernac:of_type_with_opt_coercion"),i7=a("vernac:instance_name"),i8=a("vernac:section_subset_expr"),jq=a("decorated_vernac"),jr=a("quoted_attributes"),js=a("attribute_list"),jt=a("attribute"),ju=a("attribute_value"),jv=a("vernac"),jw=a("vernac_poly"),jx=a("vernac_aux"),jy=a("located_vernac"),jB=[0,0,[0,[2,[0,a("Time")]]]],jD=[0,0,[0,[2,[0,a("\xe8\xae\xa1\xe6\x97\xb6\xe4\xba\x86")]]]],jG=[0,0,[0,[2,[0,a("Redirect")]]]],jJ=[0,0,[0,[2,[0,a("\xe9\x87\x8d\xe5\xae\x9a\xe5\x90\x91")]]]],jM=[0,0,[0,[2,[0,a("Timeout")]]]],jP=[0,0,[0,[2,[0,a("\xe8\xb6\x85\xe6\x97\xb6\xe4\xba\x86")]]]],jR=[0,0,[0,[2,[0,a("Fail")]]]],jT=[0,0,[0,[2,[0,a("\xe5\xa4\xb1\xe8\xb4\xa5\xe4\xba\x86")]]]],jV=[0,0],j2=[0,[0,a(Q)]],j3=[0,0,[0,[0,a("#[")]]],j7=[0,[0,a(t)]],ke=[0,0,[0,[0,a(dv)]]],kg=[0,[0,a(p)]],kh=[0,0,[0,[0,a(r)]]],kn=[0,0,[0,[2,[0,a("Local")]]]],kq=[0,0,[0,[2,[0,a("\xe6\x9c\xac\xe5\x9c\xb0")]]]],kt=[0,0,[0,[2,[0,a("Global")]]]],kw=[0,0,[0,[2,[0,a("\xe5\x85\xa8\xe5\x8c\x85")]]]],kB=[0,0,[0,[2,[0,a("Polymorphic")]]]],kD=[0,0,[0,[2,[0,a("\xe5\xa4\x9a\xe6\x80\x81")]]]],kF=[0,0,[0,[2,[0,a("Monomorphic")]]]],kH=[0,0,[0,[2,[0,a("\xe5\x8d\x95\xe6\x80\x81")]]]],kO=[0,[0,a(q)]],kP=[0,0,[0,[2,[0,a(ea)]]]],kT=[0,[0,a(q)]],kU=[0,0,[0,[2,[0,a(ea)]]]],kX=[0,[0,a(q)]],k0=[0,[0,a(q)]],k3=[0,[0,a(q)]],k6=[0,[0,a(q)]],la=[0,1],lg=[0,0,[0,[6,0]]],lj=[0,0,[0,[0,a(as)]]],ll=[0,0,[0,[0,a(S)]]],lt=[0,0],lu=a("pedantic"),lv=a("plural-command"),lx=a("register_token"),ly=a("register_type_token"),lz=a("register_prim_token"),lA=a("def_token"),lB=a("assumption_token"),lC=a("assumptions_token"),lD=a(cc),lE=a("univ_constraint"),lF=a("finite_token"),lG=a("cumulativity_token"),lH=a("private_token"),lI=a("reduce"),lJ=a("one_decl_notation"),lK=a("decl_sep"),lL=a("opt_constructors_or_fields"),lM=a("inductive_definition"),lN=a("constructor_list_or_record_decl"),lO=a("opt_coercion"),lP=a("corec_definition"),lQ=a("type_cstr"),lR=a("scheme"),lS=a("scheme_kind"),lT=a("record_fields"),lU=a("record_binder_body"),lV=a("record_binder"),lW=a("assum_list"),lX=a("assum_coe"),lY=a("simple_assum_coe"),lZ=a("constructor_type"),l0=a("constructor"),l6=[0,[0,a(j)]],l7=[1,0,[0,[0,a(w)]]],l8=[0,[0,a(j)]],mb=[0,[0,a(j)]],mc=[1,0,[0,[0,a(ej)]]],md=[0,[0,a(j)]],ml=[0,0,[0,[2,[0,a(a6)]]]],mp=[0,0,[0,[2,[0,a(aO)]]]],mr=[0,[0,a(w)]],mu=[0,[0,a(w)]],mv=[0,0,[0,[0,a(em)]]],my=[0,[0,a(w)]],mz=[0,0,[0,[0,a(ei)]]],mC=[0,[0,a(w)]],mD=[0,[0,0,[0,[2,[0,a(a6)]]]],[0,[0,a(em)]]],mG=[0,[0,a(w)]],mH=[0,[0,0,[0,[2,[0,a(aO)]]]],[0,[0,a(ei)]]],mJ=[0,[0,a(w)]],mK=[0,0,[0,[0,a(dT)]]],mM=[0,[0,a(w)]],mN=[0,0,[0,[0,a(dm)]]],mP=[0,[0,a(w)]],mQ=[0,[0,0,[0,[2,[0,a(a6)]]]],[0,[0,a(dT)]]],mS=[0,[0,a(w)]],mT=[0,[0,0,[0,[2,[0,a(aO)]]]],[0,[0,a(dm)]]],mV=[0,[0,a(w)]],mW=[0,0,[0,[2,[0,a(eh)]]]],mZ=[0,[0,a(t)]],m0=[0,[2,[0,a("from")]]],m1=[0,[0,0,[0,[2,[0,a("Combined")]]]],[0,[2,[0,a(eh)]]]],m4=[0,[0,a(af)]],m5=[0,0,[0,[2,[0,a(dH)]]]],m8=[0,[0,0,[0,[2,[0,a(dH)]]]],[0,[2,[0,a(b7)]]]],na=[0,[0,a(l)]],nd=[1,0,[0,[0,a(j)]]],ne=[0,0,[0,[2,[0,a("Primitive")]]]],nh=[0,0,[0,[2,[0,a("Universe")]]]],nk=[0,0,[0,[2,[0,a(ee)]]]],nm=[0,[0,a(t)]],nn=[0,0,[0,[2,[0,a("Constraint")]]]],nv=[0,0,[0,[0,a("#int63_type")]]],nz=[0,0,[0,[0,a("#int63_head0")]]],nB=[0,0,[0,[0,a("#int63_tail0")]]],nD=[0,0,[0,[0,a("#int63_add")]]],nF=[0,0,[0,[0,a("#int63_sub")]]],nH=[0,0,[0,[0,a("#int63_mul")]]],nJ=[0,0,[0,[0,a("#int63_div")]]],nL=[0,0,[0,[0,a("#int63_mod")]]],nN=[0,0,[0,[0,a("#int63_lsr")]]],nP=[0,0,[0,[0,a("#int63_lsl")]]],nR=[0,0,[0,[0,a("#int63_land")]]],nT=[0,0,[0,[0,a("#int63_lor")]]],nV=[0,0,[0,[0,a("#int63_lxor")]]],nX=[0,0,[0,[0,a("#int63_addc")]]],nZ=[0,0,[0,[0,a("#int63_subc")]]],n1=[0,0,[0,[0,a("#int63_addcarryc")]]],n3=[0,0,[0,[0,a("#int63_subcarryc")]]],n5=[0,0,[0,[0,a("#int63_mulc")]]],n7=[0,0,[0,[0,a("#int63_diveucl")]]],n9=[0,0,[0,[0,a("#int63_div21")]]],n$=[0,0,[0,[0,a("#int63_addmuldiv")]]],ob=[0,0,[0,[0,a("#int63_eq")]]],od=[0,0,[0,[0,a("#int63_lt")]]],of=[0,0,[0,[0,a("#int63_le")]]],oh=[0,0,[0,[0,a("#int63_compare")]]],ol=[0,0,[0,[0,a("Theorem")]]],on=[0,0,[0,[2,[0,a("Lemma")]]]],op=[0,0,[0,[2,[0,a("\xe8\xae\xba\xe7\x82\xb9")]]]],or=[0,0,[0,[2,[0,a("Fact")]]]],ot=[0,0,[0,[2,[0,a("Remark")]]]],ov=[0,0,[0,[2,[0,a("Corollary")]]]],ox=[0,0,[0,[2,[0,a("Proposition")]]]],oz=[0,0,[0,[2,[0,a("Property")]]]],oE=[0,0,[0,[0,a(dA)]]],oH=[0,0,[0,[0,a(eB)]]],oK=[0,0,[0,[2,[0,a("Example")]]]],oN=[0,0,[0,[2,[0,a("SubClass")]]]],oS=[0,0,[0,[0,a("Hypothesis")]]],oV=[0,0,[0,[0,a(dq)]]],oY=[0,0,[0,[0,a("\xe5\x8f\x98\xe9\x87\x8f")]]],o1=[0,0,[0,[0,a("Axiom")]]],o4=[0,0,[0,[0,a(d2)]]],o7=[0,0,[0,[0,a(dt)]]],o_=[0,0,[0,[2,[0,a("Conjecture")]]]],pd=[0,0,[0,[2,[0,a(dM)]]]],pg=[0,0,[0,[2,[0,a(aP)]]]],pj=[0,0,[0,[2,[0,a(dk)]]]],pm=[0,0,[0,[2,[0,a(d_)]]]],pp=[0,0,[0,[2,[0,a(dE)]]]],pu=[0,[0,a(p)]],pv=[0,[0,0,[0,[2,[0,a(b7)]]]],[0,[0,a(r)]]],py=[0,[0,a(p)]],pz=[0,[0,0,[0,[2,[0,a(dI)]]]],[0,[0,a(r)]]],pB=[0,0,[0,[2,[0,a(b7)]]]],pD=[0,0,[0,[2,[0,a(dI)]]]],pL=[1,0,[0,[0,a("<")]]],pN=[1,0,[0,[0,a(dv)]]],pP=[1,0,[0,[0,a("<=")]]],pW=[0,[0,a(S)]],pY=[1,0,[0,[0,a(b6)]]],p0=[0,[0,a(t)]],p1=[1,0,[0,[0,a(P)]]],p5=[1,0,[0,[0,a(S)]]],p7=[1,0,[0,[0,a("|}")]]],p_=[1,0,[0,[0,a(b6)]]],qb=[0,0,[0,[0,a(dN)]]],qi=[0,0,[0,[2,[0,a("Inductive")]]]],ql=[0,0,[0,[2,[0,a("\xe5\xbd\x92\xe7\xba\xb3\xe7\x9a\x84")]]]],qo=[0,0,[0,[2,[0,a("CoInductive")]]]],qr=[0,0,[0,[2,[0,a("\xe5\x8f\x8d\xe5\xbd\x92\xe7\xba\xb3\xe7\x9a\x84")]]]],qu=[0,0,[0,[2,[0,a("Variant")]]]],qx=[0,0,[0,[2,[0,a("Record")]]]],qA=[0,0,[0,[2,[0,a(bW)]]]],qD=[0,0,[0,[2,[0,a(d1)]]]],qH=[0,0,[0,[2,[0,a("Cumulative")]]]],qJ=[0,0,[0,[2,[0,a("NonCumulative")]]]],qN=[0,0,[0,[2,[0,a("Private")]]]],qT=[0,[0,a(l)]],qW=[0,[0,a(l)]],qX=[0,[0,a(j)]],q0=[0,[0,a(j)]],q4=[0,[0,a(at)]],q5=[0,0,[0,[2,[0,a(dG)]]]],q8=[0,[0,a(b_)]],q9=[0,0,[0,[2,[0,a(dw)]]]],re=[1,[1,0,[0,[0,a(j)]]],[0,[2,0]]],rg=[0,[0,a(l)]],rj=[0,0,[0,[2,[0,a(b1)]]]],rl=[0,0,[0,[2,[0,a("\xe5\x92\x8c")]]]],rp=[0,0,[0,[0,a(dV)]]],rr=[0,0,[0,[0,a(dp)]]],rw=[0,0,[0,[0,a(l)]]],rH=[1,0,[0,[0,a(j)]]],rK=[0,[0,a(P)]],rL=[0,0,[0,[0,a(P)]]],rO=[0,[0,a(P)]],rP=[0,[0,a(P)]],rU=[0,[0,a(S)]],rV=[0,[0,a(as)]],rX=[0,[0,a(S)]],rY=[0,0,[0,[0,a(as)]]],r4=[0,0,[0,[0,a(aQ)]]],sb=[1,0,[0,[0,a(l)]]],si=[1,0,[0,[0,a(l)]]],sm=[0,0,[0,[0,a(j)]]],su=[0,[0,a(l)]],sy=[0,[2,[0,a(ba)]]],sz=[0,[0,0,[0,[2,[0,a("Induction")]]]],[0,[0,a(ar)]]],sC=[0,[2,[0,a(ba)]]],sD=[0,[0,0,[0,[2,[0,a("Minimality")]]]],[0,[0,a(ar)]]],sG=[0,[2,[0,a(ba)]]],sH=[0,[0,0,[0,[2,[0,a("Elimination")]]]],[0,[0,a(ar)]]],sK=[0,[2,[0,a(ba)]]],sL=[0,[0,0,[0,[2,[0,a("Case")]]]],[0,[0,a(ar)]]],sO=[0,[0,0,[0,[2,[0,a("Equality")]]]],[0,[0,a(ar)]]],sV=[1,0,[0,[0,a(P)]]],sY=[0,[0,a(b4)]],s0=[0,[0,a(b4)]],s9=[0,[0,a(l)]],ta=[0,[0,a(l)]],tn=[0,[0,a(p)]],to=[0,0,[0,[0,a(r)]]],tH=[0,0,[0,[0,a(":>>")]]],tK=[0,[0,0,[0,[0,a(eq)]]],[0,[0,a(aQ)]]],tN=[0,0,[0,[0,a(eq)]]],tQ=[0,[0,[0,0,[0,[0,a(j)]]],[0,[0,a(aQ)]]],[0,[0,a(aQ)]]],tT=[0,[0,0,[0,[0,a(j)]]],[0,[0,a(aQ)]]],tV=[0,0,[0,[0,a(j)]]],t3=a("test_only_starredidentrefs"),t6=a("deprecated"),t7=a("deprecated-include-type"),t8=a("export_token"),t9=a("ext_module_type"),t_=a("ext_module_expr"),t$=a("check_module_type"),ua=a("check_module_types"),ub=a("of_module_type"),uc=a("of_module_type_check"),ud=a("is_module_type"),ue=a("is_module_expr"),uf=a("functor_app_annot"),ug=a("module_expr_inl"),uh=a("module_type_inl"),ui=a("module_binder"),uj=a("module_expr_atom"),uk=a("with_declaration"),ul=a("starredidentref"),um=a("ssexpr"),uq=[0,0,[0,[2,[0,a(X)]]]],ut=[0,[0,0,[0,[2,[0,a(ac)]]]],[0,[2,[0,a(aL)]]]],uw=[0,[0,0,[0,[2,[0,a(ag)]]]],[0,[2,[0,a(aM)]]]],uz=[0,[0,[0,0,[0,[2,[0,a(ac)]]]],[0,[2,[0,a(aL)]]]],[0,[2,[0,a("Alias")]]]],uC=[0,[0,[0,0,[0,[2,[0,a(ag)]]]],[0,[2,[0,a(aM)]]]],[0,[2,[0,a("\xe5\x88\xab\xe5\x8f\xb7")]]]],uF=[0,[0,0,[0,[2,[0,a(X)]]]],[0,[0,a(V)]]],uI=[0,0,[0,[2,[0,a(ac)]]]],uL=[0,0,[0,[2,[0,a(ag)]]]],uO=[0,[0,a(j)]],uP=[0,[0,0,[0,[2,[0,a(av)]]]],[0,[2,[0,a(X)]]]],uS=[0,[0,a(j)]],uT=[0,[0,[0,0,[0,[2,[0,a(ac)]]]],[0,[2,[0,a(aL)]]]],[0,[2,[0,a(d2)]]]],uW=[0,[0,a(j)]],uX=[0,[0,[0,0,[0,[2,[0,a(ag)]]]],[0,[2,[0,a(aM)]]]],[0,[2,[0,a(dt)]]]],u0=[0,0,[0,[2,[0,a(eb)]]]],u3=[0,0,[0,[2,[0,a("\xe9\x83\xa8\xe5\x88\x86")]]]],u6=[0,0,[0,[2,[0,a("Chapter")]]]],u9=[0,0,[0,[2,[0,a("End")]]]],va=[0,0,[0,[2,[0,a("\xe7\xbb\x93\xe6\x9d\x9f")]]]],vd=[0,[0,a(l)]],ve=[0,0,[0,[2,[0,a("Collection")]]]],vh=[0,0,[0,[2,[0,a(dr)]]]],vk=[0,0,[0,[2,[0,a(ex)]]]],vn=[0,[2,[0,a(dr)]]],vo=[0,0,[0,[2,[0,a("From")]]]],vr=[0,[2,[0,a(ex)]]],vs=[0,0,[0,[2,[0,a("\xe4\xbb\x8e")]]]],vv=[0,0,[0,[2,[0,a(dh)]]]],vy=[0,0,[0,[2,[0,a(ec)]]]],vB=[0,0,[0,[2,[0,a(a8)]]]],vE=[0,0,[0,[2,[0,a(bf)]]]],vG=[0,0,[0,[2,[0,a(d4)]]]],vI=[0,0,[0,[2,[0,a(dS)]]]],vK=[0,[0,0,[0,[2,[0,a(d4)]]]],[0,[0,a(V)]]],vM=[0,[0,0,[0,[2,[0,a(dS)]]]],[0,[0,a(b9)]]],vR=[0,0,[0,[2,[0,a(dh)]]]],vU=[0,0,[0,[2,[0,a(ec)]]]],vX=[0,0,[0,[2,[0,a(a8)]]]],v0=[0,0,[0,[2,[0,a(bf)]]]],v5=[0,0,[0,[0,a(dL)]]],v9=[0,0,[0,[0,a(dL)]]],wb=[0,0,[0,[0,a(dK)]]],wi=[0,0,[0,[0,a(j)]]],wq=[0,0,[0,[0,a(l)]]],wv=[0,0,[0,[0,a(l)]]],wB=[0,[0,a(Q)]],wC=[0,[0,[0,[0,0,[0,[0,a(Y)]]],[0,[2,[0,a(cc)]]]],[0,[0,a(M)]]],[0,[2,[0,a(W)]]]],wE=[0,[0,[0,[0,0,[0,[0,a(Y)]]],[0,[2,[0,a(dn)]]]],[0,[2,[0,a(cc)]]]],[0,[0,a(Q)]]],wK=[0,0,[0,[0,a(aR)]]],wQ=[0,0,[0,[0,a(aR)]]],wW=[0,[0,a(p)]],wX=[0,[0,a(j)]],wY=[0,0,[0,[0,a(r)]]],w8=[0,[0,a(p)]],w9=[0,0,[0,[0,a(r)]]],xb=[0,[0,a(l)]],xc=[0,0,[0,[0,a(dA)]]],xf=[0,[0,a(l)]],xg=[0,0,[0,[0,a(eB)]]],xj=[0,[0,a(l)]],xk=[0,0,[0,[2,[0,a(X)]]]],xn=[0,[0,a(l)]],xo=[0,[0,0,[0,[2,[0,a(ac)]]]],[0,[2,[0,a(aL)]]]],xr=[0,[0,a(l)]],xs=[0,[0,0,[0,[2,[0,a(ag)]]]],[0,[2,[0,a(aM)]]]],xy=[0,[0,a(p)]],xz=[0,0,[0,[0,a(r)]]],xE=[0,[0,a(w)]],xH=[0,[0,a(ej)]],xK=[0,[0,a(at)]],xL=[0,0,[0,[0,a("let")]]],xO=[0,[0,a(b_)]],xP=[0,0,[0,[0,a(aO)]]],xZ=[0,[0,a(R)]],x1=[0,0,[0,[0,a(V)]]],x3=[0,0,[0,[0,a(b9)]]],x6=[0,[0,0,[0,[0,a(V)]]],[0,[0,a(R)]]],x9=[0,[0,0,[0,[0,a(b9)]]],[0,[0,a(R)]]],yc=[0,[0,a(p)]],yd=[0,0,[0,[0,a(r)]]],yf=[0,[0,a(R)]],yg=[0,[0,a(p)]],yh=[0,0,[0,[0,a(r)]]],yj=[0,[0,a(p)]],yk=[0,0,[0,[0,a(r)]]],ym=[0,[0,a(R)]],yn=[0,[0,a(p)]],yo=[0,0,[0,[0,a(r)]]],yp=[0,a("0")],ys=[0,[0,a(b8)]],yu=[0,[0,a(b6)]],yv=[0,a("50")],yy=[0,0,[0,[0,a(b8)]]],yz=[0,a("35")],yB=a("arguments_modifier"),yC=a(et),yD=a("argument_spec"),yE=a("argument_spec_block"),yF=a("more_implicits_block"),yG=a("strategy_level"),yH=a("reserv_list"),yI=a("reserv_tuple"),yJ=a("simple_reserv"),yN=[0,0,[0,[2,[0,a(d5)]]]],yQ=[0,0,[0,[2,[0,a(el)]]]],yV=[0,[0,a(Q)]],yW=[0,[0,a(Y)]],yX=[0,0,[0,[2,[0,a(eA)]]]],y6=[1,0,[0,[2,[0,a(bW)]]]],y7=[0,0,[0,[2,[0,a(b2)]]]],za=[1,0,[0,[2,[0,a(bW)]]]],zb=[0,0,[0,[2,[0,a(b2)]]]],zf=[0,0,[0,[2,[0,a(aN)]]]],zi=[0,[0,a(a7)]],zj=[0,[0,a(j)]],zk=[0,[0,0,[0,[2,[0,a("Identity")]]]],[0,[2,[0,a(aN)]]]],zn=[0,[0,a(a7)]],zo=[0,[0,a(j)]],zp=[0,0,[0,[2,[0,a(aN)]]]],zs=[0,[0,a(a7)]],zt=[0,[0,a(j)]],zu=[0,0,[0,[2,[0,a(aN)]]]],zx=[0,0,[0,[2,[0,a("Context")]]]],zC=[0,[0,a(S)]],zD=[1,[1,0,[0,[0,a(l)]]],[0,[0,a(as)]]],zG=[1,0,[0,[0,a(l)]]],zK=a(dP),zN=[1,0,[0,[0,a(aR)]]],zP=[0,[0,a(j)]],zQ=[0,0,[0,[2,[0,a(cb)]]]],zT=[0,[0,0,[0,[2,[0,a(ch)]]]],[0,[2,[0,a(cb)]]]],zY=[1,0,[0,[0,a(P)]]],zZ=[0,[0,0,[0,[2,[0,a(ch)]]]],[0,[2,[0,a(d0)]]]],z2=[0,[0,0,[0,[2,[0,a(ch)]]]],[0,[2,[0,a(d1)]]]],z7=[0,[0,a(t)]],z8=[1,0,[0,[0,a(j)]]],Aa=[0,[0,a(t)]],Ac=[1,0,[0,[0,a(t)]]],Ae=[0,0,[0,[2,[0,a(ep)]]]],Aj=[0,[0,a(t)]],Ak=[1,0,[0,[0,a(j)]]],Ao=[0,[0,a(t)]],Aq=[1,0,[0,[0,a(t)]]],As=[0,0,[0,[2,[0,a(dZ)]]]],Au=[0,[0,0,[0,[2,[0,a(a_)]]]],[0,[0,a(V)]]],Ax=[0,[0,0,[0,[2,[0,a(a_)]]]],[0,[2,[0,a("Types")]]]],AC=[1,[1,0,[0,[2,[0,a(ci)]]]],[0,[2,[0,a(aP)]]]],AE=[1,[1,0,[0,[2,[0,a("No")]]]],[0,[2,[0,a(aP)]]]],AJ=[1,0,[0,[0,a(dq)]]],AL=[1,0,[0,[2,[0,a(aP)]]]],AM=[0,0,[0,[2,[0,a("Generalizable")]]]],AR=[0,[0,0,[0,[2,[0,a(d7)]]]],[0,[2,[0,a("nomatch")]]]],AU=[0,[0,0,[0,[2,[0,a(d7)]]]],[0,[2,[0,a("never")]]]],AX=[0,[0,0,[0,[2,[0,a("default")]]]],[0,[2,[0,a(a9)]]]],A0=[0,[0,0,[0,[2,[0,a(bd)]]]],[0,[2,[0,a(a9)]]]],A3=[0,[0,0,[0,[2,[0,a(bd)]]]],[0,[2,[0,a(bg)]]]],A6=[0,0,[0,[2,[0,a("rename")]]]],A9=[0,0,[0,[2,[0,a("assert")]]]],Ba=[0,[0,0,[0,[2,[0,a("extra")]]]],[0,[2,[0,a(bg)]]]],Bd=[0,[0,[0,[0,0,[0,[2,[0,a(bd)]]]],[0,[2,[0,a(bg)]]]],[0,[2,[0,a(b1)]]]],[0,[2,[0,a(a9)]]]],Bg=[0,[0,[0,[0,0,[0,[2,[0,a(bd)]]]],[0,[2,[0,a(a9)]]]],[0,[2,[0,a(b1)]]]],[0,[2,[0,a(bg)]]]],Bk=[0,[0,0,[0,[0,a(dx)]]],[0,[2,0]]],Bp=[0,0,[5,[0,[0,a(aR)]]]],Bu=[0,0,[0,[0,a("/")]]],Bx=[0,[0,a(p)]],By=[0,0,[0,[0,a(r)]]],BB=[0,[0,a(Q)]],BC=[0,0,[0,[0,a(Y)]]],BF=[0,[0,a(S)]],BG=[0,0,[0,[0,a(as)]]],BN=[0,[0,a(Q)]],BO=[0,0,[0,[0,a(Y)]]],BR=[0,[0,a(S)]],BS=[0,0,[0,[0,a(as)]]],BV=[0,0,[0,[2,[0,a("expand")]]]],BX=[0,0,[0,[2,[0,a("opaque")]]]],B1=[0,0,[0,[2,[0,a("transparent")]]]],B$=[0,0,[0,[0,a(P)]]],Cj=[0,[0,a(p)]],Ck=[0,0,[0,[0,a(r)]]],Cp=[0,[0,a(j)]],Cq=a("printable"),Cr=a("printunivs_subgraph"),Cs=a("locatable"),Ct=a("option_setting"),Cu=a("option_ref_value"),Cv=a("option_table"),Cw=a("as_dirpath"),Cx=a("ne_in_or_out_modules"),Cy=a("in_or_out_modules"),Cz=a("comment"),CA=a("positive_search_mark"),CB=a(et),CC=a("searchabout_query"),CD=a("searchabout_queries"),CE=a("univ_name_list"),CH=[0,[0,0,[0,[2,[0,a(a8)]]]],[0,[0,a(du)]]],CJ=[0,[0,0,[0,[2,[0,a(bf)]]]],[0,[0,a(er)]]],CL=[0,[0,0,[0,[2,[0,a(a8)]]]],[0,[2,[0,a(dj)]]]],CN=[0,[0,0,[0,[2,[0,a(bf)]]]],[0,[2,[0,a(dD)]]]],CR=[0,0,[0,[2,[0,a("Comments")]]]],CV=a(dP),CY=[1,0,[0,[0,a(aR)]]],C1=[0,[0,a(j)]],C2=[0,[0,0,[0,[2,[0,a(av)]]]],[0,[2,[0,a(cb)]]]],C4=[0,[0,[0,0,[0,[2,[0,a(av)]]]],[0,[2,[0,a(ae)]]]],[0,[2,0]]],C7=[0,0,[0,[2,[0,a("Pwd")]]]],C_=[0,0,[0,[2,[0,a(dl)]]]],Db=[0,0,[0,[2,[0,a(dl)]]]],Dh=[1,0,[0,[2,0]]],Dk=[1,0,[0,[2,[0,a("Verbose")]]]],Dm=[0,0,[0,[2,[0,a("Load")]]]],Dp=[0,[0,[0,0,[0,[2,[0,a(av)]]]],[0,[2,[0,a(aI)]]]],[0,[2,[0,a(X)]]]],Dr=[0,0,[0,[2,[0,a("Locate")]]]],Du=[0,[0,0,[0,[2,[0,a(au)]]]],[0,[2,[0,a(a4)]]]],Dx=[0,[0,[0,0,[0,[2,[0,a(au)]]]],[0,[2,[0,a(dU)]]]],[0,[2,[0,a(a4)]]]],DA=[0,[0,0,[0,[2,[0,a(cj)]]]],[0,[2,[0,a(a4)]]]],DD=[0,[0,a(af)]],DE=[0,0,[0,[2,[0,a("AddPath")]]]],DH=[0,[0,a(af)]],DI=[0,0,[0,[2,[0,a("AddRecPath")]]]],DL=[0,0,[0,[2,[0,a("DelPath")]]]],DO=[0,0,[0,[0,a(V)]]],DQ=[0,0,[0,[2,[0,a(Z)]]]],DS=[0,0,[0,[2,[0,a(ad)]]]],DV=[0,0,[0,[2,[0,a(Z)]]]],DY=[0,0,[0,[2,[0,a(ad)]]]],D1=[0,[0,[0,0,[0,[2,[0,a(Z)]]]],[0,[2,[0,a(X)]]]],[0,[0,a(V)]]],D4=[0,[0,0,[0,[2,[0,a(Z)]]]],[0,[2,[0,a(ac)]]]],D7=[0,[0,0,[0,[2,[0,a(ad)]]]],[0,[2,[0,a(ag)]]]],D_=[0,[0,0,[0,[2,[0,a(Z)]]]],[0,[2,[0,a(X)]]]],Eb=[0,[0,[0,0,[0,[2,[0,a(Z)]]]],[0,[2,[0,a(ac)]]]],[0,[2,[0,a(aL)]]]],Ee=[0,[0,[0,0,[0,[2,[0,a(ad)]]]],[0,[2,[0,a(ag)]]]],[0,[2,[0,a(aM)]]]],Eh=[0,[0,0,[0,[2,[0,a(Z)]]]],[0,[2,[0,a("Namespace")]]]],Ek=[0,0,[0,[2,[0,a("Inspect")]]]],En=[0,[0,[0,0,[0,[2,[0,a(au)]]]],[0,[2,[0,a(aI)]]]],[0,[2,[0,a(bV)]]]],Eq=[0,[0,[0,[0,0,[0,[2,[0,a(au)]]]],[0,[2,[0,a(dU)]]]],[0,[2,[0,a(aI)]]]],[0,[2,[0,a(bV)]]]],Es=[0,0,[0,[0,a(du)]]],Eu=[0,0,[0,[0,a(er)]]],Ew=[0,0,[0,[2,[0,a(dj)]]]],Ey=[0,0,[0,[2,[0,a(dD)]]]],EA=[0,[0,0,[0,[2,[0,a(Z)]]]],[0,[2,[0,a("Table")]]]],EC=[0,[0,0,[0,[2,[0,a(ad)]]]],[0,[2,[0,a("\xe8\xa1\xa8\xe6\xa0\xbc")]]]],EE=[0,[0,[0,0,[0,[2,[0,a(au)]]]],[0,[2,0]]],[0,[2,0]]],EG=[0,[0,0,[0,[2,[0,a(au)]]]],[0,[2,0]]],EJ=[0,[0,[0,0,[0,[2,[0,a("\xe5\x8a\xa0")]]]],[0,[2,[0,a(ad)]]]],[0,[2,[0,a(aO)]]]],EL=[0,[0,a(ar)]],EM=[0,0,[0,[2,[0,a(d6)]]]],EO=[0,0,[0,[2,[0,a(d6)]]]],EQ=[0,[0,[0,0,[0,[2,[0,a(cj)]]]],[0,[2,0]]],[0,[2,0]]],ES=[0,[0,0,[0,[2,[0,a(cj)]]]],[0,[2,0]]],EX=[0,[0,a(q)]],EY=[0,[0,a(at)]],EZ=[0,0,[0,[2,[0,a(dG)]]]],E2=[0,[0,a(q)]],E3=[0,[0,a(b_)]],E4=[0,0,[0,[2,[0,a(dw)]]]],E8=[0,[0,a(q)]],E9=[0,0,[0,[2,[0,a("Compute")]]]],Fb=[0,[0,a(q)]],Fc=[0,0,[0,[2,[0,a("\xe8\xae\xa1\xe7\xae\x97")]]]],Ff=[0,[0,a(q)]],Fg=[0,0,[0,[2,[0,a("Check")]]]],Fj=[0,[0,a(q)]],Fk=[0,0,[0,[2,[0,a("\xe6\xa0\xa1\xe9\xaa\x8c")]]]],Fn=[0,[0,a(q)]],Fo=[0,0,[0,[2,[0,a("About")]]]],Fr=[0,[0,a(q)]],Fs=[0,0,[0,[2,[0,a("\xe5\x85\xb3\xe4\xba\x8e")]]]],Fv=[0,[0,a(q)]],Fw=[0,0,[0,[2,[0,a("SearchHead")]]]],Fz=[0,[0,a(q)]],FA=[0,0,[0,[2,[0,a("SearchPattern")]]]],FD=[0,[0,a(q)]],FE=[0,0,[0,[2,[0,a("SearchRewrite")]]]],FH=[0,[0,a(q)]],FI=[0,0,[0,[2,[0,a("\xe6\x90\x9c\xe7\xb4\xa2'\xe6\x94\xb9\xe5\x86\x99")]]]],FK=[0,[0,a(q)]],FL=[0,0,[0,[2,[0,a("Search")]]]],FN=[0,[0,a(q)]],FO=[0,0,[0,[2,[0,a("\xe6\x90\x9c\xe7\xb4\xa2")]]]],FQ=[0,[0,a(q)]],FR=[0,0,[0,[2,[0,a(ew)]]]],FT=[0,[0,a(q)]],FU=[0,[0,a(Q)]],FV=[0,[0,0,[0,[2,[0,a(ew)]]]],[0,[0,a(Y)]]],F0=[0,0,[0,[2,[0,a(dW)]]]],F2=[0,0,[0,[2,[0,a(ci)]]]],F5=[0,0,[0,[2,[0,a(eb)]]]],F7=[0,[0,0,[0,[2,[0,a("Grammar")]]]],[0,[2,0]]],F_=[0,0,[0,[2,[0,a(a4)]]]],Gb=[0,0,[0,[2,[0,a(dF)]]]],Gd=[0,0,[0,[2,[0,a("Libraries")]]]],Gf=[0,[0,0,[0,[2,[0,a(aI)]]]],[0,[2,[0,a(bV)]]]],Gh=[0,[0,0,[0,[2,[0,a(aI)]]]],[0,[2,[0,a(dF)]]]],Gj=[0,[0,0,[0,[2,[0,a(aJ)]]]],[0,[2,[0,a("GC")]]]],Gl=[0,0,[0,[2,[0,a("Graph")]]]],Gn=[0,0,[0,[2,[0,a("Classes")]]]],Gp=[0,0,[0,[2,[0,a("TypeClasses")]]]],Gs=[0,0,[0,[2,[0,a(d0)]]]],Gu=[0,0,[0,[2,[0,a("Coercions")]]]],Gw=[0,[0,0,[0,[2,[0,a(aN)]]]],[0,[2,[0,a("Paths")]]]],Gy=[0,[0,0,[0,[2,[0,a(b2)]]]],[0,[2,[0,a("Projections")]]]],GA=[0,0,[0,[2,[0,a("Tables")]]]],GC=[0,0,[0,[2,[0,a("Options")]]]],GE=[0,0,[0,[2,[0,a(bc)]]]],GH=[0,0,[0,[2,[0,a(bc)]]]],GJ=[0,[0,0,[0,[2,[0,a(bc)]]]],[0,[0,a(R)]]],GL=[0,[0,0,[0,[2,[0,a("HintDb")]]]],[0,[2,0]]],GN=[0,0,[0,[2,[0,a("Scopes")]]]],GP=[0,[0,0,[0,[2,[0,a(ae)]]]],[0,[2,0]]],GR=[0,[0,0,[0,[2,[0,a("Visibility")]]]],[5,[0,[2,0]]]],GU=[0,0,[0,[2,[0,a(a_)]]]],GZ=[0,[2,[0,a(ee)]]],G1=[1,0,[0,[2,[0,a("Sorted")]]]],G5=[0,0,[0,[2,[0,a("Assumptions")]]]],G8=[0,[0,0,[0,[2,[0,a(el)]]]],[0,[2,[0,a(cd)]]]],G$=[0,[0,0,[0,[2,[0,a(d5)]]]],[0,[2,[0,a(cd)]]]],Hc=[0,[0,0,[0,[2,[0,a(ci)]]]],[0,[2,[0,a(cd)]]]],Hf=[0,0,[0,[2,[0,a(eA)]]]],Hi=[0,0,[0,[2,[0,a("Strategies")]]]],Hk=[0,0,[0,[2,[0,a("Registered")]]]],Hp=[0,[0,a(p)]],Hq=[0,[0,0,[0,[2,[0,a("Subgraph")]]]],[0,[0,a(r)]]],Ht=[0,0,[0,[2,[0,a("Funclass")]]]],Hv=[0,0,[0,[2,[0,a("Sortclass")]]]],HD=[0,0,[0,[2,[0,a(dW)]]]],HG=[0,0,[0,[2,[0,a("File")]]]],HJ=[0,0,[0,[2,[0,a("Library")]]]],HM=[0,0,[0,[2,[0,a(X)]]]],HS=[0,0,[0,[5,0]]],HY=[0,0,[0,[5,0]]],H4=[1,0,[0,[2,0]]],H7=[0,[0,0,[0,[2,[0,a("\xe9\x9a\x90\xe5\x90\xab")]]]],[0,[2,[0,a(dZ)]]]],H_=[0,[0,0,[0,[2,[0,a(ad)]]]],[0,[2,[0,a(cf)]]]],If=[1,0,[0,[0,a(af)]]],Ij=[0,0,[0,[2,[0,a("inside")]]]],Im=[0,0,[0,[2,[0,a("outside")]]]],Iw=[0,0,[0,[5,0]]],IB=[0,0,[0,[0,a(b8)]]],IG=[0,[0,0,[0,[0,a(dx)]]],[0,[2,0]]],IW=[0,[0,a(S)]],IX=[0,0,[0,[0,a(dN)]]],I0=[0,[0,[0,0,[0,[2,[0,a(di)]]]],[0,[2,[0,a(bh)]]]],[0,[2,0]]],I3=[0,[0,0,[0,[2,[0,a(di)]]]],[0,[2,[0,a(bh)]]]],I5=[0,[0,[0,0,[0,[2,[0,a(dJ)]]]],[0,[2,[0,a(bh)]]]],[0,[2,0]]],I8=[0,[0,0,[0,[2,[0,a(dJ)]]]],[0,[2,[0,a(bh)]]]],I_=[0,[0,0,[0,[2,[0,a(d3)]]]],[0,[2,[0,a("Initial")]]]],Jb=[0,0,[0,[2,[0,a(d3)]]]],Je=[0,0,[0,[2,[0,a(dz)]]]],Jh=[0,0,[0,[2,[0,a(dz)]]]],Jk=[0,0,[0,[2,[0,a("BackTo")]]]],Jn=[0,[0,0,[0,[2,[0,a(aJ)]]]],[0,[2,[0,a("On")]]]],Jq=[0,[0,0,[0,[2,[0,a(aJ)]]]],[0,[2,[0,a("Off")]]]],Jt=[0,[0,[0,[0,0,[0,[2,[0,a(av)]]]],[0,[2,[0,a("Reduction")]]]],[0,[2,0]]],[0,[0,a(l)]]],Jv=[0,[0,[0,[0,0,[0,[2,[0,a(av)]]]],[0,[2,[0,a("Custom")]]]],[0,[2,[0,a("Entry")]]]],[0,[2,0]]],Jx=a("only_parsing"),Jy=a(W),Jz=a("syntax_modifier"),JA=a("syntax_extension_type"),JB=a("at_level"),JC=a("constr_as_binder_kind"),JF=[0,[0,[0,0,[0,[2,[0,a("Open")]]]],[0,[2,[0,a(ae)]]]],[0,[2,0]]],JH=[0,[0,[0,0,[0,[2,[0,a("Close")]]]],[0,[2,[0,a(ae)]]]],[0,[2,0]]],JJ=[0,[0,[0,[0,[0,0,[0,[2,[0,a("Delimit")]]]],[0,[2,[0,a(ae)]]]],[0,[2,0]]],[0,[0,a(w)]]],[0,[2,0]]],JL=[0,[0,[0,0,[0,[2,[0,a("Undelimit")]]]],[0,[2,[0,a(ae)]]]],[0,[2,0]]],JN=[0,[0,[0,[0,0,[0,[2,[0,a("Bind")]]]],[0,[2,[0,a(ae)]]]],[0,[2,0]]],[0,[0,a(w)]]],JR=[1,[1,0,[0,[0,a(j)]]],[0,[2,0]]],JU=[0,[0,a(p)]],JV=[0,[0,a(t)]],JW=[1,0,[0,[0,a(r)]]],JZ=[0,[0,a(l)]],J0=[0,0,[0,[2,[0,a(ef)]]]],J3=[0,[0,a(l)]],J4=[0,0,[0,[2,[0,a(a5)]]]],J7=[0,[0,a(l)]],J8=[0,0,[0,[2,[0,a(cf)]]]],Ka=[1,[1,0,[0,[0,a(j)]]],[0,[2,0]]],Kd=[0,[0,a(p)]],Ke=[0,[0,a(t)]],Kf=[1,0,[0,[0,a(r)]]],Ki=[0,[0,a(l)]],Kj=[0,0,[0,[2,[0,a(a5)]]]],Kn=[1,[1,0,[0,[0,a(j)]]],[0,[2,0]]],Kq=[0,[0,a(p)]],Kr=[0,[0,a(t)]],Ks=[1,0,[0,[0,a(r)]]],Kv=[0,[0,a(l)]],Kw=[0,0,[0,[2,[0,a(cf)]]]],Ky=[0,[0,[0,[0,[0,0,[0,[2,[0,a("Format")]]]],[0,[2,[0,a(a5)]]]],[0,[5,0]]],[0,[5,0]]],[0,[5,0]]],KE=[0,[0,a(p)]],KF=[0,[0,a(t)]],KG=[1,0,[0,[0,a(r)]]],KJ=[0,[0,0,[0,[2,[0,a(dy)]]]],[0,[2,[0,a(ef)]]]],KN=[0,[0,a(p)]],KO=[0,[0,a(t)]],KP=[1,0,[0,[0,a(r)]]],KS=[0,[0,0,[0,[2,[0,a(dy)]]]],[0,[2,[0,a(a5)]]]],KW=[0,[0,[0,[0,0,[0,[0,a(r)]]],[0,[2,[0,a(b5)]]]],[0,[2,[0,a(es)]]]],[0,[0,a(p)]]],KY=[0,[0,[0,[0,0,[0,[0,a(r)]]],[0,[2,[0,a(ed)]]]],[0,[5,0]]],[0,[0,a(p)]]],K4=[0,0,[0,[2,[0,a(W)]]]],K6=[0,[0,0,[0,[2,[0,a("next")]]]],[0,[2,[0,a(W)]]]],K$=[0,[0,0,[0,[0,a(M)]]],[0,[2,[0,a(W)]]]],Lb=[0,[0,[0,0,[0,[0,a(at)]]],[0,[2,[0,a(ce)]]]],[0,[2,0]]],Le=[0,[0,[0,[0,[0,0,[0,[0,a(at)]]],[0,[2,[0,a(ce)]]]],[0,[2,0]]],[0,[0,a(M)]]],[0,[2,[0,a(W)]]]],Lh=[0,[0,0,[0,[2,[0,a("left")]]]],[0,[2,[0,a(bX)]]]],Lk=[0,[0,0,[0,[2,[0,a("right")]]]],[0,[2,[0,a(bX)]]]],Ln=[0,[0,0,[0,[2,[0,a(dn)]]]],[0,[2,[0,a(bX)]]]],Lp=[0,[0,0,[0,[2,[0,a(b5)]]]],[0,[2,[0,a("printing")]]]],Lr=[0,[0,0,[0,[2,[0,a(b5)]]]],[0,[2,[0,a(es)]]]],Lt=[0,[0,0,[0,[2,[0,a(ed)]]]],[0,[5,0]]],Ly=[1,0,[0,[5,0]]],LB=[1,0,[0,[5,0]]],LC=[0,0,[0,[2,[0,a("format")]]]],LG=[0,[0,a(M)]],LH=[0,[0,a(t)]],LJ=[1,0,[0,[2,0]]],LK=[0,[0,0,[0,[2,0]]],[0,[0,a(t)]]],LM=[0,[0,0,[0,[2,0]]],[0,[0,a(M)]]],LO=[0,[0,0,[0,[2,0]]],[0,[0,a(M)]]],LQ=[0,0,[0,[2,0]]],LS=[0,0,[0,[2,0]]],LW=[0,0,[0,[2,[0,a(dO)]]]],LY=[0,0,[0,[2,[0,a(b0)]]]],L0=[0,0,[0,[2,[0,a("bigint")]]]],L3=[0,0,[0,[2,[0,a(dX)]]]],L6=[0,0,[0,[2,[0,a(d9)]]]],L8=[0,0,[0,[2,[0,a(d9)]]]],L$=[0,0,[0,[2,[0,a(aw)]]]],Mc=[0,[0,[0,0,[0,[2,[0,a(aw)]]]],[0,[0,a(M)]]],[0,[2,[0,a(W)]]]],Mf=[0,[0,0,[0,[2,[0,a(bU)]]]],[0,[2,[0,a(aw)]]]],Mi=[0,[0,[0,[0,0,[0,[2,[0,a(bU)]]]],[0,[2,[0,a(aw)]]]],[0,[0,a(M)]]],[0,[2,[0,a(W)]]]],Ml=[0,[0,0,[0,[2,[0,a("closed")]]]],[0,[2,[0,a(dX)]]]],Mn=[0,[0,0,[0,[2,[0,a(ce)]]]],[0,[2,0]]],Mr=[0,0,[0,[0,a(M)]]],Mv=[0,[0,0,[0,[0,a(af)]]],[0,[2,[0,a(dO)]]]],Mx=[0,[0,0,[0,[0,a(af)]]],[0,[2,[0,a(aw)]]]],Mz=[0,[0,[0,0,[0,[0,a(af)]]],[0,[2,[0,a(bU)]]]],[0,[2,[0,a(aw)]]]],eC=0,eE=[0,[0,eD,function(a){return v[1]}],eC],eG=[0,[0,eF,function(a){return v[1]}],eE];u(m[8],s,eH,0,0,eG);var
eI=0;function
eJ(a,b){return e(v[3],0,a)}var
eL=[0,[0,[0,eK,[1,[5,c(f[16],h[11])],0]],eJ],eI];function
eM(a,b){return e(v[3],0,a)}var
eO=[0,[0,[0,eN,[1,[5,c(f[16],h[11])],0]],eM],eL];u(m[8],s,eP,0,0,eO);function
_(c,b,a){return aS[26]}function
eQ(b,a){return _}function
eR(b,a){return _}var
eS=[0,function(b,a){return _},eR,eQ],eT=[1,[2,[1,h[16]]]],eU=[1,[2,[1,h[16]]]],eV=[1,[2,[1,h[16]]]],eW=c(f[6],h[16]),eX=[0,[2,[1,c(bi[3],eW)]]],eY=0;function
eZ(c,b,a){return 0}var
e1=[0,c(y[10],e0)],e3=[0,[0,[0,[0,0,[0,c(y[10],e2)]],e1],eZ],eY];function
e4(a,c,b){return[0,a]}var
e5=[1,[6,b[15][1]]],e7=[0,[0,[0,[0,0,[0,c(y[10],e6)]],e5],e4],e3],e9=[0,[1,[0,[0,0,function(a){return e8}],e7]],eX,eV,eU,eT,eS],cn=e(m[9],e_,e9),x=cn[1],e$=cn[2];function
z(b,a){function
c(a){var
c=I(fb[9],[0,fa],0,b,a);return function(a,b){return e(c,a,b)}}return e(fc[17],c,a)}function
co(b,a,h,g,f){var
d=e(fd[16],b,a);return c(aS[27],d)}function
cp(a,h,g,f,d){function
b(b){return e(bj[27],a,b[1])}return c(aS[27],b)}function
cq(b,a,h,g,f){var
d=e(bj[24],b,a);return c(aS[27],d)}function
fe(b,a){return function(c,d,e){return cq(b,a,c,d,e)}}function
ff(b,a){return function(c,d,e){return cp(b,a,c,d,e)}}var
fg=[0,function(b,a){return function(c,d,e){return co(b,a,c,d,e)}},ff,fe],fh=[1,[1,h[12]]],fi=[1,[1,h[12]]],fj=[1,[1,h[12]]],fk=c(f[6],h[12]),fl=[0,[1,c(bi[3],fk)]],fm=0;function
fn(a,c,b){return a}var
fp=[0,c(y[10],fo)],fr=[2,[6,fq[7]],fp],ft=[0,[0,[0,[0,0,[0,c(y[10],fs)]],fr],fn],fm],fu=[0,[1,[0,[0,0,function(a){return 0}],ft]],fl,fj,fi,fh,fg],cr=e(m[9],fv,fu),F=cr[1],fw=cr[2],fx=0;function
fy(c,b,a){var
e=z(a,c);return d(T[18],0,e,b)}var
fz=[1,[5,c(f[16],x)],0],fB=[0,[0,[0,fA,[1,[5,c(f[16],F)],fz]],fy],fx];u(m[8],s,fC,0,0,fB);var
fD=0;function
fE(c,b,a){var
e=z(a,c);return d(T[18],fF,e,b)}var
fG=[1,[5,c(f[16],x)],0],fI=[0,[0,[0,fH,[1,[5,c(f[16],F)],fG]],fE],fD];u(m[8],s,fJ,0,0,fI);var
fK=0;function
fL(c,b,a){var
e=z(a,c);return d(T[18],fM,e,b)}var
fN=[1,[5,c(f[16],x)],0],fQ=[0,[0,[0,fP,[0,fO,[1,[5,c(f[16],F)],fN]]],fL],fK];u(m[8],s,fR,0,0,fQ);var
fS=0;function
fT(d,c,b,a){var
e=z(a,c);return I(T[14],0,d,e,b)}var
fU=[1,[5,c(f[16],x)],0],fV=[1,[5,c(f[16],F)],fU],fX=[0,[0,[0,fW,[1,[4,[5,c(f[16],h[6])]],fV]],fT],fS];u(m[8],s,fY,0,0,fX);var
fZ=0;function
f0(d,c,b,a){var
e=z(a,c);return I(T[14],f1,d,e,b)}var
f2=[1,[5,c(f[16],x)],0],f3=[1,[5,c(f[16],F)],f2],f5=[0,[0,[0,f4,[1,[4,[5,c(f[16],h[6])]],f3]],f0],fZ];u(m[8],s,f6,0,0,f5);var
f7=0;function
f8(d,c,b,a){var
e=z(a,c);return I(T[14],f9,d,e,b)}var
f_=[1,[5,c(f[16],x)],0],f$=[1,[5,c(f[16],F)],f_],gc=[0,[0,[0,gb,[0,ga,[1,[4,[5,c(f[16],h[6])]],f$]]],f8],f7];u(m[8],s,gd,0,0,gc);var
ge=0;function
gf(c,b,a){var
d=z(a,c);return e(v[4],d,b)}var
gh=[0,gg,[1,[5,c(f[16],h[6])],0]],gk=[0,[0,[0,gj,[0,gi,[1,[2,[5,c(f[16],h[12])]],gh]]],gf],ge];u(m[8],s,gl,0,0,gk);function
bk(a){return e(v[10],a,0)[2]}var
gm=0;function
gn(f,d,c,b,a){var
g=z(a,c),h=e(v[10],f,d);return I(v[5],0,h,g,b)}var
go=[1,[5,c(f[16],x)],0],gp=[1,[5,c(f[16],F)],go],gq=[1,[4,[5,c(f[16],h[6])]],gp],gs=[0,[0,[0,gr,[1,[4,[5,c(f[16],h[6])]],gq]],gn],gm];u(m[8],s,gt,0,0,gs);var
gu=0;function
gv(e,c,b,a){if(b){var
f=b[1],g=z(a,c),h=bk(e);return I(T[8],0,h,g,f)}var
i=z(a,c),j=bk(e);return d(T[11],0,j,i)}var
gw=[1,[5,c(f[16],x)],0],gx=[1,[5,c(f[16],F)],gw],gA=[0,[0,[0,gz,[0,gy,[1,[4,[5,c(f[16],h[6])]],gx]]],gv],gu];u(m[8],s,gB,0,0,gA);var
gC=0;function
gD(f,d,c,b,a){var
g=z(a,c),h=e(v[10],f,d);return I(v[5],gE,h,g,b)}var
gF=[1,[5,c(f[16],x)],0],gG=[1,[5,c(f[16],F)],gF],gH=[1,[4,[5,c(f[16],h[6])]],gG],gK=[0,[0,[0,gJ,[0,gI,[1,[4,[5,c(f[16],h[6])]],gH]]],gD],gC];u(m[8],s,gL,0,0,gK);var
gM=0;function
gN(f,d,c,b,a){var
g=z(a,c),h=e(v[10],f,d);return I(v[5],gO,h,g,b)}var
gP=[1,[5,c(f[16],x)],0],gQ=[1,[5,c(f[16],F)],gP],gR=[1,[4,[5,c(f[16],h[6])]],gQ],gT=[0,[0,[0,gS,[1,[4,[5,c(f[16],h[6])]],gR]],gN],gM];u(m[8],s,gU,0,0,gT);var
gV=0;function
gW(d,c,b,a){var
f=z(a,c),g=e(v[10],d,0);return I(v[5],0,g,f,b)}var
gX=[1,[5,c(f[16],x)],0],gY=[1,[5,c(f[16],F)],gX],g1=[0,[0,[0,g0,[0,gZ,[1,[4,[5,c(f[16],h[6])]],gY]]],gW],gV];u(m[8],s,g2,0,0,g1);var
g3=0;function
g4(b,a,c){return e(v[8],b,a)}var
g5=[1,[5,c(f[16],h[14])],0],g7=[0,[0,[0,g6,[1,[5,c(f[16],x)],g5]],g4],g3];u(m[8],s,g8,0,0,g7);var
g9=0;function
g_(a,d){var
b=0,c=a?[0,g$,a[1]]:ha;return e(v[9],c,b)}var
hc=[0,[0,[0,hb,[1,[5,c(f[16],x)],0]],g_],g9];function
hd(a,b,f){var
c=[0,[0,b,0]],d=a?[0,he,a[1]]:hf;return e(v[9],d,c)}var
hh=[0,hg,[1,[5,c(f[16],cm)],0]],hj=[0,[0,[0,hi,[1,[5,c(f[16],x)],hh]],hd],hc];u(m[8],s,hk,0,0,hj);var
hl=0;function
hm(g,f,a,p){try{var
o=[0,c(L[15],a)],b=o}catch(a){a=MB(a);if(a!==hn)throw a;var
b=0}if(b){var
h=[0,c(L[14][14],b[1])];return d(bl[eg],h,g,f)}var
i=c(n[3],ho),j=c(n[3],a),k=c(n[3],hp),l=e(n[12],k,j),m=e(n[12],l,i);return e(hq[65][5],0,m)}var
hs=[0,hr,[1,[5,c(f[16],h[16])],0]],ht=[1,[5,c(f[16],h[11])],hs],hv=[0,[0,[0,hu,[1,[5,c(f[16],h[11])],ht]],hm],hl];function
hw(b,a,c){return d(bl[eg],0,b,a)}var
hx=[1,[5,c(f[16],h[11])],0],hz=[0,[0,[0,hy,[1,[5,c(f[16],h[11])],hx]],hw],hv];u(m[8],s,hA,0,0,hz);var
hB=0;function
hC(a,b){return e(bl[5],a,2)}var
hE=[0,[0,[0,hD,[1,[5,c(f[16],h[11])],0]],hC],hB];u(m[8],s,hF,0,0,hE);function
cs(d,b,a){return c(L[9],ct[27])}function
bm(d,b,a){return c(L[9],bj[39])}function
cu(a){return L[12]}function
hG(b,a){return bm}function
hH(b,a){return bm}var
hI=[0,function(b,a){return cs},hH,hG],hJ=0,hK=[0,function(b,a){return a}],hL=[0,function(a,b){return[0,a,c(cu(a),b)]}],hM=0,hN=0;function
hO(a,b){return[0,a]}var
hP=[0,[0,[0,0,[1,[6,b[16][7]]]],hO],hN];function
hQ(b,a){return 0}var
hS=[0,[1,[0,[0,[0,0,[0,c(y[10],hR)]],hQ],hP]],hM,hL,hK,hJ,hI],cv=e(m[9],hT,hS),cw=cv[2],hU=cv[1];function
bn(e,d,b,a){return c(L[10],a)}function
cx(d,c,b,a){return e(L[8],ct[27],a)}function
cy(a){return L[13]}function
hV(b,a){return bn}function
hW(b,a){return bn}var
hX=[0,function(b,a){return cx},hW,hV],hY=0,hZ=[0,function(b,a){return a}],h0=[0,function(a,b){return[0,a,c(cy(a),b)]}],h1=0,h2=0;function
h3(d,a,c,b){return a}var
h5=[0,c(y[10],h4)],h7=[0,[0,[0,[0,[0,0,[0,c(y[10],h6)]],0],h5],h3],h2];function
h8(c,a,b){return[1,a]}var
h$=[0,[0,[0,h_,[0,c(y[10],h9)]],h8],h7];function
ia(b,a){return 0}var
ic=[0,[0,[0,0,[0,c(y[10],ib)]],ia],h$];function
id(b,a){return 1}var
ig=[0,[0,[0,0,[0,c(y[10],ie)]],id],ic];function
ih(b,d,a,c){return[3,a,b]}var
ik=[0,[0,[0,[0,ij,[0,c(y[10],ii)]],0],ih],ig],il=[0,[0,[0,0,[6,cw]],function(a,b){return[0,a]}],ik],io=[0,[1,[0,[0,im,function(b,a,c){return[2,a,b]}],il]],h1,h0,hZ,hY,hX],cz=e(m[9],ip,io),cA=cz[1],iq=cz[2];function
ir(b,a){return _}function
is(b,a){return _}var
it=[0,function(b,a){return _},is,ir],iu=[1,[2,[1,h[16]]]],iv=[1,[2,[1,h[16]]]],iw=[1,[2,[1,h[16]]]],ix=c(f[6],h[16]),iy=[0,[2,[1,c(bi[3],ix)]]],iz=0;function
iA(a,c,b){return[0,a]}var
iB=[1,[6,b[15][1]]],iD=[0,[0,[0,[0,0,[0,c(y[10],iC)]],iB],iA],iz],iE=[0,[1,[0,[0,0,function(a){return 0}],iD]],iy,iw,iv,iu,it],cB=e(m[9],iF,iE),cC=cB[1],iG=cB[2],iH=0,iI=0;function
iL(j,a,i,h){var
k=e(ah[1],ah[8],i),b=[2,c(L[13],j)],f=a?a[1]:iK,g=c(iJ[5],k);d(L[22],g,f,b);return h}var
iN=[0,iM,[1,[5,c(f[16],cC)],0]],iR=[0,[0,0,[0,iQ,[0,iP,[0,iO,[1,[5,c(f[16],cA)],iN]]]],iL,iI],iH],iS=0,iT=[0,function(a){return cD[6]}];I(cD[2],iU,iT,iS,iR);var
cE=[0,s,cm,_,x,e$,z,co,cp,cq,F,fw,bk,cs,bm,cu,hU,cw,bn,cx,cy,cA,iq,cC,iG];bT(951,cE,"Jisuanji_plugin.Jisuanji_g_auto");c(cl[9],cF);var
cG=e(o[19],iW,iV),iX=y[1];function
iY(a){return e(iX,0,a)}e(o[18][11],iY,cG);var
bo=c(b[2][1],iZ),bp=c(b[2][1],i0),J=c(b[2][1],i1),aT=c(b[2][1],i2),$=c(b[2][1],i3),ai=c(b[2][1],i4),ax=c(b[2][1],i5),aj=c(b[2][1],i6),bq=c(b[2][1],i7),br=c(b[2][1],i8);function
cH(b){var
a=E.caml_ml_string_length(b),c=E.caml_string_get(b,0)-42|0;if(!(3<c>>>0))switch(c){case
0:return[1,a];case
1:return[2,a];case
2:break;default:return[0,a]}throw[0,i_,i9]}function
bs(a){var
g=E.caml_string_compare(a,i$);if(0<=g)if(0<g)if(B(a,ja))if(B(a,jb))if(B(a,jc)){var
h=B(a,jd);if(h)if(B(a,je))var
b=0,f=0;else
var
i=1,f=1;else
var
i=h,f=1;if(f)return i}else
var
b=1;else
var
b=1;else
var
b=1;else
var
b=1;else
if(B(a,jl))if(B(a,jm)){if(!B(a,jn))return 2;var
b=B(a,jo)?B(a,jp)?0:1:1}else
var
b=1;else
var
b=1;if(b){var
p=c(n[3],ji),q=c(n[3],a),r=c(n[3],jj),s=e(n[12],r,q),t=e(n[12],s,p);return d(U[6],0,jk,t)}var
j=c(n[3],jf),k=c(n[3],a),l=c(n[3],jg),m=e(n[12],l,k),o=e(n[12],m,j);return d(U[6],0,jh,o)}var
cI=c(b[2][1],jq),cJ=c(b[2][1],jr),bt=c(b[2][1],js),cK=c(b[2][1],jt),cL=c(b[2][1],ju),cM=c(b[2][1],jv),ak=c(b[2][1],jw),aa=c(b[2][1],jx),N=c(b[2][1],jy),jz=0,jA=0,jC=[0,[0,[0,jB,[6,N]],function(a,c,b){return[1,0,a]}],jA],jE=[0,[0,[0,jD,[6,N]],function(a,c,b){return[1,0,a]}],jC];function
jF(b,a,d,c){return[2,a,b]}var
jH=[0,[0,[0,[0,jG,[6,b[15][21]]],[6,N]],jF],jE];function
jI(b,a,d,c){return[2,a,b]}var
jK=[0,[0,[0,[0,jJ,[6,b[15][21]]],[6,N]],jI],jH];function
jL(b,a,d,c){return[3,a,b]}var
jN=[0,[0,[0,[0,jM,[6,b[15][10]]],[6,N]],jL],jK];function
jO(b,a,d,c){return[3,a,b]}var
jQ=[0,[0,[0,[0,jP,[6,b[15][10]]],[6,N]],jO],jN],jS=[0,[0,[0,jR,[6,N]],function(a,c,b){return[4,a]}],jQ],jU=[0,[0,[0,jT,[6,N]],function(a,c,b){return[4,a]}],jS],jW=[0,jV,[0,[0,0,0,[0,[0,[0,0,[6,cI]],function(a,b){return[0,a[1],a[2]]}],jU]],jz]];d(b[19],i[2][5],0,jW);var
jX=0,jY=0,jZ=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[3,[6,cJ]]],[6,cM]],function(a,b,h){var
d=a[2],f=a[1],g=c(o[18][59],b);return[0,e(o[18][57],g,f),d]}],jY]],jX]];d(b[19],cI,0,jZ);var
j0=0,j1=0,j4=[0,0,[0,[0,0,0,[0,[0,[0,[0,j3,[6,bt]],j2],function(d,a,c,b){return a}],j1]],j0]];d(b[19],cJ,0,j4);var
j5=0,j6=0,j8=[0,0,[0,[0,0,0,[0,[0,[0,0,[4,[6,cK],j7]],function(a,b){return a}],j6]],j5]];d(b[19],bt,0,j8);var
j9=0,j_=0;function
j$(b,a,d){return[0,c(ka[1][8],a),b]}d(b[19],cK,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,b[16][6]]],[6,cL]],j$],j_]],j9]]);var
kb=0,kc=0;function
kd(a,c,b){return[0,a]}var
kf=[0,[0,[0,ke,[6,b[15][13]]],kd],kc],ki=[0,[0,[0,[0,kh,[6,bt]],kg],function(d,a,c,b){return[1,a]}],kf],kj=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],ki]],kb]];d(b[19],cL,0,kj);var
kk=0,kl=0,ko=[0,[0,[0,kn,[6,ak]],function(a,c,b){return[0,[0,km,a[1]],a[2]]}],kl],kr=[0,[0,[0,kq,[6,ak]],function(a,c,b){return[0,[0,kp,a[1]],a[2]]}],ko],ku=[0,[0,[0,kt,[6,ak]],function(a,c,b){return[0,[0,ks,a[1]],a[2]]}],kr],kx=[0,[0,[0,kw,[6,ak]],function(a,c,b){return[0,[0,kv,a[1]],a[2]]}],ku],ky=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,ak]],function(a,b){return a}],kx]],kk]];d(b[19],cM,0,ky);var
kz=0,kA=0,kC=[0,[0,[0,kB,[6,aa]],function(a,c,b){return[0,[0,ah[22],a[1]],a[2]]}],kA],kE=[0,[0,[0,kD,[6,aa]],function(a,c,b){return[0,[0,ah[22],a[1]],a[2]]}],kC],kG=[0,[0,[0,kF,[6,aa]],function(a,c,b){return[0,[0,ah[23],a[1]],a[2]]}],kE],kI=[0,[0,[0,kH,[6,aa]],function(a,c,b){return[0,[0,ah[23],a[1]],a[2]]}],kG],kJ=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,aa]],function(a,b){return a}],kI]],kz]];d(b[19],ak,0,kJ);var
kK=0,kL=0;function
kM(d,a,c,b){return[0,kN,a]}var
kQ=[0,[0,[0,[0,kP,[6,i[2][1]]],kO],kM],kL];function
kR(d,a,c,b){return[0,kS,a]}var
kV=[0,[0,[0,[0,kU,[6,i[2][2]]],kT],kR],kQ];function
kW(c,a,b){return[0,0,a]}var
kY=[0,[0,[0,[0,0,[6,i[2][1]]],kX],kW],kV];function
kZ(c,a,b){return[0,0,a]}var
k1=[0,[0,[0,[0,0,[6,i[2][2]]],k0],kZ],kY];function
k2(c,a,b){return[0,0,a]}var
k4=[0,[0,[0,[0,0,[6,i[2][3]]],k3],k2],k1];function
k5(c,a,b){return[0,0,a]}var
k7=[0,[0,[0,[0,0,[6,i[2][4]]],k6],k5],k4],k8=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,bp]],function(a,b){return[0,0,a]}],k7]],kK]];d(b[19],aa,0,k8);var
k9=0,k_=0;function
k$(a,b){return[0,0,a]}d(b[19],aa,0,[0,la,[0,[0,0,0,[0,[0,[0,0,[6,i[2][8]]],k$],k_]],k9]]);var
lb=0,lc=0,ld=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,bo]],function(a,b){return c(a,0)}],lc]],lb]];d(b[19],i[2][7],0,ld);var
le=0,lf=0,lh=[0,[0,lg,function(a,b){return[77,cH(a)]}],lf],lk=[0,[0,lj,function(b,a){return li}],lh],lm=[0,0,[0,[0,0,0,[0,[0,ll,function(b,a){return 5}],lk]],le]];d(b[19],bp,0,lm);var
ln=0,lo=0;function
lp(b,a){return e(k[1],[0,a],b)}d(b[19],N,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,i[2][5]]],lp],lo]],ln]]);function
lq(a){var
b=e(ls[4],lr,a);return c(n[22],b)}var
bu=I(cN[1],lv,lu,lt,lq);function
cO(d,c,a){if(a){var
b=a[1][2][1];if(b)if(!b[2])if(!a[2])return e(bu,[0,d],c)}return 0}function
cP(d,c,a){if(a){var
b=a[1][1];if(b)if(!b[2])if(!a[2])return e(bu,[0,d],c)}return 0}function
lw(a){return[0,a]}var
aU=c(k[2],lw),cQ=c(o[1],aU),cR=c(b[2][1],lx),cS=c(b[2][1],ly),cT=c(b[2][1],lz),cU=c(b[2][1],lA),cV=c(b[2][1],lB),cW=c(b[2][1],lC),bv=c(b[2][1],lD),bw=c(b[2][1],lE),cX=c(b[2][1],lF),cY=c(b[2][1],lG),cZ=c(b[2][1],lH),bx=c(b[2][1],lI),by=c(b[2][1],lJ),bz=c(b[2][1],lK),c0=c(b[2][1],lL),c1=c(b[2][1],lM),c2=c(b[2][1],lN),c3=c(b[2][1],lO),ay=c(b[2][1],lP),bA=c(b[2][1],lQ),c4=c(b[2][1],lR),bB=c(b[2][1],lS),aV=c(b[2][1],lT),c5=c(b[2][1],lU),c6=c(b[2][1],lV),bC=c(b[2][1],lW),c7=c(b[2][1],lX),bD=c(b[2][1],lY),aW=c(b[2][1],lZ),bE=c(b[2][1],l0),l1=0,l2=0;function
l3(e,d,g,c,b,a,f){return[11,a,[0,[0,b,[0,c,d]],e]]}var
l4=0;function
l5(c,f,b,a,e,d){return[0,a,[0,b,c]]}var
l9=[0,[0,[0,[0,[0,[0,[0,[0,0,[6,aT]],[6,b[15][6]]],[6,b[16][16]]],l8],[6,b[16][3]]],[3,[8,[0,[0,[1,[1,[1,[1,l7,[6,b[15][6]]],[6,b[16][16]]],l6],[6,b[16][3]]],l5],l4]]]],l3],l2];function
l_(e,d,g,c,b,a,f){return[11,a,[0,[0,b,[0,c,d]],e]]}var
l$=0;function
ma(c,f,b,a,e,d){return[0,a,[0,b,c]]}var
me=[0,[0,[0,[0,[0,[0,[0,[0,0,[6,aT]],[6,b[15][6]]],[6,b[16][16]]],md],[6,b[16][3]]],[3,[8,[0,[0,[1,[1,[1,[1,mc,[6,b[15][6]]],[6,b[16][16]]],mb],[6,b[16][3]]],ma],l$]]]],l_],l9],mf=[0,[0,[0,[0,[0,0,[6,cV]],[6,bv]],[6,bC]],function(c,b,a,d){return[14,a,b,c]}],me],mg=[0,[0,[0,[0,[0,0,[6,cW]],[6,bv]],[6,bC]],function(b,d,a,c){cO(c,a[1],b);return[14,a[2],d,b]}],mf];function
mh(d,b,a,e){return[10,a,c(cQ,b),d]}var
mi=[0,[0,[0,[0,[0,0,[6,cU]],[6,b[15][6]]],[6,$]],mh],mg];function
mj(b,a,e,d){return[10,mk,[0,c(aU,a),0],b]}var
mm=[0,[0,[0,[0,ml,[6,b[15][4]]],[6,$]],mj],mi];function
mn(b,a,e,d){return[10,mo,[0,c(aU,a),0],b]}var
mq=[0,[0,[0,[0,mp,[6,b[15][4]]],[6,$]],mn],mm],ms=[0,[0,[0,[0,[0,[0,0,[5,[6,cY]]],[6,cZ]],[6,cX]],[2,[6,c1],mr]],function(d,a,c,b,i){var
f=a[1];function
g(b){var
a=b[1];return[0,[0,a[1],a[2],a[3],f,a[4]],b[2]]}var
h=e(o[18][68],g,d);return[15,b,c,a[2],h]}],mq];function
mt(a,c,b){return[16,1,a]}var
mw=[0,[0,[0,mv,[2,[6,i[2][6]],mu]],mt],ms];function
mx(a,c,b){return[16,1,a]}var
mA=[0,[0,[0,mz,[2,[6,i[2][6]],my]],mx],mw];function
mB(a,d,c,b){return[16,0,a]}var
mE=[0,[0,[0,mD,[2,[6,i[2][6]],mC]],mB],mA];function
mF(a,d,c,b){return[16,0,a]}var
mI=[0,[0,[0,mH,[2,[6,i[2][6]],mG]],mF],mE],mL=[0,[0,[0,mK,[2,[6,ay],mJ]],function(a,c,b){return[17,1,a]}],mI],mO=[0,[0,[0,mN,[2,[6,ay],mM]],function(a,c,b){return[17,1,a]}],mL],mR=[0,[0,[0,mQ,[2,[6,ay],mP]],function(a,d,c,b){return[17,0,a]}],mO],mU=[0,[0,[0,mT,[2,[6,ay],mS]],function(a,d,c,b){return[17,0,a]}],mR],mX=[0,[0,[0,mW,[2,[6,c4],mV]],function(a,c,b){return[18,a]}],mU];function
mY(b,f,a,e,d,c){return[19,a,b]}var
m2=[0,[0,[0,[0,[0,m1,[6,b[15][4]]],m0],[2,[6,b[15][4]],mZ]],mY],mX];function
m3(b,e,a,d,c){return[70,a,[0,b]]}var
m6=[0,[0,[0,[0,[0,m5,[6,b[16][7]]],m4],[6,b[15][16]]],m3],m2];function
m7(a,d,c,b){return[70,a,0]}var
m9=[0,[0,[0,m8,[6,b[16][7]]],m7],m6];function
m_(c,f,b,a,e,d){return[71,a,c,b]}var
m$=[6,cR],nb=0;function
nc(a,c,b){return a}var
nf=[0,[0,[0,[0,[0,[0,ne,[6,b[15][4]]],[5,[8,[0,[0,[1,nd,[6,b[16][3]]],nc],nb]]]],na],m$],m_],m9];function
ng(a,c,b){return[20,a]}var
ni=[0,[0,[0,nh,[1,[6,b[15][4]]]],ng],nf];function
nj(a,c,b){return[20,a]}var
nl=[0,[0,[0,nk,[1,[6,b[15][4]]]],nj],ni],no=[0,0,[0,[0,0,0,[0,[0,[0,nn,[2,[6,bw],nm]],function(a,c,b){return[21,a]}],nl]],l1]];d(b[19],i[2][1],0,no);var
np=0,nq=0,nr=[0,[0,[0,0,[6,cT]],function(a,b){return[0,a]}],nq],ns=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,cS]],function(a,b){return[1,a]}],nr]],np]];d(b[19],cR,0,ns);var
nt=0,nu=0,nw=[0,0,[0,[0,0,0,[0,[0,nv,function(b,a){return 0}],nu]],nt]];d(b[19],cS,0,nw);var
nx=0,ny=0,nA=[0,[0,nz,function(b,a){return 0}],ny],nC=[0,[0,nB,function(b,a){return 1}],nA],nE=[0,[0,nD,function(b,a){return 2}],nC],nG=[0,[0,nF,function(b,a){return 3}],nE],nI=[0,[0,nH,function(b,a){return 4}],nG],nK=[0,[0,nJ,function(b,a){return 5}],nI],nM=[0,[0,nL,function(b,a){return 6}],nK],nO=[0,[0,nN,function(b,a){return 7}],nM],nQ=[0,[0,nP,function(b,a){return 8}],nO],nS=[0,[0,nR,function(b,a){return 9}],nQ],nU=[0,[0,nT,function(b,a){return 10}],nS],nW=[0,[0,nV,function(b,a){return 11}],nU],nY=[0,[0,nX,function(b,a){return 12}],nW],n0=[0,[0,nZ,function(b,a){return 13}],nY],n2=[0,[0,n1,function(b,a){return 14}],n0],n4=[0,[0,n3,function(b,a){return 15}],n2],n6=[0,[0,n5,function(b,a){return 16}],n4],n8=[0,[0,n7,function(b,a){return 17}],n6],n_=[0,[0,n9,function(b,a){return 18}],n8],oa=[0,[0,n$,function(b,a){return 19}],n_],oc=[0,[0,ob,function(b,a){return 20}],oa],oe=[0,[0,od,function(b,a){return 21}],oc],og=[0,[0,of,function(b,a){return 22}],oe],oi=[0,0,[0,[0,0,0,[0,[0,oh,function(b,a){return 23}],og]],nx]];d(b[19],cT,0,oi);var
oj=0,ok=0,om=[0,[0,ol,function(b,a){return 0}],ok],oo=[0,[0,on,function(b,a){return 1}],om],oq=[0,[0,op,function(b,a){return 1}],oo],os=[0,[0,or,function(b,a){return 2}],oq],ou=[0,[0,ot,function(b,a){return 3}],os],ow=[0,[0,ov,function(b,a){return 6}],ou],oy=[0,[0,ox,function(b,a){return 5}],ow],oA=[0,0,[0,[0,0,0,[0,[0,oz,function(b,a){return 4}],oy]],oj]];d(b[19],aT,0,oA);var
oB=0,oC=0,oF=[0,[0,oE,function(b,a){return oD}],oC],oI=[0,[0,oH,function(b,a){return oG}],oF],oL=[0,[0,oK,function(b,a){return oJ}],oI],oO=[0,0,[0,[0,0,0,[0,[0,oN,function(b,a){return oM}],oL]],oB]];d(b[19],cU,0,oO);var
oP=0,oQ=0,oT=[0,[0,oS,function(b,a){return oR}],oQ],oW=[0,[0,oV,function(b,a){return oU}],oT],oZ=[0,[0,oY,function(b,a){return oX}],oW],o2=[0,[0,o1,function(b,a){return o0}],oZ],o5=[0,[0,o4,function(b,a){return o3}],o2],o8=[0,[0,o7,function(b,a){return o6}],o5],o$=[0,0,[0,[0,0,0,[0,[0,o_,function(b,a){return o9}],o8]],oP]];d(b[19],cV,0,o$);var
pa=0,pb=0,pe=[0,[0,pd,function(b,a){return pc}],pb],ph=[0,[0,pg,function(b,a){return pf}],pe],pk=[0,[0,pj,function(b,a){return pi}],ph],pn=[0,[0,pm,function(b,a){return pl}],pk],pq=[0,0,[0,[0,0,0,[0,[0,pp,function(b,a){return po}],pn]],pa]];d(b[19],cW,0,pq);var
pr=0,ps=0;function
pt(e,a,d,c,b){return[0,a]}var
pw=[0,[0,[0,[0,pv,[6,b[15][10]]],pu],pt],ps];function
px(e,a,d,c,b){return[0,a]}var
pA=[0,[0,[0,[0,pz,[6,b[15][10]]],py],px],pw],pC=[0,[0,pB,function(b,a){return 1}],pA],pE=[0,[0,pD,function(b,a){return 1}],pC],pF=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],pE]],pr]];d(b[19],bv,0,pF);var
pG=0,pH=0;function
pI(c,b,a,d){return[0,a,b,c]}var
pJ=[6,b[16][8]],pK=0,pM=[0,[0,pL,function(b,a){return 0}],pK],pO=[0,[0,pN,function(b,a){return 2}],pM],pQ=[8,[0,[0,pP,function(b,a){return 1}],pO]];d(b[19],bw,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,b[16][8]]],pQ],pJ],pI],pH]],pG]]);var
pR=0,pS=0;function
pT(a,c,b,e,d){return[0,b,c,a[1],a[2]]}var
pU=0;function
pV(e,b,a,d,c){return[0,a,b]}var
pX=0,pZ=[0,[0,pY,function(b,a){return 1}],pX],p2=[0,[0,[1,[1,[1,p1,[4,[6,bw],p0]],[8,[0,[0,0,function(a){return 0}],pZ]]],pW],pV],pU];function
p3(a,b){return[0,0,a]}var
p4=0,p6=[0,[0,p5,function(b,a){return 1}],p4],p8=[8,[0,[0,[1,0,[8,[0,[0,p7,function(b,a){return 0}],p6]]],p3],p2]],p9=0,p$=[0,[0,p_,function(b,a){return 1}],p9],qa=[8,[0,[0,0,function(a){return 0}],p$]];d(b[19],b[15][5],0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,qb,[3,[6,b[15][4]]]],qa],p8],pT],pS]],pR]]);var
qc=0,qd=0;function
qe(b,a,c){return[0,a,b]}d(b[19],b[15][6],0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,b[15][4]]],[5,[6,b[15][5]]]],qe],qd]],qc]]);var
qf=0,qg=0,qj=[0,[0,qi,function(b,a){return qh}],qg],qm=[0,[0,ql,function(b,a){return qk}],qj],qp=[0,[0,qo,function(b,a){return qn}],qm],qs=[0,[0,qr,function(b,a){return qq}],qp],qv=[0,[0,qu,function(b,a){return qt}],qs],qy=[0,[0,qx,function(b,a){return qw}],qv],qB=[0,[0,qA,function(b,a){return qz}],qy],qE=[0,0,[0,[0,0,0,[0,[0,qD,function(b,a){return qC}],qB]],qf]];d(b[19],cX,0,qE);var
qF=0,qG=0,qI=[0,[0,qH,function(b,a){return 0}],qG],qK=[0,0,[0,[0,0,0,[0,[0,qJ,function(b,a){return 1}],qI]],qF]];d(b[19],cY,0,qK);var
qL=0,qM=0,qO=[0,[0,qN,function(b,a){return 1}],qM],qP=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],qO]],qL]];d(b[19],cZ,0,qP);var
qQ=0,qR=0;function
qS(c,b,j,a,h){function
i(a){return 2===a[0]?1:0}if(e(o[18][22],i,a))return[1,0,b,d(G[16],[0,h],a,c),0];var
f=c[1];if(16===f[0]){var
g=f[2];if(typeof
g!=="number"&&0===g[0])return[1,a,b,f[1],[0,g[1]]]}return[1,a,b,c,0]}var
qU=[0,[0,[0,[0,[0,[0,0,[6,b[16][16]]],qT],[6,bx]],[6,b[16][3]]],qS],qR];function
qV(f,j,p,c,n,a,b){function
l(a){return 2===a[0]?1:0}if(e(o[18][22],l,a))var
m=e(k[1],[0,b],[16,f,[0,c]]),i=0,h=d(G[16],[0,b],a,m),g=0;else
var
i=a,h=f,g=[0,c];return[1,i,j,h,g]}var
qY=[0,[0,[0,[0,[0,[0,[0,[0,0,[6,b[16][16]]],qX],[6,b[16][3]]],qW],[6,bx]],[6,b[16][3]]],qV],qU];function
qZ(b,d,a,c){return[0,a,b]}d(b[19],$,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,b[16][16]]],q0],[6,b[16][3]]],qZ],qY]],qQ]]);var
q1=0,q2=0;function
q3(d,a,c,b){return[0,a]}var
q6=[0,[0,[0,[0,q5,[6,i[2][10]]],q4],q3],q2];function
q7(d,a,c,b){return[0,a]}var
q_=[0,[0,[0,[0,q9,[6,i[2][10]]],q8],q7],q6],q$=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],q_]],q1]];d(b[19],bx,0,q$);var
ra=0,rb=0;function
rc(c,b,e,a,d){return[0,a,b,c]}var
rd=0,rf=[5,[8,[0,[0,re,function(a,c,b){return a}],rd]]];d(b[19],by,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,0,[6,b[15][22]]],rg],[6,b[16][1]]],rf],rc],rb]],ra]]);var
rh=0,ri=0,rk=[0,[0,rj,function(b,a){return 0}],ri],rm=[0,0,[0,[0,0,0,[0,[0,rl,function(b,a){return 0}],rk]],rh]];d(b[19],bz,0,rm);var
rn=0,ro=0,rq=[0,[0,[0,rp,[2,[6,by],[6,bz]]],function(a,c,b){return a}],ro],rs=[0,[0,[0,rr,[2,[6,by],[6,bz]]],function(a,c,b){return a}],rq],rt=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],rs]],rn]];d(b[19],ai,0,rt);var
ru=0,rv=0,rx=[0,[0,[0,rw,[6,c2]],function(a,c,b){return a}],rv],rz=[0,0,[0,[0,0,0,[0,[0,0,function(a){return ry}],rx]],ru]];d(b[19],c0,0,rz);var
rA=0,rB=0;function
rC(f,e,d,c,b,a,g){return[0,[0,[0,a,b],c,d,e],f]}var
rD=[6,ai],rE=[6,c0],rF=0;function
rG(a,c,b){return a}d(b[19],c1,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,[0,[0,0,[6,c3]],[6,b[15][6]]],[6,b[16][16]]],[5,[8,[0,[0,[1,rH,[6,b[16][3]]],rG],rF]]]],rE],rD],rC],rB]],rA]]);var
rI=0,rJ=0,rM=[0,[0,[0,rL,[2,[6,bE],rK]],function(a,c,b){return[0,a]}],rJ];function
rN(d,f,b,a,e){return[0,[0,c(b,a),d]]}var
rQ=[0,[0,[0,[0,[0,[0,0,[6,b[15][4]]],[6,aW]],rP],[4,[6,bE],rO]],rN],rM];function
rR(b,a,d){return[0,[0,c(b,a),0]]}var
rS=[0,[0,[0,[0,0,[6,b[15][4]]],[6,aW]],rR],rQ];function
rT(e,b,d,a,c){return[1,[0,a],b]}var
rW=[0,[0,[0,[0,[0,[0,0,[6,b[15][4]]],rV],[6,aV]],rU],rT],rS],rZ=[0,[0,[0,[0,rY,[6,aV]],rX],function(d,a,c,b){return[1,0,a]}],rW],r1=[0,0,[0,[0,0,0,[0,[0,0,function(a){return r0}],rZ]],rI]];d(b[19],c2,0,r1);var
r2=0,r3=0,r5=[0,[0,r4,function(b,a){return 1}],r3],r6=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],r5]],r2]];d(b[19],c3,0,r6);var
r7=0,r8=0;function
r9(e,d,c,a,b,f){return[0,[0,b,a[2],a[1],c,d],e]}var
r_=[6,ai],r$=0;function
sa(a,c,b){return a}d(b[19],i[2][6],0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,[0,0,[6,b[15][6]]],[6,b[16][18]]],[6,bA]],[5,[8,[0,[0,[1,sb,[6,b[16][3]]],sa],r$]]]],r_],r9],r8]],r7]]);var
sc=0,sd=0;function
se(e,d,c,b,a,f){return[0,[0,a,b,c,d],e]}var
sf=[6,ai],sg=0;function
sh(a,c,b){return a}d(b[19],ay,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,[0,0,[6,b[15][6]]],[6,b[16][16]]],[6,bA]],[5,[8,[0,[0,[1,si,[6,b[16][3]]],sh],sg]]]],sf],se],sd]],sc]]);var
sj=0,sk=0;function
sl(a,c,b){return a}var
sn=[0,[0,[0,sm,[6,b[16][3]]],sl],sk],sp=[0,0,[0,[0,0,0,[0,[0,0,function(a){return e(k[1],[0,a],so)}],sn]],sj]];d(b[19],bA,0,sp);var
sq=0,sr=0,ss=[0,[0,[0,0,[6,bB]],function(a,b){return[0,0,a]}],sr];function
st(b,d,a,c){return[0,[0,a],b]}d(b[19],c4,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,b[15][4]]],su],[6,bB]],st],ss]],sq]]);var
sv=0,sw=0;function
sx(b,f,a,e,d,c){return[0,1,a,b]}var
sA=[0,[0,[0,[0,[0,sz,[6,b[15][19]]],sy],[6,b[16][10]]],sx],sw];function
sB(b,f,a,e,d,c){return[0,0,a,b]}var
sE=[0,[0,[0,[0,[0,sD,[6,b[15][19]]],sC],[6,b[16][10]]],sB],sA];function
sF(b,f,a,e,d,c){return[1,1,a,b]}var
sI=[0,[0,[0,[0,[0,sH,[6,b[15][19]]],sG],[6,b[16][10]]],sF],sE];function
sJ(b,f,a,e,d,c){return[1,0,a,b]}var
sM=[0,[0,[0,[0,[0,sL,[6,b[15][19]]],sK],[6,b[16][10]]],sJ],sI];function
sN(a,d,c,b){return[2,a]}d(b[19],bB,0,[0,0,[0,[0,0,0,[0,[0,[0,sO,[6,b[15][19]]],sN],sM]],sv]]);var
sP=0,sQ=0;function
sR(c,b,a,d){return[0,[0,a,b],c]}var
sS=[6,ai],sT=0;function
sU(a,c,b){return a}d(b[19],ax,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,c6]],[5,[8,[0,[0,[1,sV,[6,b[15][10]]],sU],sT]]]],sS],sR],sQ]],sP]]);var
sW=0,sX=0,sZ=[0,[0,[0,[0,[0,0,[6,ax]],sY],[6,aV]],function(b,d,a,c){return[0,a,b]}],sX],s1=[0,[0,[0,[0,0,[6,ax]],s0],function(c,a,b){return[0,a,0]}],sZ],s2=[0,[0,[0,0,[6,ax]],function(a,b){return[0,a,0]}],s1],s3=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],s2]],sW]];d(b[19],aV,0,s3);var
s4=0,s5=0;function
s6(f,e,c,b,a){return[0,e,[0,a,d(G[17],[0,b],c,f)]]}var
s7=[0,[0,[0,[0,[0,0,[6,b[16][16]]],[6,aj]],[6,b[16][3]]],s6],s5];function
s8(g,i,f,e,b,a,c){var
h=[0,d(G[17],[0,a],b,f)];return[0,e,[1,c,d(G[16],[0,a],b,g),h]]}var
s_=[0,[0,[0,[0,[0,[0,[0,0,[6,b[16][16]]],[6,aj]],[6,b[16][3]]],s9],[6,b[16][3]]],s8],s7];function
s$(f,i,b,a,e){var
c=f[1];if(16===c[0]){var
g=c[2];if(typeof
g!=="number"){var
h=[0,d(G[17],[0,a],b,g[1])];return[0,0,[1,e,d(G[16],[0,a],b,c[1]),h]]}}return[0,0,[1,e,d(G[16],[0,a],b,f),0]]}d(b[19],c5,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,b[16][16]]],ta],[6,b[16][3]]],s$],s_]],s4]]);var
tb=0,tc=0;function
td(b,a){return[0,0,[0,b,e(k[1],[0,a],te)]]}var
tf=[0,[0,[0,0,[6,b[15][3]]],td],tc];function
tg(b,a,d){return c(b,a)}d(b[19],c6,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,b[15][3]]],[6,c5]],tg],tf]],tb]]);var
th=0,ti=0,tj=[0,[0,[0,0,[1,[6,c7]]],function(a,b){return a}],ti],tk=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,bD]],function(a,b){return[0,a,0]}],tj]],th]];d(b[19],bC,0,tk);var
tl=0,tm=0,tp=[0,0,[0,[0,0,0,[0,[0,[0,[0,to,[6,bD]],tn],function(d,a,c,b){return a}],tm]],tl]];d(b[19],c7,0,tp);var
tq=0,tr=0;function
ts(d,b,a,e){return[0,1-c(K[3],b),[0,a,d]]}d(b[19],bD,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[1,[6,b[15][6]]]],[6,aj]],[6,b[16][3]]],ts],tr]],tq]]);var
tt=0,tu=0;function
tv(b,a,d){return c(b,a)}var
tw=0;function
tx(g,f,e,b,a){var
h=[0,a,d(G[17],[0,e],b,g)];return[0,1-c(K[3],f),h]}var
ty=[0,[0,[1,[1,0,[6,aj]],[6,b[16][3]]],tx],tw],tA=[8,[0,[0,0,function(a,c,b){var
f=e(k[1],[0,a],tz);return[0,0,[0,b,d(G[17],[0,a],c,f)]]}],ty]];d(b[19],aW,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,b[16][16]]],tA],tv],tu]],tt]]);var
tB=0,tC=0;function
tD(b,a,d){return c(b,a)}d(b[19],bE,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,b[15][4]]],[6,aW]],tD],tC]],tB]]);var
tE=0,tF=0,tI=[0,[0,tH,function(b,a){return tG}],tF],tL=[0,[0,tK,function(c,b,a){return tJ}],tI],tO=[0,[0,tN,function(b,a){return tM}],tL],tR=[0,[0,tQ,function(d,c,b,a){return tP}],tO],tU=[0,[0,tT,function(c,b,a){return tS}],tR],tW=[0,0,[0,[0,0,0,[0,[0,tV,function(b,a){return 0}],tU]],tE]];d(b[19],aj,0,tW);function
tX(g){var
d=0;for(;;){var
f=e(o[24],d,g);if(typeof
f==="number")var
a=0;else
switch(f[0]){case
0:var
c=f[1];if(B(c,tZ))if(B(c,t0))if(B(c,t1))if(B(c,t2))var
a=0,b=0;else
var
a=1,b=0;else
var
b=1;else
var
a=1,b=0;else
var
b=1;if(b)return 0;break;case
2:var
a=1;break;default:var
a=0}if(a){var
d=d+1|0;continue}throw tY[1]}}var
aX=e(b[2][4],t3,tX);function
aY(a){if(a){var
b=a[1],c=a[2],e=function(b,a){return[2,b,a]};return d(o[18][16],e,c,b)}return a}function
t4(a){return c(n[22],t5)}var
bF=I(cN[1],t7,t6,0,t4),A=c(b[2][1],t8),aZ=c(b[2][1],t9),a0=c(b[2][1],t_),c8=c(b[2][1],t$),al=c(b[2][1],ua),a1=c(b[2][1],ub),bG=c(b[2][1],uc),a2=c(b[2][1],ud),am=c(b[2][1],ue),bH=c(b[2][1],uf),bI=c(b[2][1],ug),C=c(b[2][1],uh),H=c(b[2][1],ui),a3=c(b[2][1],uj),az=c(b[2][1],uk),aA=c(b[2][1],ul),O=c(b[2][1],um),un=0,uo=0;function
up(e,d,c,b,a,g,f){return[36,a,b,c,d,e]}var
ur=[0,[0,[0,[0,[0,[0,[0,uq,[6,A]],[6,b[15][4]]],[3,[6,H]]],[6,a1]],[6,am]],up],uo];function
us(e,d,c,b,a,h,g,f){return[36,a,b,c,d,e]}var
uu=[0,[0,[0,[0,[0,[0,[0,ut,[6,A]],[6,b[15][4]]],[3,[6,H]]],[6,a1]],[6,am]],us],ur];function
uv(e,d,c,b,a,h,g,f){return[36,a,b,c,d,e]}var
ux=[0,[0,[0,[0,[0,[0,[0,uw,[6,A]],[6,b[15][4]]],[3,[6,H]]],[6,a1]],[6,am]],uv],uu];function
uy(e,d,c,b,a,i,h,g,f){return[36,a,b,c,d,e]}var
uA=[0,[0,[0,[0,[0,[0,[0,uz,[6,A]],[6,b[15][4]]],[3,[6,H]]],[6,bG]],[6,am]],uy],ux];function
uB(e,d,c,b,a,i,h,g,f){return[36,a,b,c,d,e]}var
uD=[0,[0,[0,[0,[0,[0,[0,uC,[6,A]],[6,b[15][4]]],[3,[6,H]]],[6,bG]],[6,am]],uB],uA];function
uE(d,c,b,a,g,f,e){return[37,a,b,c,d]}var
uG=[0,[0,[0,[0,[0,[0,uF,[6,b[15][4]]],[3,[6,H]]],[6,al]],[6,a2]],uE],uD];function
uH(d,c,b,a,f,e){return[37,a,b,c,d]}var
uJ=[0,[0,[0,[0,[0,[0,uI,[6,b[15][4]]],[3,[6,H]]],[6,al]],[6,a2]],uH],uG];function
uK(d,c,b,a,f,e){return[37,a,b,c,d]}var
uM=[0,[0,[0,[0,[0,[0,uL,[6,b[15][4]]],[3,[6,H]]],[6,al]],[6,a2]],uK],uJ];function
uN(d,h,c,b,a,g,f,e){return[35,a,b,c,d]}var
uQ=[0,[0,[0,[0,[0,[0,[0,uP,[6,A]],[6,b[15][4]]],[3,[6,H]]],uO],[6,C]],uN],uM];function
uR(d,i,c,b,a,h,g,f,e){return[35,a,b,c,d]}var
uU=[0,[0,[0,[0,[0,[0,[0,uT,[6,A]],[6,b[15][4]]],[3,[6,H]]],uS],[6,C]],uR],uQ];function
uV(d,i,c,b,a,h,g,f,e){return[35,a,b,c,d]}var
uY=[0,[0,[0,[0,[0,[0,[0,uX,[6,A]],[6,b[15][4]]],[3,[6,H]]],uW],[6,C]],uV],uU];function
uZ(a,c,b){return[22,a]}var
u1=[0,[0,[0,u0,[6,b[15][4]]],uZ],uY];function
u2(a,c,b){return[22,a]}var
u4=[0,[0,[0,u3,[6,b[15][4]]],u2],u1];function
u5(a,c,b){return[22,a]}var
u7=[0,[0,[0,u6,[6,b[15][4]]],u5],u4];function
u8(a,c,b){return[23,a]}var
u_=[0,[0,[0,u9,[6,b[15][4]]],u8],u7];function
u$(a,c,b){return[23,a]}var
vb=[0,[0,[0,va,[6,b[15][4]]],u$],u_];function
vc(b,e,a,d,c){return[29,a,b]}var
vf=[0,[0,[0,[0,[0,ve,[6,b[15][4]]],vd],[6,br]],vc],vb];function
vg(b,a,d,c){return[24,0,a,b]}var
vi=[0,[0,[0,[0,vh,[6,A]],[1,[6,b[16][7]]]],vg],vf];function
vj(b,a,d,c){return[24,0,a,b]}var
vl=[0,[0,[0,[0,vk,[6,A]],[1,[6,b[16][7]]]],vj],vi];function
vm(c,b,f,a,e,d){return[24,[0,a],b,c]}var
vp=[0,[0,[0,[0,[0,[0,vo,[6,b[16][7]]],vn],[6,A]],[1,[6,b[16][7]]]],vm],vl];function
vq(c,b,f,a,e,d){return[24,[0,a],b,c]}var
vt=[0,[0,[0,[0,[0,[0,vs,[6,b[16][7]]],vr],[6,A]],[1,[6,b[16][7]]]],vq],vp];function
vu(a,c,b){return[25,0,a]}var
vw=[0,[0,[0,vv,[1,[6,b[16][7]]]],vu],vt];function
vx(a,c,b){return[25,0,a]}var
vz=[0,[0,[0,vy,[1,[6,b[16][7]]]],vx],vw];function
vA(a,c,b){return[25,1,a]}var
vC=[0,[0,[0,vB,[1,[6,b[16][7]]]],vA],vz];function
vD(a,c,b){return[25,1,a]}var
vF=[0,[0,[0,vE,[1,[6,b[16][7]]]],vD],vC],vH=[0,[0,[0,[0,vG,[6,C]],[3,[6,a0]]],function(b,a,d,c){return[38,[0,a,b]]}],vF],vJ=[0,[0,[0,[0,vI,[6,C]],[3,[6,a0]]],function(b,a,d,c){return[38,[0,a,b]]}],vH],vL=[0,[0,[0,[0,vK,[6,C]],[3,[6,aZ]]],function(c,b,f,d,a){e(bF,[0,a],0);return[38,[0,b,c]]}],vJ],vN=[0,0,[0,[0,0,0,[0,[0,[0,[0,vM,[6,C]],[3,[6,aZ]]],function(c,b,f,d,a){e(bF,[0,a],0);return[38,[0,b,c]]}],vL]],un]];d(b[19],i[2][2],0,vN);var
vO=0,vP=0,vS=[0,[0,vR,function(b,a){return vQ}],vP],vV=[0,[0,vU,function(b,a){return vT}],vS],vY=[0,[0,vX,function(b,a){return vW}],vV],v1=[0,[0,v0,function(b,a){return vZ}],vY],v2=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],v1]],vO]];d(b[19],A,0,v2);var
v3=0,v4=0,v6=[0,0,[0,[0,0,0,[0,[0,[0,v5,[6,C]],function(a,c,b){return a}],v4]],v3]];d(b[19],aZ,0,v6);var
v7=0,v8=0,v_=[0,0,[0,[0,0,0,[0,[0,[0,v9,[6,bI]],function(a,c,b){return a}],v8]],v7]];d(b[19],a0,0,v_);var
v$=0,wa=0,wc=[0,0,[0,[0,0,0,[0,[0,[0,wb,[6,C]],function(a,c,b){return a}],wa]],v$]];d(b[19],c8,0,wc);var
wd=0,we=0,wf=[0,0,[0,[0,0,0,[0,[0,[0,0,[3,[6,c8]]],function(a,b){return a}],we]],wd]];d(b[19],al,0,wf);var
wg=0,wh=0,wj=[0,[0,[0,wi,[6,C]],function(a,c,b){return[0,a]}],wh],wk=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,al]],function(a,b){return[1,a]}],wj]],wg]];d(b[19],a1,0,wk);var
wl=0,wm=0,wn=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,al]],function(a,b){return[1,a]}],wm]],wl]];d(b[19],bG,0,wn);var
wo=0,wp=0,wr=[0,[0,[0,[0,wq,[6,C]],[3,[6,aZ]]],function(b,a,d,c){return[0,a,b]}],wp],ws=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],wr]],wo]];d(b[19],a2,0,ws);var
wt=0,wu=0,ww=[0,[0,[0,[0,wv,[6,bI]],[3,[6,a0]]],function(b,a,d,c){return[0,a,b]}],wu],wx=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],ww]],wt]];d(b[19],am,0,wx);var
wy=0,wz=0;function
wA(g,a,f,e,d,c,b){return[0,a]}var
wD=[0,[0,[0,[0,wC,[6,b[15][10]]],wB],wA],wz],wF=[0,[0,wE,function(e,d,c,b,a){return 0}],wD],wG=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 1}],wF]],wy]];d(b[19],bH,0,wG);var
wH=0,wI=0;function
wJ(a,c,b){return[0,a,0]}var
wL=[0,[0,[0,wK,[6,b[17][1]]],wJ],wI];function
wM(b,a,c){return[0,a,b]}d(b[19],bI,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,b[17][1]]],[6,bH]],wM],wL]],wH]]);var
wN=0,wO=0;function
wP(a,c,b){return[0,a,0]}var
wR=[0,[0,[0,wQ,[6,b[17][2]]],wP],wO];function
wS(b,a,c){return[0,a,b]}d(b[19],C,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,b[17][2]]],[6,bH]],wS],wR]],wN]]);var
wT=0,wU=0;function
wV(g,c,f,b,a,e,d){return[0,a,b,c]}d(b[19],H,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,[0,wY,[6,A]],[1,[6,b[15][4]]]],wX],[6,C]],wW],wV],wU]],wT]]);var
wZ=0,w0=0,w1=[0,[0,[0,0,[6,a3]],function(a,b){return a}],w0];function
w2(c,b,a){return e(k[1],[0,a],[1,b,c])}d(b[19],b[17][1],0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,b[17][1]]],[6,a3]],w2],w1]],wZ]]);var
w3=0,w4=0;function
w5(b,a){return e(k[1],[0,a],[0,b])}var
w6=[0,[0,[0,0,[6,b[15][16]]],w5],w4];function
w7(d,a,c,b){return a}d(b[19],a3,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,w9,[6,b[17][1]]],w8],w7],w6]],w3]]);var
w_=0,w$=0;function
xa(c,f,b,a,e,d){return[1,a,b,c]}var
xd=[0,[0,[0,[0,[0,[0,xc,[6,b[15][17]]],[5,[6,b[15][5]]]],xb],[6,b[16][3]]],xa],w$];function
xe(c,f,b,a,e,d){return[1,a,b,c]}var
xh=[0,[0,[0,[0,[0,[0,xg,[6,b[15][17]]],[5,[6,b[15][5]]]],xf],[6,b[16][3]]],xe],xd];function
xi(b,e,a,d,c){return[0,a,b]}var
xl=[0,[0,[0,[0,[0,xk,[6,b[15][17]]],xj],[6,b[15][16]]],xi],xh];function
xm(b,f,a,e,d,c){return[0,a,b]}var
xp=[0,[0,[0,[0,[0,xo,[6,b[15][17]]],xn],[6,b[15][16]]],xm],xl];function
xq(b,f,a,e,d,c){return[0,a,b]}d(b[19],az,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,xs,[6,b[15][17]]],xr],[6,b[15][16]]],xq],xp]],w_]]);var
xt=0,xu=0;function
xv(b,a){return e(k[1],[0,a],[0,b])}var
xw=[0,[0,[0,0,[6,b[15][16]]],xv],xu];function
xx(d,a,c,b){return a}var
xA=[0,[0,[0,[0,xz,[6,b[17][2]]],xy],xx],xw];function
xB(c,b,a){return e(k[1],[0,a],[1,b,c])}var
xC=[0,[0,[0,[0,0,[6,b[17][2]]],[6,a3]],xB],xA];function
xD(c,d,b,a){return e(k[1],[0,a],[2,b,c])}var
xF=[0,[0,[0,[0,[0,0,[6,b[17][2]]],xE],[6,az]],xD],xC];function
xG(c,d,b,a){return e(k[1],[0,a],[2,b,c])}var
xI=[0,[0,[0,[0,[0,0,[6,b[17][2]]],xH],[6,az]],xG],xF];function
xJ(c,f,b,d,a){return e(k[1],[0,a],[2,c,b])}var
xM=[0,[0,[0,[0,[0,xL,[6,az]],xK],[6,b[17][2]]],xJ],xI];function
xN(c,f,b,d,a){return e(k[1],[0,a],[2,c,b])}d(b[19],b[17][2],0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,xP,[6,az]],xO],[6,b[17][2]]],xN],xM]],xt]]);var
xQ=0,xR=0,xS=[0,[0,[0,[0,0,[6,aX]],[3,[6,aA]]],function(a,c,b){return aY(a)}],xR],xT=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,O]],function(a,b){return a}],xS]],xQ]];d(b[19],br,0,xT);var
xU=0,xV=0;function
xW(a,b){return[0,a]}var
xX=[0,[0,[0,0,[6,b[15][4]]],xW],xV];function
xY(c,a,b){return[4,[0,a]]}var
x0=[0,[0,[0,[0,0,[6,b[15][4]]],xZ],xY],xX],x2=[0,[0,x1,function(b,a){return 1}],x0],x4=[0,[0,x3,function(b,a){return 1}],x2],x7=[0,[0,x6,function(c,b,a){return x5}],x4],x_=[0,0,[0,[0,0,0,[0,[0,x9,function(c,b,a){return x8}],x7]],xU]];d(b[19],aA,0,x_);var
x$=0,ya=0,yb=[0,[0,[0,0,[6,aA]],function(a,b){return a}],ya],ye=[0,[0,[0,[0,[0,yd,[6,aX]],[3,[6,aA]]],yc],function(e,a,d,c,b){return aY(a)}],yb],yi=[0,[0,[0,[0,[0,[0,yh,[6,aX]],[3,[6,aA]]],yg],yf],function(f,e,a,d,c,b){return[4,aY(a)]}],ye],yl=[0,[0,[0,[0,yk,[6,O]],yj],function(d,a,c,b){return a}],yi],yq=[0,[0,yp,0,[0,[0,[0,[0,[0,yo,[6,O]],yn],ym],function(e,d,a,c,b){return[4,a]}],yl]],x$],yr=0,yt=[0,[0,[0,[0,[0,0,[6,O]],ys],[6,O]],function(b,d,a,c){return[3,a,b]}],yr],yw=[0,[0,yv,0,[0,[0,[0,[0,[0,0,[6,O]],yu],[6,O]],function(b,d,a,c){return[2,a,b]}],yt]],yq],yx=0,yA=[0,0,[0,[0,yz,0,[0,[0,[0,yy,[6,O]],function(a,c,b){return[1,a]}],yx]],yw]];d(b[19],O,0,yA);var
bJ=c(b[2][1],yB),aB=c(b[2][1],yC),aC=c(b[2][1],yD),bK=c(b[2][1],yE),bL=c(b[2][1],yF),c9=c(b[2][1],yG),bM=c(b[2][1],yH),c_=c(b[2][1],yI),bN=c(b[2][1],yJ),yK=0,yL=0;function
yM(a,c,b){return[57,[0,c$[3],a]]}var
yO=[0,[0,[0,yN,[1,[6,b[15][19]]]],yM],yL];function
yP(a,c,b){return[57,[0,1,a]]}var
yR=[0,[0,[0,yQ,[1,[6,b[15][19]]]],yP],yO];function
yS(a,c,b){return[58,a]}var
yT=0;function
yU(e,b,d,a,c){return[0,a,b]}var
yY=[0,[0,[0,yX,[1,[8,[0,[0,[1,[1,[1,[1,0,[6,c9]],yW],[1,[6,b[15][19]]]],yV],yU],yT]]]],yS],yR];function
yZ(b,a,l,j,f){if(b){var
d=b[1],g=c(G[22],a),h=d[2],i=d[1];return[10,y0,[0,e(k[1],0,[0,g]),i],h]}return[26,e(k[1],[0,f],[0,a])]}var
y1=0;function
y2(b,a,c){return[0,a,b]}var
y3=[5,[8,[0,[0,[1,[1,0,[5,[6,b[15][5]]]],[6,$]],y2],y1]]],y4=[6,b[16][7]],y5=0,y8=[0,[0,[0,[0,[0,y7,[5,[8,[0,[0,y6,function(b,a){return 0}],y5]]]],y4],y3],yZ],yY];function
y9(b,d,c,a){return[26,e(k[1],[0,a],[1,b])]}var
y_=[6,b[15][18]],y$=0,zc=[0,[0,[0,[0,zb,[5,[8,[0,[0,za,function(b,a){return 0}],y$]]]],y_],y9],y8];function
zd(d,b,a,h,g){var
f=[0,c(G[22],a)];return[10,ze,[0,e(k[1],0,f),b],d]}var
zg=[0,[0,[0,[0,[0,zf,[6,b[16][7]]],[5,[6,b[15][5]]]],[6,$]],zd],zc];function
zh(c,h,b,g,a,f,e,d){return[28,a,b,c]}var
zl=[0,[0,[0,[0,[0,[0,[0,zk,[6,b[15][4]]],zj],[6,J]],zi],[6,J]],zh],zg];function
zm(d,h,c,g,b,f,a){return[27,e(k[1],[0,a],[0,b]),c,d]}var
zq=[0,[0,[0,[0,[0,[0,[0,zp,[6,b[16][7]]],zo],[6,J]],zn],[6,J]],zm],zl];function
zr(d,h,c,g,b,f,a){return[27,e(k[1],[0,a],[1,b]),c,d]}var
zv=[0,[0,[0,[0,[0,[0,[0,zu,[6,b[15][18]]],zt],[6,J]],zs],[6,J]],zr],zq];function
zw(a,d,b){return[32,c(o[18][59],a)]}var
zy=[0,[0,[0,zx,[1,[6,b[16][15]]]],zw],zv];function
zz(e,d,c,b,h,a,g,f){return[30,a[2],[0,a[1],b,c],e,d]}var
zA=0;function
zB(e,a,d,c,b){return[0,[0,1,a]]}var
zE=[0,[0,[1,[1,zD,[6,b[16][20]]],zC],zB],zA];function
zF(a,c,b){return[0,[0,0,a]]}var
zH=[0,[0,[1,zG,[6,b[16][3]]],zF],zE],zI=[8,[0,[0,0,function(a){return 0}],zH]],zJ=[6,i[2][11]],zL=[7,b[16][5],zK],zM=0,zO=[0,[0,zN,function(b,a){return 1}],zM],zR=[0,[0,[0,[0,[0,[0,[0,[0,zQ,[6,bq]],zP],[8,[0,[0,0,function(a){return 0}],zO]]],zL],zJ],zI],zz],zy];function
zS(b,a,e,d,c){return[33,[0,[0,a,b],0]]}var
zU=[0,[0,[0,[0,zT,[6,b[16][7]]],[6,i[2][11]]],zS],zR];function
zV(b,a,h,g,f){var
c=[0,b,0];function
d(a){return[0,a,c]}return[33,e(o[18][68],d,a)]}var
zW=0;function
zX(a,c,b){return a}var
z0=[0,[0,[0,[0,zZ,[1,[6,b[16][7]]]],[5,[8,[0,[0,[1,zY,[6,b[15][10]]],zX],zW]]]],zV],zU];function
z1(a,d,c,b){return[34,a]}var
z3=[0,[0,[0,z2,[6,b[16][7]]],z1],z0];function
z4(a,i,h,g,p,m){var
j=a?c(o[18][59],a[1]):a,b=[0,0];function
f(e,i){var
a=i;for(;;){if(a){var
g=a[1];if(typeof
g==="number"){if(c(K[3],b[1])){b[1]=[0,e];var
a=a[2];continue}var
j=c(n[3],z5);return d(U[6],0,0,j)}var
k=f(e+1|0,a[2]),h=[0,g[2],k]}else
var
h=a;return h}}var
k=f(0,c(o[18][59],h)),l=e(K[23],0,i);return[54,g,k,l,b[1],j]}var
z6=0,z9=[5,[8,[0,[0,[1,z8,[2,[6,bJ],z7]],function(a,c,b){return a}],z6]]],z_=0;function
z$(a,c,b){return a}var
Ab=0,Ad=[5,[8,[0,[0,[1,Ac,[2,[8,[0,[0,[1,0,[3,[6,bL]]],function(a,b){return c(o[18][59],a)}],Ab]],Aa]],z$],z_]]],Af=[0,[0,[0,[0,[0,[0,Ae,[6,b[15][19]]],[3,[6,bK]]],Ad],z9],z4],z3];function
Ag(a,i,h,g,p,m){var
j=a?c(o[18][59],a[1]):a,b=[0,0];function
f(e,i){var
a=i;for(;;){if(a){var
g=a[1];if(typeof
g==="number"){if(c(K[3],b[1])){b[1]=[0,e];var
a=a[2];continue}var
j=c(n[3],Ah);return d(U[6],0,0,j)}var
k=f(e+1|0,a[2]),h=[0,g[2],k]}else
var
h=a;return h}}var
k=f(0,c(o[18][59],h)),l=e(K[23],0,i);return[54,g,k,l,b[1],j]}var
Ai=0,Al=[5,[8,[0,[0,[1,Ak,[2,[6,bJ],Aj]],function(a,c,b){return a}],Ai]]],Am=0;function
An(a,c,b){return a}var
Ap=0,Ar=[5,[8,[0,[0,[1,Aq,[2,[8,[0,[0,[1,0,[3,[6,bL]]],function(a,b){return c(o[18][59],a)}],Ap]],Ao]],An],Am]]],At=[0,[0,[0,[0,[0,[0,As,[6,b[15][19]]],[3,[6,bK]]],Ar],Al],Ag],Af],Av=[0,[0,[0,Au,[6,bM]],function(a,d,c,b){return[55,a]}],At],Ay=[0,[0,[0,Ax,[6,bM]],function(a,d,c,b){cP(b,Aw,a);return[55,a]}],Av];function
Az(a,c,b){return[56,a]}var
AA=0,AD=[0,[0,AC,function(c,b,a){return AB}],AA],AF=[0,[0,AE,function(c,b,a){return 0}],AD];function
AG(a,c,b){return[0,a]}var
AH=[1,[6,b[15][4]]],AI=0,AK=[0,[0,AJ,function(b,a){return 0}],AI],AN=[0,0,[0,[0,0,0,[0,[0,[0,AM,[8,[0,[0,[1,[1,0,[8,[0,[0,AL,function(b,a){return 0}],AK]]],AH],AG],AF]]],Az],Ay]],yK]];d(b[19],i[2][2],0,AN);var
AO=0,AP=0,AS=[0,[0,AR,function(c,b,a){return AQ}],AP],AV=[0,[0,AU,function(c,b,a){return AT}],AS],AY=[0,[0,AX,function(c,b,a){return AW}],AV],A1=[0,[0,A0,function(c,b,a){return AZ}],AY],A4=[0,[0,A3,function(c,b,a){return A2}],A1],A7=[0,[0,A6,function(b,a){return A5}],A4],A_=[0,[0,A9,function(b,a){return A8}],A7],Bb=[0,[0,Ba,function(c,b,a){return A$}],A_],Be=[0,[0,Bd,function(e,d,c,b,a){return Bc}],Bb],Bh=[0,0,[0,[0,0,0,[0,[0,Bg,function(e,d,c,b,a){return Bf}],Be]],AO]];d(b[19],bJ,0,Bh);var
Bi=0,Bj=0,Bl=[0,0,[0,[0,0,0,[0,[0,Bk,function(a,c,b){return a}],Bj]],Bi]];d(b[19],aB,0,Bl);var
Bm=0,Bn=0;function
Bo(f,d,b,a){function
g(b){return e(k[1],[0,a],b)}var
h=e(K[16],g,f),i=1-c(K[3],b);return[0,d[1],i,h]}d(b[19],aC,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,Bp,[6,b[15][3]]],[5,[6,aB]]],Bo],Bn]],Bm]]);var
Bq=0,Br=0,Bs=[0,[0,[0,0,[6,aC]],function(a,b){return[0,[0,bb,[0,a[1],a[2],a[3],2]],0]}],Br],Bv=[0,[0,Bu,function(b,a){return Bt}],Bs],Bz=[0,[0,[0,[0,[0,By,[1,[6,aC]]],Bx],[5,[6,aB]]],function(f,i,a,g,h){function
b(a){var
g=a[3],l=2;if(f)if(g)var
i=c(n[3],Bw),b=d(U[6],0,0,i);else
var
j=function(a){return e(k[1],[0,h],a)},b=e(K[16],j,f);else
var
b=g;return[0,bb,[0,a[1],a[2],b,l]]}return e(o[18][68],b,a)}],Bv],BD=[0,[0,[0,[0,[0,BC,[1,[6,aC]]],BB],[5,[6,aB]]],function(f,i,a,g,h){function
b(a){var
g=a[3],l=0;if(f)if(g)var
i=c(n[3],BA),b=d(U[6],0,0,i);else
var
j=function(a){return e(k[1],[0,h],a)},b=e(K[16],j,f);else
var
b=g;return[0,bb,[0,a[1],a[2],b,l]]}return e(o[18][68],b,a)}],Bz],BH=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,BG,[1,[6,aC]]],BF],[5,[6,aB]]],function(f,i,a,g,h){function
b(a){var
g=a[3],l=1;if(f)if(g)var
i=c(n[3],BE),b=d(U[6],0,0,i);else
var
j=function(a){return e(k[1],[0,h],a)},b=e(K[16],j,f);else
var
b=g;return[0,bb,[0,a[1],a[2],b,l]]}return e(o[18][68],b,a)}],BD]],Bq]];d(b[19],bK,0,BH);var
BI=0,BJ=0;function
BK(a,b){return[0,[0,a[1],2],0]}var
BL=[0,[0,[0,0,[6,b[15][3]]],BK],BJ];function
BM(f,a,d,c){function
b(a){return[0,a[1],0]}return e(o[18][68],b,a)}var
BP=[0,[0,[0,[0,BO,[1,[6,b[15][3]]]],BN],BM],BL];function
BQ(f,a,d,c){function
b(a){return[0,a[1],1]}return e(o[18][68],b,a)}d(b[19],bL,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,BS,[1,[6,b[15][3]]]],BR],BQ],BP]],BI]]);var
BT=0,BU=0,BW=[0,[0,BV,function(b,a){return 0}],BU],BY=[0,[0,BX,function(b,a){return 1}],BW];function
BZ(a,b){return[0,a]}var
B0=[0,[0,[0,0,[6,b[15][12]]],BZ],BY],B2=[0,0,[0,[0,0,0,[0,[0,B1,function(b,a){return c$[3]}],B0]],BT]];d(b[19],c9,0,B2);var
B3=0,B4=0;function
B5(b,a,g){var
c=a[2],d=a[1];function
f(a){return[0,a]}return[0,[0,e(k[2],f,d),c],b]}var
B6=[0,[0,[0,[0,0,[6,b[15][6]]],[6,b[16][16]]],B5],B4],B7=[0,0,[0,[0,0,0,[0,[0,0,function(a){return[0,[0,e(k[1],[0,a],0),0],0]}],B6]],B3]];d(b[19],bq,0,B7);var
B8=0,B9=0;function
B_(b,a,d,c){return[0,a,b]}var
Ca=[0,[0,[0,[0,B$,[5,[6,b[15][10]]]],[5,[6,b[16][12]]]],B_],B9],Cc=[0,0,[0,[0,0,0,[0,[0,0,function(a){return Cb}],Ca]],B8]];d(b[19],i[2][11],0,Cc);var
Cd=0,Ce=0,Cf=[0,[0,[0,0,[1,[6,c_]]],function(a,b){return a}],Ce],Cg=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,bN]],function(a,b){return[0,a,0]}],Cf]],Cd]];d(b[19],bM,0,Cg);var
Ch=0,Ci=0,Cl=[0,0,[0,[0,0,0,[0,[0,[0,[0,Ck,[6,bN]],Cj],function(d,a,c,b){return a}],Ci]],Ch]];d(b[19],c_,0,Cl);var
Cm=0,Cn=0;function
Co(b,d,a,c){return[0,a,b]}d(b[19],bN,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[1,[6,b[15][4]]]],Cp],[6,b[16][3]]],Co],Cn]],Cm]]);var
bO=c(b[2][1],Cq),da=c(b[2][1],Cr),db=c(b[2][1],Cs),aD=c(b[2][1],Ct),ab=c(b[2][1],Cu),D=c(b[2][1],Cv),aE=c(b[2][1],Cw),bP=c(b[2][1],Cx),an=c(b[2][1],Cy),dc=c(b[2][1],Cz),bQ=c(b[2][1],CA),dd=c(b[2][1],CB),ao=c(b[2][1],CC),aF=c(b[2][1],CD),ap=c(b[2][1],CE),CF=0,CG=0,CI=[0,[0,[0,[0,CH,[6,D]],[6,aD]],function(b,a,e,d,c){return[59,1,a,b]}],CG],CK=[0,[0,[0,[0,CJ,[6,D]],[6,aD]],function(b,a,e,d,c){return[59,1,a,b]}],CI],CM=[0,[0,[0,CL,[6,D]],function(a,d,c,b){return[59,1,a,0]}],CK],CO=[0,0,[0,[0,0,0,[0,[0,[0,CN,[6,D]],function(a,d,c,b){return[59,1,a,0]}],CM]],CF]];d(b[19],i[2][2],0,CO);var
CP=0,CQ=0,CS=[0,[0,[0,CR,[3,[6,dc]]],function(a,c,b){return[72,a]}],CQ];function
CT(e,d,c,i,b,a,h,g,f){return[31,b,[0,a,c,d],e]}var
CU=[6,i[2][11]],CW=[7,b[16][5],CV],CX=0,CZ=[0,[0,CY,function(b,a){return 1}],CX],C0=[8,[0,[0,0,function(a){return 0}],CZ]],C3=[0,[0,[0,[0,[0,[0,[0,[0,C2,[6,b[15][6]]],[6,b[16][16]]],C1],C0],CW],CU],CT],CS],C5=[0,[0,C4,function(a,d,c,b){return[3,a]}],C3],C8=[0,[0,C7,function(b,a){return C6}],C5],C$=[0,[0,C_,function(b,a){return C9}],C8];function
Da(a,c,b){return[44,[0,a]]}var
Dc=[0,[0,[0,Db,[6,b[15][21]]],Da],C$];function
Dd(b,a,d,c){return[0,a,b]}var
De=0;function
Df(a,b){return a}var
Dg=[0,[0,[1,0,[6,b[15][21]]],Df],De],Di=[8,[0,[0,Dh,function(a,b){return a}],Dg]],Dj=0,Dl=[0,[0,Dk,function(b,a){return 1}],Dj],Dn=[0,[0,[0,[0,Dm,[8,[0,[0,0,function(a){return 0}],Dl]]],Di],Dd],Dc];function
Do(a,e,d,c,b){return[43,a]}var
Dq=[0,[0,[0,Dp,[1,[6,b[15][21]]]],Do],Dn],Ds=[0,[0,[0,Dr,[6,db]],function(a,c,b){return[69,a]}],Dq];function
Dt(b,a,e,d,c){return[40,0,a,b]}var
Dv=[0,[0,[0,[0,Du,[6,b[15][21]]],[6,aE]],Dt],Ds];function
Dw(b,a,f,e,d,c){return[40,1,a,b]}var
Dy=[0,[0,[0,[0,Dx,[6,b[15][21]]],[6,aE]],Dw],Dv];function
Dz(a,d,c,b){return[41,a]}var
DB=[0,[0,[0,DA,[6,b[15][21]]],Dz],Dy];function
DC(b,e,a,d,c){return[40,0,a,b]}var
DF=[0,[0,[0,[0,[0,DE,[6,b[15][21]]],DD],[6,aE]],DC],DB];function
DG(b,e,a,d,c){return[40,1,a,b]}var
DJ=[0,[0,[0,[0,[0,DI,[6,b[15][21]]],DH],[6,aE]],DG],DF];function
DK(a,c,b){return[41,a]}var
DM=[0,[0,[0,DL,[6,b[15][21]]],DK],DJ];function
DN(a,c,b){return[65,a]}var
DP=[0,[0,[0,DO,[6,b[16][3]]],DN],DM],DR=[0,[0,[0,DQ,[6,bO]],function(a,c,b){return[67,a]}],DP],DT=[0,[0,[0,DS,[6,bO]],function(a,c,b){return[67,a]}],DR];function
DU(b,a,d,c){return[67,[7,a,b]]}var
DW=[0,[0,[0,[0,DV,[6,b[15][19]]],[5,[6,ap]]],DU],DT];function
DX(b,a,d,c){return[67,[7,a,b]]}var
DZ=[0,[0,[0,[0,DY,[6,b[15][19]]],[5,[6,ap]]],DX],DW];function
D0(a,e,d,c,b){return[67,[5,a]]}var
D2=[0,[0,[0,D1,[6,b[16][7]]],D0],DZ];function
D3(a,d,c,b){return[67,[5,a]]}var
D5=[0,[0,[0,D4,[6,b[16][7]]],D3],D2];function
D6(a,d,c,b){return[67,[5,a]]}var
D8=[0,[0,[0,D7,[6,b[16][7]]],D6],D5];function
D9(a,d,c,b){return[67,[4,a]]}var
D$=[0,[0,[0,D_,[6,b[16][7]]],D9],D8];function
Ea(a,e,d,c,b){return[67,[4,a]]}var
Ec=[0,[0,[0,Eb,[6,b[16][7]]],Ea],D$];function
Ed(a,e,d,c,b){return[67,[4,a]]}var
Ef=[0,[0,[0,Ee,[6,b[16][7]]],Ed],Ec];function
Eg(a,d,c,b){return[67,[6,a]]}var
Ei=[0,[0,[0,Eh,[6,b[15][20]]],Eg],Ef];function
Ej(a,c,b){return[67,[1,a]]}var
El=[0,[0,[0,Ek,[6,b[15][10]]],Ej],Ei];function
Em(a,e,d,c,b){return[42,0,a]}var
Eo=[0,[0,[0,En,[6,b[15][21]]],Em],El];function
Ep(a,f,e,d,c,b){return[42,1,a]}var
Er=[0,[0,[0,Eq,[6,b[15][21]]],Ep],Eo],Et=[0,[0,[0,[0,Es,[6,D]],[6,aD]],function(b,a,d,c){return[59,0,a,b]}],Er],Ev=[0,[0,[0,[0,Eu,[6,D]],[6,aD]],function(b,a,d,c){return[59,0,a,b]}],Et],Ex=[0,[0,[0,Ew,[6,D]],function(a,c,b){return[59,0,a,0]}],Ev],Ez=[0,[0,[0,Ey,[6,D]],function(a,c,b){return[59,0,a,0]}],Ex],EB=[0,[0,[0,EA,[6,D]],function(a,d,c,b){return[63,a]}],Ez],ED=[0,[0,[0,EC,[6,D]],function(a,d,c,b){return[63,a]}],EB],EF=[0,[0,[0,EE,[1,[6,ab]]],function(c,b,a,e,d){return[60,[0,a,[0,b,0]],c]}],ED],EH=[0,[0,[0,EG,[1,[6,ab]]],function(b,a,d,c){return[60,[0,a,0],b]}],EF],EK=[0,[0,[0,EJ,[1,[6,ab]]],function(a,e,d,c,b){return[60,EI,a]}],EH],EN=[0,[0,[0,[0,[0,EM,[6,D]],EL],[1,[6,ab]]],function(b,e,a,d,c){return[62,a,b]}],EK],EP=[0,[0,[0,EO,[6,D]],function(a,c,b){return[63,a]}],EN],ER=[0,[0,[0,EQ,[1,[6,ab]]],function(c,b,a,e,d){return[61,[0,a,[0,b,0]],c]}],EP],ET=[0,0,[0,[0,0,0,[0,[0,[0,ES,[1,[6,ab]]],function(b,a,d,c){return[61,[0,a,0],b]}],ER]],CP]];d(b[19],i[2][3],0,ET);var
EU=0,EV=0;function
EW(g,c,f,b,e,d,a){return[64,[0,b],a,c]}var
E0=[0,[0,[0,[0,[0,[0,EZ,[6,i[2][10]]],EY],[6,b[16][3]]],EX],EW],EV];function
E1(g,c,f,b,e,d,a){return[64,[0,b],a,c]}var
E5=[0,[0,[0,[0,[0,[0,E4,[6,i[2][10]]],E3],[6,b[16][3]]],E2],E1],E0];function
E6(e,b,d,c,a){return[64,E7,a,b]}var
E_=[0,[0,[0,[0,E9,[6,b[16][3]]],E8],E6],E5];function
E$(e,b,d,c,a){return[64,Fa,a,b]}var
Fd=[0,[0,[0,[0,Fc,[6,b[16][3]]],Fb],E$],E_];function
Fe(e,b,d,c,a){return[64,0,a,b]}var
Fh=[0,[0,[0,[0,Fg,[6,b[16][3]]],Ff],Fe],Fd];function
Fi(e,b,d,c,a){return[64,0,a,b]}var
Fl=[0,[0,[0,[0,Fk,[6,b[16][3]]],Fj],Fi],Fh];function
Fm(f,c,b,e,d,a){return[67,[15,b,c,a]]}var
Fp=[0,[0,[0,[0,[0,Fo,[6,b[15][19]]],[5,[6,ap]]],Fn],Fm],Fl];function
Fq(f,c,b,e,d,a){return[67,[15,b,c,a]]}var
Ft=[0,[0,[0,[0,[0,Fs,[6,b[15][19]]],[5,[6,ap]]],Fr],Fq],Fp];function
Fu(f,c,b,e,d,a){return[68,[2,b],a,c]}var
Fx=[0,[0,[0,[0,[0,Fw,[6,b[16][12]]],[6,an]],Fv],Fu],Ft];function
Fy(f,c,b,e,d,a){return[68,[0,b],a,c]}var
FB=[0,[0,[0,[0,[0,FA,[6,b[16][12]]],[6,an]],Fz],Fy],Fx];function
FC(f,c,b,e,d,a){return[68,[1,b],a,c]}var
FF=[0,[0,[0,[0,[0,FE,[6,b[16][12]]],[6,an]],FD],FC],FB];function
FG(f,c,b,e,d,a){return[68,[1,b],a,c]}var
FJ=[0,[0,[0,[0,[0,FI,[6,b[16][12]]],[6,an]],FH],FG],FF],FM=[0,[0,[0,[0,[0,FL,[6,ao]],[6,aF]],FK],function(g,a,b,f,e){var
c=a[2],d=a[1];return function(a){return[68,[3,[0,b,d]],a,c]}}],FJ],FP=[0,[0,[0,[0,[0,FO,[6,ao]],[6,aF]],FN],function(g,a,b,f,e){var
c=a[2],d=a[1];return function(a){return[68,[3,[0,b,d]],a,c]}}],FM],FS=[0,[0,[0,[0,[0,FR,[6,ao]],[6,aF]],FQ],function(f,a,c,e,d,b){return[68,[3,[0,c,a[1]]],b,a[2]]}],FP],FW=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,FV,[1,[6,ao]]],FU],[6,an]],FT],function(h,c,g,b,f,e,d,a){return[68,[3,b],a,c]}],FS]],EU]];d(b[19],bo,0,FW);var
FX=0,FY=0;function
FZ(b,a,d,c){return[7,a,b]}var
F1=[0,[0,[0,[0,F0,[6,b[15][19]]],[5,[6,ap]]],FZ],FY],F3=[0,[0,F2,function(b,a){return 1}],F1];function
F4(a,c,b){return[0,a]}var
F6=[0,[0,[0,F5,[6,b[16][7]]],F4],F3],F8=[0,[0,F7,function(a,c,b){return[2,a]}],F6];function
F9(a,c,b){return[3,a]}var
F$=[0,[0,[0,F_,[5,[6,b[15][20]]]],F9],F8],Gc=[0,[0,Gb,function(e,b){var
a=c(n[3],Ga);return d(U[6],0,0,a)}],F$],Ge=[0,[0,Gd,function(b,a){return 2}],Gc],Gg=[0,[0,Gf,function(c,b,a){return 3}],Ge],Gi=[0,[0,Gh,function(c,b,a){return 4}],Gg],Gk=[0,[0,Gj,function(c,b,a){return 5}],Gi],Gm=[0,[0,Gl,function(b,a){return 6}],Gk],Go=[0,[0,Gn,function(b,a){return 7}],Gm],Gq=[0,[0,Gp,function(b,a){return 8}],Go];function
Gr(a,c,b){return[8,a]}var
Gt=[0,[0,[0,Gs,[6,b[15][19]]],Gr],Gq],Gv=[0,[0,Gu,function(b,a){return 9}],Gt],Gx=[0,[0,[0,[0,Gw,[6,J]],[6,J]],function(b,a,e,d,c){return[9,a,b]}],Gv],Gz=[0,[0,Gy,function(c,b,a){return 10}],Gx],GB=[0,[0,GA,function(b,a){return 0}],Gz],GD=[0,[0,GC,function(b,a){return 0}],GB],GF=[0,[0,GE,function(b,a){return 11}],GD];function
GG(a,c,b){return[11,a]}var
GI=[0,[0,[0,GH,[6,b[15][19]]],GG],GF],GK=[0,[0,GJ,function(c,b,a){return 12}],GI],GM=[0,[0,GL,function(a,c,b){return[12,a]}],GK],GO=[0,[0,GN,function(b,a){return 13}],GM],GQ=[0,[0,GP,function(a,c,b){return[13,a]}],GO],GS=[0,[0,GR,function(a,c,b){return[14,a]}],GQ];function
GT(a,c,b){return[16,a]}var
GV=[0,[0,[0,GU,[6,b[15][19]]],GT],GS];function
GW(c,b,e,a,d){return[10,a,b,c]}var
GX=[5,[6,b[15][21]]],GY=[5,[6,da]],G0=0,G2=[0,[0,G1,function(b,a){return 1}],G0],G3=[0,[0,[0,[0,[0,[0,0,[8,[0,[0,0,function(a){return 0}],G2]]],GZ],GY],GX],GW],GV];function
G4(a,c,b){return[17,0,0,a]}var
G6=[0,[0,[0,G5,[6,b[15][19]]],G4],G3];function
G7(a,d,c,b){return[17,1,0,a]}var
G9=[0,[0,[0,G8,[6,b[15][19]]],G7],G6];function
G_(a,d,c,b){return[17,0,1,a]}var
Ha=[0,[0,[0,G$,[6,b[15][19]]],G_],G9];function
Hb(a,d,c,b){return[17,1,1,a]}var
Hd=[0,[0,[0,Hc,[6,b[15][19]]],Hb],Ha];function
He(a,c,b){return[18,[0,a]]}var
Hg=[0,[0,[0,Hf,[6,b[15][19]]],He],Hd],Hj=[0,[0,Hi,function(b,a){return Hh}],Hg],Hl=[0,0,[0,[0,0,0,[0,[0,Hk,function(b,a){return 14}],Hj]],FX]];d(b[19],bO,0,Hl);var
Hm=0,Hn=0;function
Ho(e,a,d,c,b){return a}d(b[19],da,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,Hq,[3,[6,b[15][15]]]],Hp],Ho],Hn]],Hm]]);var
Hr=0,Hs=0,Hu=[0,[0,Ht,function(b,a){return 0}],Hs],Hw=[0,[0,Hv,function(b,a){return 1}],Hu];function
Hx(a,b){return[0,a]}d(b[19],J,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,b[15][19]]],Hx],Hw]],Hr]]);var
Hy=0,Hz=0;function
HA(a,b){return[0,a]}var
HB=[0,[0,[0,0,[6,b[15][19]]],HA],Hz];function
HC(a,c,b){return[1,a]}var
HE=[0,[0,[0,HD,[6,b[15][19]]],HC],HB];function
HF(a,c,b){return[5,a]}var
HH=[0,[0,[0,HG,[6,b[15][21]]],HF],HE];function
HI(a,c,b){return[2,a]}var
HK=[0,[0,[0,HJ,[6,b[16][7]]],HI],HH];function
HL(a,c,b){return[3,a]}d(b[19],db,0,[0,0,[0,[0,0,0,[0,[0,[0,HM,[6,b[16][7]]],HL],HK]],Hy]]);var
HN=0,HO=0,HP=[0,[0,0,function(a){return 1}],HO];function
HQ(a,b){return[0,a]}var
HR=[0,[0,[0,0,[6,b[15][12]]],HQ],HP],HT=[0,0,[0,[0,0,0,[0,[0,HS,function(a,b){return[1,a]}],HR]],HN]];d(b[19],aD,0,HT);var
HU=0,HV=0;function
HW(a,b){return[1,a]}var
HX=[0,[0,[0,0,[6,b[16][7]]],HW],HV],HZ=[0,0,[0,[0,0,0,[0,[0,HY,function(a,b){return[0,a]}],HX]],HU]];d(b[19],ab,0,HZ);var
H0=0,H1=0;function
H2(a,b){return a}var
H3=0,H5=[0,[0,[0,0,[1,[8,[0,[0,H4,function(a,b){return a}],H3]]]],H2],H1],H8=[0,[0,H7,function(c,b,a){return H6}],H5],H$=[0,0,[0,[0,0,0,[0,[0,H_,function(c,b,a){return H9}],H8]],H0]];d(b[19],D,0,H$);var
Ia=0,Ib=0;function
Ic(a,b){return a}var
Id=0;function
Ie(a,c,b){return a}d(b[19],aE,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[5,[8,[0,[0,[1,If,[6,b[15][20]]],Ie],Id]]]],Ic],Ib]],Ia]]);var
Ig=0,Ih=0;function
Ii(a,c,b){return[0,a]}var
Ik=[0,[0,[0,Ij,[1,[6,b[16][7]]]],Ii],Ih];function
Il(a,c,b){return[1,a]}d(b[19],bP,0,[0,0,[0,[0,0,0,[0,[0,[0,Im,[1,[6,b[16][7]]]],Il],Ik]],Ig]]);var
In=0,Io=0,Ip=[0,[0,[0,0,[6,bP]],function(a,b){return a}],Io],Ir=[0,0,[0,[0,0,0,[0,[0,0,function(a){return Iq}],Ip]],In]];d(b[19],an,0,Ir);var
Is=0,It=0;function
Iu(a,b){return[0,a]}var
Iv=[0,[0,[0,0,[6,b[16][1]]],Iu],It],Ix=[0,[0,Iw,function(a,b){return[1,a]}],Iv];function
Iy(a,b){return[2,a]}d(b[19],dc,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,b[15][10]]],Iy],Ix]],Is]]);var
Iz=0,IA=0,IC=[0,[0,IB,function(b,a){return 0}],IA],ID=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 1}],IC]],Iz]];d(b[19],bQ,0,ID);var
IE=0,IF=0,IH=[0,0,[0,[0,0,0,[0,[0,IG,function(a,c,b){return a}],IF]],IE]];d(b[19],dd,0,IH);var
II=0,IJ=0;function
IK(c,b,a,d){return[0,a,[1,b,c]]}var
IL=[0,[0,[0,[0,[0,0,[6,bQ]],[6,b[15][21]]],[5,[6,dd]]],IK],IJ];function
IM(b,a,c){return[0,a,[0,b]]}d(b[19],ao,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,bQ]],[6,b[16][12]]],IM],IL]],II]]);var
IN=0,IO=0,IP=[0,[0,[0,0,[6,bP]],function(a,b){return[0,0,a]}],IO],IQ=[0,[0,[0,[0,0,[6,ao]],[6,aF]],function(a,b,c){return[0,[0,b,a[1]],a[2]]}],IP],IS=[0,0,[0,[0,0,0,[0,[0,0,function(a){return IR}],IQ]],IN]];d(b[19],aF,0,IS);var
IT=0,IU=0;function
IV(d,a,c,b){return a}d(b[19],ap,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,IX,[3,[6,b[15][3]]]],IW],IV],IU]],IT]]);var
IY=0,IZ=0,I1=[0,[0,I0,function(a,d,c,b){return[45,a]}],IZ];function
I2(a,d,c,b){return[45,a]}var
I4=[0,[0,[0,I3,[6,b[15][21]]],I2],I1],I6=[0,[0,I5,function(a,d,c,b){return[46,a]}],I4];function
I7(a,d,c,b){return[46,a]}var
I9=[0,[0,[0,I8,[6,b[15][21]]],I7],I6],I$=[0,[0,I_,function(c,b,a){return 0}],I9];function
Ja(a,c,b){return[47,a]}var
Jc=[0,[0,[0,Jb,[6,b[15][4]]],Ja],I$],Jf=[0,[0,Je,function(b,a){return Jd}],Jc];function
Jg(a,c,b){return[48,a]}var
Ji=[0,[0,[0,Jh,[6,b[15][10]]],Jg],Jf];function
Jj(a,c,b){return[49,a]}var
Jl=[0,[0,[0,Jk,[6,b[15][10]]],Jj],Ji],Jo=[0,[0,Jn,function(c,b,a){return Jm}],Jl],Jr=[0,[0,Jq,function(c,b,a){return Jp}],Jo];function
Js(b,f,a,e,d,c){return[66,a,b]}var
Ju=[0,[0,[0,Jt,[6,i[2][10]]],Js],Jr],Jw=[0,0,[0,[0,0,0,[0,[0,Jv,function(a,e,d,c,b){return[9,a]}],Ju]],IY]];d(b[19],i[2][3],0,Jw);var
bR=c(b[2][1],Jx),aG=c(b[2][1],Jy),aq=c(b[2][1],Jz),de=c(b[2][1],JA),bS=c(b[2][1],JB),aH=c(b[2][1],JC),JD=0,JE=0,JG=[0,[0,JF,function(a,d,c,b){return[2,1,a]}],JE],JI=[0,[0,JH,function(a,d,c,b){return[2,0,a]}],JG],JK=[0,[0,JJ,function(b,f,a,e,d,c){return[4,a,[0,b]]}],JI],JM=[0,[0,JL,function(a,d,c,b){return[4,a,0]}],JK],JO=[0,[0,[0,JN,[1,[6,J]]],function(b,f,a,e,d,c){return[5,a,b]}],JM];function
JP(d,c,b,g,a,f,e){return[6,[0,a,c],b,d]}var
JQ=0,JS=[5,[8,[0,[0,JR,function(a,c,b){return a}],JQ]]],JT=0,JX=[0,[0,[1,[1,JW,[2,[6,aq],JV]],JU],function(d,a,c,b){return a}],JT],JY=[8,[0,[0,0,function(a){return 0}],JX]],J1=[0,[0,[0,[0,[0,[0,[0,J0,[6,b[15][22]]],JZ],[6,b[16][1]]],JY],JS],JP],JO];function
J2(d,c,g,b,a,f,e){return[53,a,[0,b,c],d]}var
J5=[0,[0,[0,[0,[0,[0,[0,J4,[6,b[15][4]]],[3,[6,b[16][6]]]],J3],[6,b[16][1]]],[6,bR]],J2],J1];function
J6(d,c,g,b,a,f,e){return[53,a,[0,b,c],d]}var
J9=[0,[0,[0,[0,[0,[0,[0,J8,[6,b[15][4]]],[3,[6,b[16][6]]]],J7],[6,b[16][1]]],[6,bR]],J6],J5];function
J_(d,c,b,g,a,f,e){return[7,b,[0,a,c],d]}var
J$=0,Kb=[5,[8,[0,[0,Ka,function(a,c,b){return a}],J$]]],Kc=0,Kg=[0,[0,[1,[1,Kf,[2,[6,aq],Ke]],Kd],function(d,a,c,b){return a}],Kc],Kh=[8,[0,[0,0,function(a){return 0}],Kg]],Kk=[0,[0,[0,[0,[0,[0,[0,Kj,[6,b[15][14]]],Ki],[6,b[16][1]]],Kh],Kb],J_],J9];function
Kl(d,c,b,g,a,f,e){return[7,b,[0,a,c],d]}var
Km=0,Ko=[5,[8,[0,[0,Kn,function(a,c,b){return a}],Km]]],Kp=0,Kt=[0,[0,[1,[1,Ks,[2,[6,aq],Kr]],Kq],function(d,a,c,b){return a}],Kp],Ku=[8,[0,[0,0,function(a){return 0}],Kt]],Kx=[0,[0,[0,[0,[0,[0,[0,Kw,[6,b[15][14]]],Kv],[6,b[16][1]]],Ku],Ko],Kl],Kk],Kz=[0,[0,Ky,function(c,b,a,f,e,d){return[8,a,b,c]}],Kx];function
KA(b,a,g,f,d){function
c(a){var
b=e(df[17],a,KB);return e(df[17],KC,b)}return[1,1,[0,e(k[2],c,a),b]]}var
KD=0,KH=[0,[0,[1,[1,KG,[2,[6,aq],KF]],KE],function(d,a,c,b){return a}],KD],KI=[8,[0,[0,0,function(a){return 0}],KH]],KK=[0,[0,[0,[0,KJ,[6,b[15][22]]],KI],KA],Kz];function
KL(b,a,e,d,c){return[1,0,[0,a,b]]}var
KM=0,KQ=[0,[0,[1,[1,KP,[2,[6,aq],KO]],KN],function(d,a,c,b){return a}],KM],KR=[8,[0,[0,0,function(a){return 0}],KQ]];d(b[19],i[2][4],0,[0,0,[0,[0,0,0,[0,[0,[0,[0,KS,[6,b[15][22]]],KR],KL],KK]],JD]]);var
KT=0,KU=0,KX=[0,[0,KW,function(e,d,c,b,a){return KV}],KU],KZ=[0,[0,KY,function(e,a,d,c,b){return[0,bs(a)]}],KX],K0=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],KZ]],KT]];d(b[19],bR,0,K0);var
K1=0,K2=0;function
K3(a,c,b){return[0,a]}var
K5=[0,[0,[0,K4,[6,b[15][10]]],K3],K2],K7=[0,0,[0,[0,0,0,[0,[0,K6,function(c,b,a){return 0}],K5]],K1]];d(b[19],aG,0,K7);var
K8=0,K9=0;function
K_(a,d,c,b){return[1,a]}var
La=[0,[0,[0,K$,[6,b[15][10]]],K_],K9],Lc=[0,[0,Lb,function(a,d,c,b){return[2,a,0]}],La];function
Ld(b,g,f,a,e,d,c){return[2,a,[0,b]]}var
Lf=[0,[0,[0,Le,[6,b[15][10]]],Ld],Lc],Li=[0,[0,Lh,function(c,b,a){return Lg}],Lf],Ll=[0,[0,Lk,function(c,b,a){return Lj}],Li],Lo=[0,[0,Ln,function(c,b,a){return Lm}],Ll],Lq=[0,[0,Lp,function(c,b,a){return 1}],Lo],Ls=[0,[0,Lr,function(c,b,a){return 0}],Lq],Lu=[0,[0,Lt,function(a,c,b){return[5,bs(a)]}],Ls];function
Lv(b,a,d,c){return b?[6,a[1],b[1]]:[6,Lw,a]}var
Lx=0,Lz=[5,[8,[0,[0,Ly,function(b,a){return e(k[1],[0,a],b)}],Lx]]],LA=0,LD=[0,[0,[0,[0,LC,[8,[0,[0,LB,function(b,a){return e(k[1],[0,a],b)}],LA]]],Lz],Lv],Lu];function
LE(c,f,b,e,a,d){return[0,[0,a,b],0,[0,c]]}var
LF=[6,aG],LI=0,LL=[0,[0,[0,[0,[0,LK,[2,[8,[0,[0,LJ,function(a,b){return a}],LI]],LH]],LG],LF],LE],LD],LN=[0,[0,[0,LM,[6,aG]],function(b,d,a,c){return[0,[0,a,0],0,[0,b]]}],LL],LP=[0,[0,[0,[0,LO,[6,aG]],[6,aH]],function(c,b,e,a,d){return[0,[0,a,0],[0,c],[0,b]]}],LN],LR=[0,[0,[0,LQ,[6,aH]],function(b,a,c){return[0,[0,a,0],[0,b],0]}],LP],LT=[0,0,[0,[0,0,0,[0,[0,[0,LS,[6,de]],function(b,a,c){return[4,a,b]}],LR]],K8]];d(b[19],aq,0,LT);var
LU=0,LV=0,LX=[0,[0,LW,function(b,a){return 0}],LV],LZ=[0,[0,LY,function(b,a){return 1}],LX],L1=[0,[0,L0,function(b,a){return 2}],LZ],L4=[0,[0,L3,function(b,a){return L2}],L1],L7=[0,[0,L6,function(b,a){return L5}],L4],L9=[0,[0,[0,[0,L8,[5,[6,bS]]],[5,[6,aH]]],function(b,a,d,c){return[1,0,b,a]}],L7],Ma=[0,[0,L$,function(b,a){return L_}],L9];function
Mb(a,e,d,c,b){return[2,0,[0,a]]}var
Md=[0,[0,[0,Mc,[6,b[15][10]]],Mb],Ma],Mg=[0,[0,Mf,function(c,b,a){return Me}],Md];function
Mh(a,f,e,d,c,b){return[2,1,[0,a]]}var
Mj=[0,[0,[0,Mi,[6,b[15][10]]],Mh],Mg],Mm=[0,[0,Ml,function(c,b,a){return Mk}],Mj],Mo=[0,0,[0,[0,0,0,[0,[0,[0,[0,Mn,[5,[6,bS]]],[5,[6,aH]]],function(c,b,a,e,d){return[1,[0,a],c,b]}],Mm]],LU]];d(b[19],de,0,Mo);var
Mp=0,Mq=0,Ms=[0,0,[0,[0,0,0,[0,[0,[0,Mr,[6,aG]],function(a,c,b){return a}],Mq]],Mp]];d(b[19],bS,0,Ms);var
Mt=0,Mu=0,Mw=[0,[0,Mv,function(c,b,a){return 0}],Mu],My=[0,[0,Mx,function(c,b,a){return 1}],Mw],MA=[0,0,[0,[0,0,0,[0,[0,Mz,function(d,c,b,a){return 2}],My]],Mt]];d(b[19],aH,0,MA);var
dg=[0,cF,cG,bo,bp,J,aT,$,ai,ax,aj,bq,br,cH,bs,bu,cO,cP,aU,cQ,aX,aY,bF];bT(965,dg,"Jisuanji_plugin.G_jisuanji_vernac");bT(966,[0,cE,dg],"Jisuanji_plugin");return}
