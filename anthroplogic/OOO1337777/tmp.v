(* 

When the ouput of the first function [f] is the same as the input of the subsequent function [g] , it is easy to compose the two functions simply by composing/sequencing their effect .
 *)
Section section_composition.

(*
     f           g
A ------>   B  ----->   C

a |----->  f a |----> g (f a)   

*)  
 
  Variables (A : Type) (B : Type) (f : A -> B ).
  Variables (C : Type) (g : B -> C) .

  Definition composition : A -> C
    := fun a => g ( f a )  .

End section_composition.
(*

How to express that the output of the first function is NOT the same as the input of the subsequent function ? 

Solution : by generalizing from the idea of function to the idea of function with reindexed/restricted/multiplied input : [ B_ <--b-- B --g--> C ] . Now the restriction function [b] should be read backwards , and the effectful function [g] should be read forward .

 *)

Section section_reindexed_function.
(*
     g
 B ------> C
 |
 . b
 |
 v
 B_

 *)
  
  Variables (B_ : Type) (B : Type) (b : B -> B_) (C : Type) (g : B -> C) .

(*
Example :

.  
.   .
.   .
  _
  |
  | b
  v
 
. . .

*)

(* 
Therefore any function can be read forward (the effectful orientation) or read backward (the reindexer/multiplier orientation). Here is how to read backward some function .
*)

(*
 The question now is : how to compose/sequence two reindexed functions ( functions with reindexed/restricted/multiplied inputs ) such as (a <o> f) then (b <o> g) ? In other words : when the ouput of the first effectful function [f] may NOT be the same as the input of the subsequent effectful function [g] ? This is more complex than simply sequencing the effects , and shall be described in two steps .
 *)

  Variables (A_ A : Type) (a : A -> A_) (f : A -> B_ ).
  
(*
                   g
             B ------> C
             |
             . b
             |
             v
  A  ------> B_
  |    f
  . a
  |
  v
  A_

 *)

End section_reindexed_function.
  
(* 

When the ouput of the first function is NOT the same as the input of the subsequent function , then the inputs of first function must be restricted/intersected such that the corresponding outputs of the first function fall within the input of the subsequent function .

The first function is "transported" ( "pullback" ) against the input of the subsequent function ; the restricted inputs of this transported first function is named "intersection" .

This transport process can be described on its own now , and the general transporting-composition can be described later .

*)
Section section_intersection.

(*
              transported
intersection  ---------->   Multiples
 |                             |
 .                             . multiplier
 |                             |
 v                             v
 A -----------------------> Domain
               f
*)

  Variables (Domain : Type) (Multiples : Type) (multiplier : Multiples -> Domain) .

  Definition intersection (A : Type) (f : A -> Domain) : Type
    := { ab : ( A * Multiples ) |
         f ( fst ab ) = multiplier ( snd ab ) } . 

  Definition transported (A : Type) (f : A -> Domain) : intersection A f -> Multiples
    := fun ab_eq => snd (proj1_sig ab_eq) .

  Definition multimultiplier (A : Type) (f : A -> Domain) : intersection A f -> A
    := fun ab_eq => fst (proj1_sig ab_eq) .

  Definition multiplicity_of_intersection (A : Type) (f : A -> Domain) : intersection A f -> Domain
    := fun ab_eq => multiplier ( snd (proj1_sig ab_eq) ) .
End section_intersection.

(* 

Memo : this transport has both restriction flavor and multiplication flavor ; nevertheless restriction can be viewed as exclusion via "multiplication by zero" , therefore multiplication is more general than restriction . 

*)

(* 
Example of multiplying the function [3] against the multiplier [2] .
*)


Section section_intersection_example .
(*

   transported
...    |-->    . 
...    |-->    . 
               _
               | multiplier
               v
 
...    |-->    .
        f

 *)
  
  Inductive two : Type := Two1 : two | Two2 : two .
  Inductive three : Type := Three1 : three | Three2 : three | Three3 : three .
  Inductive infinite : Type := Start : infinite | Next : infinite -> infinite .

  Let Domain : Type := infinite.
  Let Multiples : Type := two.
  Let multiplier : Multiples -> Domain := fun m => (Next Start).
  Let A : Type := three.
  Let f : A -> Domain := fun a => (Next Start) .
  Compute ( transported Domain Multiples multiplier A f : {ab : three * two | (Next Start) = (Next Start)} -> two ) .
  Check fun ab_eq : {ab : three * two | (Next Start) = (Next Start)} =>
          let (_, b) := (let (ab, _) := ab_eq
                         in ab)
          in b .
End section_intersection_example.

(* 

Here is the final step of composing two (general) reindexed functions . The transporting-composition makes use of the transport process to restrict/multiply the first effectful function [f] againt the input multiplier [b] of the subsequent function [g] ; then this transporting-composition simply composes their transported first effectful function [transp f] with the subsequent effectful function [g] to obtain [transp f o> g].

 ( Moreover , if the input of the first function was somehow already restricted/multiplied via [a] , then this old restriction/multiplication [a] shall be composed/sequenced after the new restriction/multiplication [mult b] from this transport process ) 

*)

Section section_transporting_composition.
  Variables (A_ A : Type) (a : A -> A_) (B_ : Type) (f : A -> B_ ).
  Variables (B : Type) (b : B -> B_) (C : Type) (g : B -> C) .
  
(*

( a , f ) o> ( b , g )

                  g
             B ------> C
             |
             . b
             |
             v
  A  ------> B_
  |    f
  . a
  |
  v
  A_


 :=  ( a <o mult b , transp f o> g)

     transp f      g
  AB --------> B ------> C
   |  
   . mult b
   | 
   v 
   A 
   | 
   . a
   |
   v
   A_

*)

  Definition transporting_composition : ( intersection B_ B b A f -> A_ ) * ( intersection B_ B b A f -> C )
    := ( composition _ _ (multimultiplier B_ B b A f) _  a  , composition _ _ (transported B_ B b A f ) _  g  ) .
  
End section_transporting_composition.  
(*
The bottom line is : to compute/eliminate/normalize the transporting-composition is rephrasable/reducible as to compute/eliminate/normalize the (common) composition in the presence of transports ...
 *)


(*

Transporting some function against the identity produces the same function .

This technical idea is used to show this example . The effective totality/domain may cause some hidden assumptions , such that the class becomes (unspoken) intersection-of-more-classes :

*)

Section section_intersection_with_identity.
(*
         transp f := f
F /\ W ---------------------> W
   ^                          ^
   |                          |
   . =                        . =
   |                          |
   v                          v
   F  ----------------------> W
                 f
*)  
Lemma intersection_with_identity : forall F W : Prop , forall hidden : ( F -> W ) (* hidden assumption caused by the effective totality/domain *),
      ( ( F /\ W ) <-> F ) (* therefore the class F becomes (unspoken) intersection-of-more-classes *) .
Proof.
  intros F W hidden. split.
  - intros wf. destruct wf as [w f].
    + exact w.
  - intros f. split.
    + exact f .
    + exact (hidden f).
Qed.

End section_intersection_with_identity.


(**

Dictionary. What is the dictionary definition of "format" / "form" ? <a href="https://www.thefreedictionary.com/format">https://www.thefreedictionary.com/format</a> , <a href="https://www.thefreedictionary.com/form">https://www.thefreedictionary.com/form</a> says :
* form - the <b>spatial arrangement</b> of something as distinct from its <b>substance</b> ;
* format - the <b>arrangement of data</b> for computer input or output, as the number of <b>fields</b> in a <b>database record</b> or the margins in a report ;
* form - a category of things distinguished by some <b>common characteristic</b> or quality ;
* format - the organization of information according to <b>preset specifications</b> (usually for computer processing) .

These key words "substance" , "data" , "database record" signify the "totality of the real data in memory" . But sometimes only some (preset) "partial information" ( "characteristic" , "specifications" , "field" / "projection" ) from this total data is "touched" / "indexing" ( for example for "spatial arrangement" ) .

What is the contrary/complement of "format" / "formal" / "grammatical" ? It is : "sense" ( "substance" , "data" ) in any other "possible forms" , and therefore may mention "discretion for the possibility" or "risk for the possibility" .  On the contrary : "format" is similar as "reference form" , "copy-me grading" , "objective" , which refuses to be subjective-under-teaching (of possible forms) .

Another manner in which "format" / "formal" is contrary to "sense" is : any declaration is formally valid when prefixed with "it is possible that ... " ( for example , "maybe you are thief" ) , but the inferred wanted sense is different from the lacking actual sense .

In summary : forced-fool-and-theft/lie/falsification has :
* ( "flip-flop" ) , fool via : confused or by contradicting oneself , or by flip-flop « possibility » ( "discretion" , "risk" ) versus « copy-me grading » ( "format" , "CV" , "objective" , refuse to be subjective-under-teaching ) ,
* ( "play-monopoly" ) , fool via : by defame/accuse ( without personal-knowledge ) in the form of possibility/question-excuse ( " ... ? " , " maybe ... " ) ,

Legislation. What is the legislation definition of "format" ? https://www.canlii.org/en/on/laws/regu/rro-1990-reg-194/latest/rro-1990-reg-194.html#sec37.12.1 , at "Rules of Civil Procedure, RRO 1990, Reg 194 / HEARING WITHOUT ORAL ARGUMENT / Opposed Motions in Writing" , says :
37.12.1 (4) Where the issues of fact and law are not complex, the moving party may propose in the notice of motion that the motion be heard in writing without the attendance of the parties, in which case,
(a) the motion shall be made on at least <b>fourteen days notice</b>;
(b) the moving party shall serve with the notice of motion and immediately file, with proof of service in the court office where the motion is to be heard, <b>a motion record, a draft order and a factum</b> entitled factum for a motion in writing, setting out the moving party’s argument;
(c) the motion may be heard <b>in writing without the attendance of the parties</b>, unless the court orders otherwise.

Why would the "oral format" be contrasted from the "written format" ? The "partial information" which is touched is not same for these two formats ? 

Indeed , in the oral format , the "bad party" will more easily confuse/mix up facts and falsify hidden presuppositions and indirectly suggest diffamatory/accusation possibilities/questions ( "frame" ) ; while the "real party" will more difficultly describe precisely the totality of the true reality , without mentioning/validating diffamatory/accusation possibilities/questions .

In contrast , the written format is always processed in advance ( "fourteen days notice" ) , has detailled description ( "a motion record, a draft order and a factum" ) which is not overwritten (masked/erased) by any confused-and-incomplete oral commentary ( "in writing without the attendance of the parties" ) .


 *)

(**

The ends is to describe how the inputs and outputs of some function may be formatted , and to discover that there are 4 equivalent alternative formatting/descriptions of "formatted-function" . The solution is : to rearrange and pair/separate the inputs or outputs such to produce many formats .

The general reading is that in the diagram below : [ff] is formatted-function which is parametrized-over the reindexed-function [ ( a <o> f ) ] via the parametrizators [ ( a' , b' ) ] . The types at the top ( [A'] and [B'] ) contain the totality of the data or substance or "database records" . The types at the bottom ( [A_] and [B] and [A] ) contain the "partial information" and are "fields" / "projections" / "components" ( via [a'] and [b'] ) from the totality of the data . More precisely , the type [A] reindexes/reparametrizes the type [A_] ...

There are 4 equivalent alternative formatting/descriptions of "formatted-function" . The distinction is : how much the 3 inputs ( of type [A'] or [A] or [A_] ) are paired/grouped together ? The first format [formatted_NOPARAM] pairs/group all the inputs , and the other formats separate/decouple at least one input which becomes the parametrizator . The last format [formatted_INTERNAL_REPARAM] separate/ungroup all the inputs , the format [formatted_EXTERNAL] parametrizes some external function , the format [formatted_INTERNAL] parametrizes some internal function .

*)

Section section_formatted_function.

(**

   ff : formatted
A'----------------> B'
|                   |
. a'                . b'
|                   |
.          f        v
| A --------------> B
. |
| . a
. |
v v
 A_

 *)
  
  Variables (A_ : Type) (A : Type) (a : A -> A_) (B : Type) (f : A -> B) .
  Variables (A' : Type) (a' : A' -> A_) (B' : Type) (b' : B' -> B) .

  Definition formatted_NOPARAM := forall x_x'_ : { x_x' : A * A' | a ( fst x_x' ) = a' ( snd x_x' ) } ,
      { y' : B' | f ( fst (proj1_sig x_x'_) ) = b' y' } .

  Definition formatted_EXTERNAL := forall x : A ,
      { x' : A' | a ( x ) = a' ( x' ) } -> { y' : B' | f ( x ) = b' y' } .
  
  Definition formatted_INTERNAL := forall x' : A' ,
      forall x_ : { x : A | a ( x ) = a' ( x' ) } , { y' : B' | f ( proj1_sig x_ ) = b' y' } .

  Definition formatted_INTERNAL_REPARAM := forall x0 : A_ ,
      { x' : A' | x0 = a' ( x' ) } -> forall x_ : { x : A | a ( x ) = x0 } , { y' : B' | f ( proj1_sig x_ ) = b' y' } .
  
  Lemma formatted_EXTERNAL_of_formatted_NOPARAM : formatted_NOPARAM -> formatted_EXTERNAL .
  Proof.
    unfold formatted_NOPARAM , formatted_EXTERNAL . intros ff . intros x x'_ . unshelve eexists.
    - refine (proj1_sig (ff _)).
      exists ( x , proj1_sig x'_ ).
      simpl. exact (proj2_sig x'_).
    - set ( x_x'_ := exist _ _ _ ). exact (proj2_sig (ff x_x'_)).
  Defined.

  Lemma formatted_INTERNAL_of_formatted_EXTERNAL : formatted_EXTERNAL -> formatted_INTERNAL .
  Proof.
    unfold formatted_EXTERNAL , formatted_INTERNAL . intros ff . intros x' x_ . unshelve eexists.
    - refine (proj1_sig (ff (proj1_sig x_) _)).
      exists x' .
      exact (proj2_sig x_).
    - set ( x'_ := exist _ _ _ ). exact (proj2_sig (ff (proj1_sig x_) x'_)).
  Defined.
  

  Lemma formatted_INTERNAL_REPARAM_of_formatted_INTERNAL : formatted_INTERNAL -> formatted_INTERNAL_REPARAM .
  Proof.
    unfold formatted_INTERNAL_REPARAM , formatted_INTERNAL . intros ff . intros x0 x'_ x_ . unshelve eexists.
    - refine (proj1_sig (ff (proj1_sig x'_) _)).
      exists (proj1_sig x_) .
      rewrite (proj2_sig x_) . rewrite <- (proj2_sig x'_). reflexivity.
    - set ( x__ := exist _ _ _ ). exact (proj2_sig (ff (proj1_sig x'_) x__)).
  Defined.

  Lemma formatted_NOPARAM_of_formatted_INTERNAL_REPARAM : formatted_INTERNAL_REPARAM -> formatted_NOPARAM .
  Proof.
    unfold formatted_INTERNAL_REPARAM , formatted_NOPARAM . intros ff . intros x_x'_ . unshelve eexists.
    - refine (proj1_sig (ff (a (fst (proj1_sig x_x'_))) _ _)).
      exists (snd (proj1_sig x_x'_)) . exact (proj2_sig x_x'_).
      exists (fst (proj1_sig x_x'_)) . reflexivity.
    - set ( x_ := exist _ _ _ ). set ( x'_ := exist _ _ _ ). 
      exact (proj2_sig (ff (a (fst (proj1_sig x_x'_))) x'_ x_)).
  Defined.

  (**NOT NECESSARY
  Lemma formatted_NOPARAM_of_formatted_INTERNAL : formatted_INTERNAL -> formatted_NOPARAM .
  Proof.
    unfold formatted_INTERNAL , formatted_NOPARAM . intros ff . intros x_x'_ . unshelve eexists.
    - refine (proj1_sig (ff (snd (proj1_sig x_x'_)) _)).
      exists (fst (proj1_sig x_x'_)) .
      exact (proj2_sig x_x'_).
    - set ( x_ := exist _ _ _ ). exact (proj2_sig (ff (snd (proj1_sig x_x'_)) x_)).
  Defined.
  
  Lemma formatted_INTERNAL_REPARAM_of_formatted_NOPARAM : formatted_NOPARAM -> formatted_INTERNAL_REPARAM .
  Proof.
    unfold formatted_INTERNAL_REPARAM , formatted_NOPARAM . intros ff . intros x0 x'_ x_ . unshelve eexists.
    - refine (proj1_sig (ff _)).
      exists ((proj1_sig x_) , (proj1_sig x'_)) .
      simpl. rewrite (proj2_sig x_) . rewrite <- (proj2_sig x'_). reflexivity.
    - set ( x_x'_ := exist _ _ _ ). 
      exact (proj2_sig (ff x_x'_)).
  Defined.
  **)


(**

Below , internal function , whose internal input is of type [ { x : A | a ( x ) = x0 } ] ( or simply written [x0] ) , is similar as some tuple/pair/record whose sections/components may be accessed . Given some section , there are many equivalent alternatives for accessing this section : these accessors correspond to the many equivalent alternatives formats/descriptions for formatted-function .
*)  
  Definition internal_functions_at (x0 : A_ ) := forall x_ : { x : A | a ( x ) = x0 } , { y' : B' | f ( proj1_sig x_ ) = b' y' } .
  Check eq_refl _ : formatted_INTERNAL = forall x' : A' , internal_functions_at ( a' ( x' ) )  .
  Check eq_refl _ : formatted_INTERNAL_REPARAM = forall x0 : A_ ,
        { x' : A' | x0 = a' ( x' ) } -> internal_functions_at x0 .

  Section application_at_section.

(**

   ff : formatted
A'----------------> B'
|                   |
. a'                . b'
|                   |
.          f        v
| A --------------> B
. |  ^
| .  |
. |a |
| .  .
. |  |
| .  . section
v v  |
   A_

 *)
    
    Variables (section : A_ -> A) (section_ : forall x0 : A_ , a ( section x0 ) = x0 ).

    Definition application_at_section : forall (x0 : A_ ) , internal_functions_at x0 -> { y' : B' | f ( section x0 ) = b' y' }
      := fun x0 ff => (ff ( exist _ (section x0) (section_ x0) )).
   
    Axiom formatted_INTERNAL_REPARAM_property :
      forall ff : formatted_INTERNAL_REPARAM ,
       forall x0 : A_ , forall x'_ : { x' : A' | x0 = a' ( x' ) } , forall x_ : { x : A | a ( x ) = x0 } ,
             forall x00 : A_ , forall x'__ : { x' : A' | x00 = a' ( x' ) } , forall x__ : { x : A | a ( x ) = x00 } ,
                   x0 = x00 -> proj1_sig x'_ = proj1_sig x'__ -> proj1_sig x_ = proj1_sig x__ ->
                   proj1_sig (ff x0 x'_ x_) = proj1_sig (ff x00 x'__ x__) .

    Lemma compose_with_application_at_section :
      forall ff : formatted_INTERNAL_REPARAM, forall x0 : A_ ,
      forall x'_ : { x' : A' | x0 = a' ( x' ) } ,
        proj1_sig ( composition _ _ (ff x0) _ (application_at_section x0) x'_ )
        = proj1_sig ( (formatted_EXTERNAL_of_formatted_NOPARAM (formatted_NOPARAM_of_formatted_INTERNAL_REPARAM
                        ff )) (section x0) (exist (fun x' => a (section x0) = a' ( x' ))
                                           (proj1_sig x'_) ltac:(rewrite (section_ x0); exact (proj2_sig x'_))) ).
    Proof.
      intros. simpl. unfold composition , application_at_section. simpl.
      set (x_ := exist _ _ _).
      set (x'__ := exist _ _ _). set (x__ := exist _ _ _).
      eapply (formatted_INTERNAL_REPARAM_property ff x0 x'_ x_ (a (section x0)) x'__ x__).
      - rewrite section_. reflexivity.
      - reflexivity.
      - reflexivity.
    Defined.

  End application_at_section.
    
End section_formatted_function.

(**
The ends is to compose/sequence two formatted-functions when the formatted-output of the first formatted-function is not formatted as (included within) the formatted-input of the subsequent formatted-functiion . The solution is : to intersec the formatting of the input of the first formatted-function ( so that its output is formatted ) as the formatting of the input of the subsequent formatted-function .
 *)

Section section_formatted_function_composition.

(**
                       
            ff                    gg
 A' ------------------> B' ---------------> C'
 |                      |                   |
 .                      .                   .
 |                      |                   |
 .                      . b'                . c'
 |                      |                   |
 .                      .                   v
 | a'                   | B --------------> C
 .                      . |        g
 |                      | .  
 .                      . | b
 |                      | .
 .                      . |
 |                      v v
 . A ------------------> B_
 | |          f         
 . .                 
 | |                   
 . . a
 | |
 . .
 v v
  A_

 *)
  
  Variables (A_ : Type) (A : Type) (a : A -> A_) (B_ : Type) (f : A -> B_) .
  Variables (A' : Type) (a' : A' -> A_) (B' : Type) (b' : B' -> B_) .
  Variables (ff : formatted_EXTERNAL A_ A a B_ f A' a' B' b').
  Variables (B : Type) (b : B -> B_) (C : Type) (g : B -> C) .
  Variables (C' : Type) (c' : C' -> C) .
  Variables (gg : formatted_EXTERNAL B_ B b C g B' b' C' c').

  Lemma formatted_composition : formatted_EXTERNAL A_ (intersection B_ B b A f)
                                          (composition _ A (multimultiplier B_ B b A f) A_ a)
                                          C (composition _ _ (transported B_ B b A f) _ g)
                                          A' a' C' c' .
  Proof.
    unfold formatted_EXTERNAL , intersection , composition , multimultiplier , transported in * . intros x_y_ x'_ . unshelve eexists.
    - refine (proj1_sig (gg (snd (proj1_sig x_y_)) _ )). unshelve eexists.
      + refine (proj1_sig (ff (fst (proj1_sig x_y_)) _ )).
        exists (proj1_sig x'_) .
        exact (proj2_sig x'_).
      + rewrite <- (proj2_sig x_y_).
        set ( x'__ := exist _ _ _ ).  exact (proj2_sig (ff (fst (proj1_sig x_y_)) x'__)).
    - set ( ff_x'__ := exist _ _ _ ).  exact (proj2_sig (gg _ ff_x'__)).
  Defined.

End section_formatted_function_composition.

(**

<p style="text-align:center"> &nbsp&nbsp You &nbsp&nbsp read &nbsp&nbsp this korean word &nbsp&nbsp 안녕 &nbsp&nbsp . </p>

<p> The ends is to describe how there are <b>varying degrees of grammar , or correspondingly how there are varying degrees of sense . At one extreme (concrete grammar / syntax) is the ink which is stored as some sequence of tokens on the paper . At the other extreme (sense) is the total reality which is stored as some memory dynamics in the brain , which is then written on or read from the paper . In the middle is the COQ computer ( automatized/implicit notations ) . </p>

<p> <b>Some information may be lost or gained when varying the degree of grammar</b> . Primo , here is how information may be lost . Any grammar has multiple entry classes , for example : verb , noun , sentence ( , import-another-language ... ) . These grammar entries may be <b>mutually interdependent</b> , and this mutual interdepence may be lost when varying the degree from grammar to sense . In other words : the mutually recursive functions which read/parse the sequence/list of tokens on the paper are indeed very mutually dependent , but the corresponding inductive types in the COQ computer cease to be mutually dependent and have only-hierarchical dependence . ( <i>MEMO : one solution to recover the mutual dependence in the COQ computer is to describe the inductive types <b> "manually" </b> instead of automatically ... </i> ) </p>

<p> Secondo , here is how information may be gained when varying the degree of grammar . Comnmonly , some text (sequence of tokens) is read from the paper then many post-processing of varying degree are done to determine how much sensible is this information . One example of such post-processing step is to decide/check/verify whether two grammatical items have indeed the same sense ( "unify" ) as wanted . Now <b>this sensible post-processing step may be integrated into the tokens-grammar itself</b> such to give some new intermediate grammar , but the reader algorithm for such intermediate grammar may become almost as complex as the reference sense . For example , if the reference sense is the Coq computer , then the reader for this intermediate grammar, which asserts/checks/verifies that some two grammatical items have the same sense , needs the (outer) COQ unification algorithm ( and such reader may therefore be programmed in LTAC , <i>or even better : disregard any such intermediate grammar and directly touch the reference sense as the <b>COQ computer automatized/implicit notations</b> ... </i> ) . </p>


*)

Module Type Sensible_Grammar .

From Coq Require Import Lists.List Arith.Compare.
Import ListNotations.
 
(* some preliminary definitions *)
Definition boolunit (n : nat) : Type := match n with
                                        O => unit
                                      | S m => bool
                                      end .
Definition truett (n : nat) : boolunit n := match n with
                                            O => tt
                                          | S m => true
                                          end .

(* the corresponding inductive types [ilist] and [bulist] in the COQ computer cease to be mutually dependent and have only-hierarchical dependence *)

(* indeed , [bulist] is absent from [ilist] *)
Inductive ilist : nat -> Type :=
  INil : ilist 0
| ICons : forall (n : nat), ilist n -> boolunit n (* asserts/checks this [n] is same sense as [n] in [ilist n] *)
                -> ilist (S n) .

(* but , [ilist] is present in [bulist] *)
Inductive bulist : forall n : nat, ilist n  -> boolunit n -> Type :=
  Bulist : forall (n : nat) (l : ilist n), bulist n l (truett n) .

Compute (existT ilist 1 (ICons 0 INil tt)) .
Compute (existT ilist 2 (ICons 1 (ICons 0 INil tt) true)) .

(* the mutually recursive functions which read/parse the sequence/list of tokens on the paper are indeed very mutually dependent . *)
Inductive token : Type :=
  Token_INil | Token_ICons | Token_Bulist .

Fixpoint reader_INil (steps : nat) (xs : list token)
: option ( (ilist 0) * list token) :=
match steps with
| 0 => None
| S steps' =>
  match xs with
  | [] => None
  | x :: rest =>
    match x with
    | Token_INil => Some ( INil , rest )
    | Token_ICons => None
    | Token_Bulist => None
    end
  end
  end

with (* mutual dependence *)
reader_ICons (steps : nat) (xs : list token)
: option ( { n & ilist (S n) } * list token) :=
match steps with
| 0 => None
| S steps' =>
  match xs with
  | [] => None
  | x :: rest =>
    match x with
    | Token_ICons =>
      (* inner mutual dependence *)
      match (reader_ilist steps' rest) with
      | Some (existT _ n l , rest') =>
        (* HERE: outer mutual dependence *)
        match (reader_bulist steps' rest' ) with
        | Some ( (existT _ n' (existT _ l' (existT _ b t))) , rest'' ) =>
          (* Memo in the reference sense :
             [ | ICons : forall n , ilist n -> boolunit n ... ] 
             asserts/checks this [n] is same sense as [n] in [ilist n] *)
          (* Luckily , the reader for this particular intermediate grammar 
             does not need the (outer) COQ unification algorithm ... *)
          (* HERE: (easy) integration of the would-be-tokens-only grammar 
             with this post-processing step of the reference-sense *)
          match Compare.Pcompare n n' with
          | inleft (right pr) =>
            Some (existT _ n (ICons n l ltac:(rewrite pr; exact b)) , rest'')
          | _ => None
          end
        | None => None
        end
      | None => None
      end
    | Token_INil => None
    | Token_Bulist => None
    end
  end
end

with (* mutual dependence *)
reader_ilist (steps : nat) (xs : list token)
: option ( { n & ilist n } * list token) :=
match steps with
| 0 => None
| S steps' =>
  match (reader_ICons steps' xs) with
  | Some (existT _ n l , rest') => Some ( (existT _ (S n) l) , rest')
  | None =>
    match (reader_INil steps' xs) with
    | Some (l , rest') => Some ( (existT _ 0 l) , rest')
    | None => None
    end
  end
end
  
with (* mutual dependence *)
reader_Bulist (steps : nat) (xs : list token)
: option ( { n & { l : ilist n & bulist n l (truett n) } } * list token) :=
match steps with
| 0 => None
| S steps' =>
  match xs with
  | [] => None
  | x :: rest =>
    match x with
    | Token_Bulist =>
      (* HERE: outer mutual dependence *)
      match (reader_ilist steps' rest ) with
      | Some (existT _ n l , rest') =>
        Some ((existT _ n (existT _ l (Bulist n l))) , rest')
      | None => None
      end
    | Token_INil => None
    | Token_ICons => None
    end
  end
end

with (* mutual dependence *)
reader_bulist (steps : nat) (xs : list token)
: option ( { n & { l : ilist n & { b : boolunit n & bulist n l b } } } * list token) :=
match steps with
| 0 => None
| S steps' =>
  match (reader_Bulist steps' xs) with
  | Some ((existT _ n (existT _ l t)) , rest) =>
    Some ( (existT _ n (existT _ l (existT _ (truett n) t))) , rest)
  | None => None
  end
end .

Compute ( reader_ilist 100 [Token_ICons ;
                                        Token_INil ;
                                        Token_Bulist ;
                                        Token_INil ] ) .
(* = Some (existT _ 1 (ICons 0 INil tt), []) *)
Compute ( reader_ilist 100 [Token_ICons ;
                                        Token_ICons ; Token_INil ; Token_Bulist ; Token_INil ;
                                        Token_Bulist ;
                                        Token_ICons ; Token_INil ; Token_Bulist ; Token_INil ] ).
(* = Some (existT _ 2 (ICons 1 (ICons 0 INil tt) true), []) *)
Compute ( reader_ilist 100 [Token_ICons ;
                                        Token_ICons ; Token_INil ; Token_Bulist ; Token_INil ;
                                        Token_Bulist ;
                                        Token_INil ] ).
(* = None *)

End Sensible_Grammar.


Module Type Reader .
Check list.
  From Coq Require Import Lists.List.
  Import ListNotations.
  
  Module Type Reader1.
  Inductive ilist : nat -> Type :=
    INil : ilist 0
  | ISucc : forall n , ilist n -> ilist (S n).

  Inductive token : Type :=
    Token_INil | Token_ISucc .

  Fixpoint readerINil (steps : nat) (xs : list token) : option ( (ilist 0) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: xs => match x with
                           | Token_INil => Some ( INil , xs )
                           | Token_ISucc => None
                           end
               end
  end
  
  with readerISucc (steps : nat) (xs : list token) : option ( {n & ilist (S n) } * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: rest => match x with
                           | Token_ISucc => (match (readerilist steps' rest ) with
                                            | Some (existT _ n l , rest') => Some (existT _ n (ISucc n l) , rest')
                                            | None => None
                                            end)
                           | Token_INil => None
                           end
               end
  end

  with readerilist (steps : nat) (xs : list token) : option ( {n & ilist n} * list token) :=
  match steps with
  | 0 => None
  | S steps' => (match (readerISucc steps' xs) with
                        | Some (existT _ n l , rest') => Some ( (existT _ (S n) l) , rest')
                        | None => (match (readerINil steps' xs) with
                                  | Some (l , rest') => Some ( (existT _ 0 l) , rest')
                                  | None => None
                                  end)
                end)
  end.
  Check ( readerilist 100 [Token_ISucc ; Token_INil] )         . 
  Compute ( readerilist 100 [Token_ISucc ; Token_INil] ) .
  Compute ( readerilist 100 [Token_ISucc ; Token_ISucc ; Token_INil] ) .
  End Reader1.

  Module Type Reader11.
  Inductive ilist : nat -> Type :=
    INil : ilist 0
  | ISucc : forall n , ilist n -> bool -> ilist (S n).

  Inductive itrue : forall n : nat, ilist n -> bool -> Type :=
    ITrue : forall n  (l : ilist n) , itrue n l true .

  Inductive token : Type :=
    Token_INil | Token_ISucc | Token_ITrue .

  Fixpoint readerINil (steps : nat) (xs : list token) : option ( (ilist 0) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: xs => match x with
                           | Token_INil => Some ( INil , xs )
                           | Token_ISucc => None
                           | Token_ITrue => None
                           end
               end
  end
  
  with readerISucc (steps : nat) (xs : list token) : option ( {n & ilist (S n) } * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: rest => match x with
                           | Token_ISucc => (match (readerilist steps' rest ) with
                                            | Some (existT _ n l , rest') => (match (readeritrue steps' rest' ) with
                                                                             | Some ( (existT _ n' (existT _ l' (existT _ b t))) , rest'' )
                                                                               => Some (existT _ n (ISucc n l b) , rest'')
                                                                             | None => None
                                                                             end)
                                            | None => None
                                            end)
                           | Token_INil => None
                           | Token_ITrue => None
                           end
               end
  end

  with readerilist (steps : nat) (xs : list token) : option ( {n & ilist n} * list token) :=
  match steps with
  | 0 => None
  | S steps' => (match (readerISucc steps' xs) with
                        | Some (existT _ n l , rest') => Some ( (existT _ (S n) l) , rest')
                        | None => (match (readerINil steps' xs) with
                                  | Some (l , rest') => Some ( (existT _ 0 l) , rest')
                                  | None => None
                                  end)
                end)
  end

  with  readerITrue (steps : nat) (xs : list token) : option ( { n & { l : ilist n & itrue n l true } } * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: rest => match x with
                           | Token_ITrue => (match (readerilist steps' rest ) with
                                            | Some (existT _ n l , rest') =>
                                    Some ((existT _ n (existT _ l (ITrue n l))) , rest')
                                            | None => None
                                            end)
                           | Token_INil => None
                           | Token_ISucc => None
                           end
               end
  end
  with readeritrue (steps : nat) (xs : list token) : option ( { n & { l : ilist n & { b & itrue n l b } } } * list token) :=
  match steps with
  | 0 => None
  | S steps' => (match (readerITrue steps' xs) with
                        | Some ((existT _ n (existT _ l t)) , rest) => Some ( (existT _ n (existT _ l (existT _ true t))) , rest)
                        | None => None
                end)
  end .

  Compute ( readerilist 100 [Token_ISucc ; Token_INil ; Token_ITrue ; Token_INil ] ) .
  Compute ( readerilist 100 [Token_ISucc ; Token_ISucc ; Token_INil ; Token_ITrue ; Token_INil ; Token_ITrue ; Token_INil ] ) .
  End Reader11.

  Module Type Sensible_Grammar .

  From Coq Require Import Lists.List Arith.Compare.
  Import ListNotations.
 
  (* but the corresponding inductive types in the COQ computer cease to be mutually dependent and have only-hierarchical dependence *)

  Definition boolunit (n : nat) :=
    match n with
      O => unit
    | S m => bool
    end .

  Inductive ilist : nat -> Type :=
    INil : ilist 0
  | ISucc : forall n , ilist n -> boolunit n (* asserts/checks this [n] is same sense as [n] in [ilist n] *)
                  -> ilist (S n) .

  Definition truett (n : nat) : boolunit n := 
    match n with
      O => tt
    | S m => true
    end .

  Inductive itrue : forall n : nat, ilist n -> boolunit n -> Type :=
    ITrue : forall n  (l : ilist n) , itrue n l (truett n) .

  (* the mutually recursive functions which read/parse the sequence/list of tokens on the paper are indeed very mutually dependent . *)
  Inductive token : Type :=
    Token_INil | Token_ISucc | Token_ITrue .

  Fixpoint readerINil (steps : nat) (xs : list token) : option ( (ilist 0) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: xs => match x with
                           | Token_INil => Some ( INil , xs )
                           | Token_ISucc => None
                           | Token_ITrue => None
                           end
               end
  end
  
  with readerISucc (steps : nat) (xs : list token) : option ( {n & ilist (S n) } * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: rest => match x with
                           | Token_ISucc => (match (readerilist steps' rest ) with
                                            | Some (existT _ n l , rest') => (match (readeritrue steps' rest' ) with
                                                                             | Some ( (existT _ n' (existT _ l' (existT _ b t))) , rest'' )
                                                                               => match Compare.Pcompare n n' with
                                                                                 | inleft (right pr) => Some (existT _ n (ISucc n l ltac:(rewrite pr; exact b)) , rest'')
                                                                                 | _ => None
                                                                                 end
                                                                             | None => None
                                                                             end)
                                            | None => None
                                            end)
                           | Token_INil => None
                           | Token_ITrue => None
                           end
               end
  end

  with readerilist (steps : nat) (xs : list token) : option ( {n & ilist n} * list token) :=
  match steps with
  | 0 => None
  | S steps' => (match (readerISucc steps' xs) with
                        | Some (existT _ n l , rest') => Some ( (existT _ (S n) l) , rest')
                        | None => (match (readerINil steps' xs) with
                                  | Some (l , rest') => Some ( (existT _ 0 l) , rest')
                                  | None => None
                                  end)
                end)
  end

  with readerITrue (steps : nat) (xs : list token) : option ( { n & { l : ilist n & itrue n l (truett n) } } * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: rest => match x with
                           | Token_ITrue => (match (readerilist steps' rest ) with
                                            | Some (existT _ n l , rest') =>
                                    Some ((existT _ n (existT _ l (ITrue n l))) , rest')
                                            | None => None
                                            end)
                           | Token_INil => None
                           | Token_ISucc => None
                           end
               end
  end
  with readeritrue (steps : nat) (xs : list token) : option ( { n & { l : ilist n & { b : boolunit n & itrue n l b } } } * list token) :=
  match steps with
  | 0 => None
  | S steps' => (match (readerITrue steps' xs) with
                        | Some ((existT _ n (existT _ l t)) , rest) => Some ( (existT _ n (existT _ l (existT _ (truett n) t))) , rest)
                        | None => None
                end)
  end .

  Compute ( readerilist 100 [Token_ISucc ;
                                         Token_INil ;
                                         Token_ITrue ;
                                         Token_INil ] ) .
  (* = Some (existT _ 1 (ISucc 0 INil tt), []) *)
  Compute ( readerilist 100 [Token_ISucc ;
                                         Token_ISucc ; Token_INil ; Token_ITrue ; Token_INil ;
                                         Token_ITrue ;
                                         Token_ISucc ; Token_INil ; Token_ITrue ; Token_INil ] ).
  (* = Some (existT _ 2 (ISucc 1 (ISucc 0 INil tt) true), []) *)
  Compute ( readerilist 100 [Token_ISucc ;
                                         Token_ISucc ; Token_INil ; Token_ITrue ; Token_INil ;
                                         Token_ITrue ;
                                         Token_INil ] ).
  (* = None *)

  End Sensible_Grammar.

  Module Type Reader2.
  Inductive ilist : nat -> Type :=
    INil : ilist 0
  | ISucc : forall n , ilist n -> ilist (S n)
  | IApp : forall n , ilist n -> forall m , ilist m -> ilist (n + m).

  Inductive token : Type :=
    Token_INil | Token_ISucc | Token_IApp .

  Fixpoint readerINil (steps : nat) (xs : list token) : option ( (ilist 0) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: xs => match x with
                           | Token_INil => Some ( INil , xs )
                           | Token_ISucc => None
                           | Token_IApp => None
                           end
               end
  end
  
  with readerISucc (steps : nat) (xs : list token) : option ( {n & ilist (S n) } * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: rest => match x with
                           | Token_ISucc => (match (readerilist steps' rest ) with
                                            | Some (existT _ n l , rest') => Some (existT _ n (ISucc n l) , rest')
                                            | None => None
                                            end)
                           | Token_INil => None
                           | Token_IApp => None
                           end
               end
  end

  with readerIApp (steps : nat) (xs : list token) : option ( {nm : nat * nat & ilist (fst nm + snd nm) } * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: rest => match x with
                           | Token_IApp => (match (readerilist steps' rest ) with
                                    | Some (existT _ n l , rest') => (match (readerilist steps' rest' ) with
                                                      | Some (existT _ m p , rest'') =>
                                                             Some (existT _ (n,m) (IApp n l m p) , rest'')
                                                       | None => None
                                                                     end) 
                                    | None => None
                                           end)
                           | Token_INil => None
                           | Token_ISucc => None
                           end
               end
  end

  with readerilist (steps : nat) (xs : list token) : option ( {n & ilist n} * list token) :=
  match steps with
  | 0 => None
  | S steps' => (match (readerIApp steps' xs) with
                | Some (existT _ nm l , rest') => Some ( (existT _ (fst nm + snd nm) l) , rest' )
                | None =>
                  (match (readerISucc steps' xs) with
                   | Some (existT _ n l , rest') => Some ( (existT _ (S n) l) , rest')
                   | None => (match (readerINil steps' xs) with
                             | Some (l , rest') => Some ( (existT _ 0 l) , rest')
                             | None => None
                             end)
                   end)
                end)
  end.
  Check ( readerilist 100 [Token_ISucc ; Token_INil] )         . 
  Compute ( readerilist 100 [Token_ISucc ; Token_INil] ) .
  Compute ( readerilist 100 [Token_ISucc ; Token_ISucc ; Token_INil] ) .
  Compute ( readerilist 100 [Token_IApp ; Token_ISucc ; Token_ISucc ; Token_INil ; Token_ISucc ; Token_INil] ) .
  Compute ( readerilist 100 [Token_IApp ; Token_ISucc ; Token_ISucc ; Token_IApp ; Token_INil ; Token_ISucc ; Token_INil ; Token_INil] ) .
  End Reader2.

  Module Type Reader3.
  From Coq Require Import Arith.EqNat.
  From Coq Require Import Arith.Compare.
  Inductive ilist : nat -> Type :=
    INil : ilist 0
  | ISucc : forall n , ilist n -> ilist (S n)
  | IApp : forall n , ilist n -> ilist n (* /!\ parser must do as Coq unification /!\ *) -> ilist (n + n).

  Inductive token : Type :=
    Token_INil | Token_ISucc | Token_IApp .

  Fixpoint readerINil (steps : nat) (xs : list token) : option ( (ilist 0) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: xs => match x with
                           | Token_INil => Some ( INil , xs )
                           | Token_ISucc => None
                           | Token_IApp => None
                           end
               end
  end
  
  with readerISucc (steps : nat) (xs : list token) : option ( {n & ilist (S n) } * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: rest => match x with
                           | Token_ISucc => (match (readerilist steps' rest ) with
                                            | Some (existT _ n l , rest') => Some (existT _ n (ISucc n l) , rest')
                                            | None => None
                                            end)
                           | Token_INil => None
                           | Token_IApp => None
                           end
               end
  end

  with readerIApp (steps : nat) (xs : list token) : option ( {n : nat & ilist (n + n) } * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: rest => match x with
                           | Token_IApp => (match (readerilist steps' rest ) with
                                    | Some (existT _ n l , rest') => (match (readerilist steps' rest' ) with
                                                      | Some (existT _ m p , rest'') =>
                                                        (**SHORT:    /!\ other than nat , the parser must be (at least) as powerful as the Coq unifier ; therefore don't write some separate parser, and input directly the Coq expressions using the implicit-notations . MEMO: moreover any propositional equations arguments which are not yet carried by code must be rephrased as code /!\  *)
                                                        match Pcompare n m with
                                                            | inleft (right pr) => Some (existT _ n (IApp n l ltac:(rewrite pr; exact p)) , rest'')
                                                            | _ => None
                                                        end
                                                       | None => None
                                                                     end) 
                                    | None => None
                                           end)
                           | Token_INil => None
                           | Token_ISucc => None
                           end
               end
  end

  with readerilist (steps : nat) (xs : list token) : option ( {n & ilist n} * list token) :=
  match steps with
  | 0 => None
  | S steps' => (match (readerIApp steps' xs) with
                | Some (existT _ n l , rest') => Some ( (existT _ (n + n) l) , rest' )
                | None =>
                  (match (readerISucc steps' xs) with
                   | Some (existT _ n l , rest') => Some ( (existT _ (S n) l) , rest')
                   | None => (match (readerINil steps' xs) with
                             | Some (l , rest') => Some ( (existT _ 0 l) , rest')
                             | None => None
                             end)
                   end)
                end)
  end.
  Check ( readerilist 100 [Token_ISucc ; Token_INil] )         . 
  Compute ( readerilist 100 [Token_ISucc ; Token_INil] ) .
  Compute ( readerilist 100 [Token_ISucc ; Token_ISucc ; Token_INil] ) .
  Eval simpl in  ( readerilist 100 [Token_IApp ; Token_ISucc ; Token_INil ; Token_ISucc ; Token_INil] ) .
  Compute  ( readerilist 100 [Token_IApp ; Token_ISucc ; Token_INil ; Token_ISucc ; Token_INil] ) .
  Compute  ( readerilist 100 [Token_IApp ; Token_ISucc ; Token_ISucc ; Token_INil ; Token_ISucc ; Token_INil] ) .
  Compute ( readerilist 100 [Token_IApp ; Token_ISucc ; Token_ISucc ; Token_IApp ; Token_INil ; Token_ISucc ; Token_INil ; Token_INil] ) .
  End Reader3.
  
  Module Type Reader4.
  Require Import Coq.Arith.EqNat.
  Require Import  Coq.Arith.Compare.
  Inductive ilist : nat -> Type :=
    INil : ilist 0
  | ISucc : forall n , ilist n -> ilist (S n)
  | IApp : forall n , ilist n -> ilist n -> ilist (n + n).

  Inductive token : Type :=
    Token_INil | Token_ISucc | Token_IApp .

  Ltac readerINil := fun steps xs =>
  match steps with
  | 0 => constr:(None : option ( (ilist 0) * list token))
  | S ?steps' => match xs with
                 [] => constr:(None: option ( (ilist 0) * list token))
               | ?x :: ?xs => match x with
                           | Token_INil => constr:(Some ( INil , xs ) : option ( ilist (0) * list token))
                           | Token_ISucc => constr:(None: option ( (ilist 0) * list token))
                           | Token_IApp => constr:(None: option ( (ilist 0) * list token))
                           end
               end
  end  
  
  with readerISucc := fun steps xs =>
  match steps with
  | 0 => constr:(None:option ( {n & ilist (S n) } * list token))
  | S ?steps' => match xs with
                 [] => constr:(None:option ( {n & ilist (S n) } * list token))
               | ?x :: ?rest => match x with
                           | Token_ISucc => (match (readerilist steps' rest ) with
                                            | Some (existT (fun n : nat => ilist n)  ?n ?l , ?rest') => constr:(Some (existT _ n (ISucc n l) , rest') :option ( {n & ilist (S n) } * list token) )
                                            | None => constr:(None:option ( {n & ilist (S n) } * list token))
                                            end)
                           | Token_INil => constr:(None:option ( {n & ilist (S n) } * list token))
                           | Token_IApp => constr:(None:option ( {n & ilist (S n) } * list token))
                           end
               end
  end

  with readerIApp := fun steps xs =>
  match steps with
  | 0 => constr:(None: option ( {n : nat & ilist (n + n) } * list token))
  | S ?steps' => match xs with
                 [] => constr:(None: option ( {n : nat & ilist (n + n) } * list token))
               | ?x :: ?rest => match x with
                           | Token_IApp => (match (readerilist steps' rest ) with
                                    | Some (existT (fun n : nat => ilist n) ?n ?l , ?rest') => (match (readerilist steps' rest' ) with
                                                      | Some (existT _ ?m ?p , ?rest'') =>
                                                        match constr:(Pcompare n m) with
                                                            | inleft (right ?pr) => constr:(Some (existT _ n (IApp n l ltac:(rewrite pr; exact p)) , rest'') : option ( {n : nat & ilist (n + n) } * list token))
                                                            | _ => constr:(None: option ( {n : nat & ilist (n + n) } * list token))
                                                        end
                                                       | None => constr:(None: option ( {n : nat & ilist (n + n) } * list token))
                                                                     end) 
                                    | None => constr:(None: option ( {n : nat & ilist (n + n) } * list token))
                                           end)
                           | Token_INil => constr:(None: option ( {n : nat & ilist (n + n) } * list token))
                           | Token_ISucc => constr:(None: option ( {n : nat & ilist (n + n) } * list token))
                           end
               end
  end

  with readerilist := fun steps xs => 
  match steps with
  | 0 => constr:(None: option ( {n & ilist n} * list token))
  | S ?steps' =>  (match (readerIApp steps' xs) with
                | Some (existT (fun n : nat => ilist (n + n))  ?n ?l , ?rest') : option ( {n : nat & ilist (n + n) } * list token) => constr:(Some ( (existT _ (n + n) l) , rest' ) : option ( {n & ilist n} * list token))
                | None : option ( {n : nat & ilist (n + n) } * list token) =>
                  (match (readerISucc steps' xs) with
                   | Some (existT (fun n : nat => ilist (S n))  ?n ?l , ?rest') : option ( {n & ilist (S n)} * list token) => constr:(Some ( (existT _ (S n) l) , rest') : option ( {n & ilist n} * list token))
                   | None : option ( {n & ilist (S n)} * list token) => (match (readerINil steps' xs) with
                             | Some (?l , ?rest') : option ( (ilist 0) * list token) => constr:(Some ( (existT _ 0 l) , rest') : option ( {n & ilist n} * list token))
                             | None : option ( (ilist 0) * list token) => constr:(None: option ( {n & ilist n} * list token))
                             end)
                   end)
                end)
  end.
  Ltac tac := let kdd :=readerilist 100 [Token_ISucc ; Token_INil] in idtac .
  Goal True.  let kdd :=readerINil 100 [Token_INil] in
           pose (fe :=  kdd).

           let kkdd :=readerilist 100 [Token_INil] in
           pose (ffe :=  kkdd).
Fail let lkddd := readerilist 100 [Token_ISucc ; Token_INil] in
           pose (lfee :=  lkddd).            Abort.
(*           Set Ltac Debug.
Fail let kddd := readerilist 100 [Token_ISucc ; Token_INil] in
           pose (fee :=  kddd).            
  Compute ( readerilist 100 [Token_ISucc ; Token_INil] ) .
  Compute ( readerilist 100 [Token_ISucc ; Token_ISucc ; Token_INil] ) .
  Eval simpl in  ( readerilist 100 [Token_IApp ; Token_ISucc ; Token_INil ; Token_ISucc ; Token_INil] ) .
  Compute  ( readerilist 100 [Token_IApp ; Token_ISucc ; Token_INil ; Token_ISucc ; Token_INil] ) .
  Compute  ( readerilist 100 [Token_IApp ; Token_ISucc ; Token_ISucc ; Token_INil ; Token_ISucc ; Token_INil] ) .
  Compute ( readerilist 100 [Token_IApp ; Token_ISucc ; Token_ISucc ; Token_IApp ; Token_INil ; Token_ISucc ; Token_INil ; Token_INil] ) . *)
  End Reader4.
(*
  Fixpoint readerINil (steps : nat) (xs : list token) : option ( (ilist 0) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: xs => match x with
                           | Token_INil => Some ( INil , xs )
                           | Token_ISucc => None
                           end
               end
  end
  
  with readerISucc (steps : nat) (xs : list token) n : option ( ilist (S n) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: rest => match x with
                           | Token_ISucc => (match (readerilist steps' rest n) with
                                            | Some (l , rest') => Some (ISucc n l , rest')
                                            | None => None
                                            end)
                           | Token_INil => None
                           end
               end
  end

  with readerilist (steps : nat) (xs : list token) n : option ( ilist n * list token) :=
  match steps with
  | 0 => None
  | S steps' => match n with
               | 0 => (match (readerINil steps' xs) with
                        | Some (l , rest') => Some (l , rest')
                        | None => None
                        end)
               | S m => (match (readerISucc steps' xs m) with
                        | Some (l , rest') => Some (l , rest')
                        | None => None
                        end)
               end
  end.

  Check ( readerilist 100 [Token_ISucc ; Token_INil] 1 )         . 
  Compute ( readerilist 100 [Token_ISucc ; Token_INil] 1 ) .
  Compute ( readerilist 100 [Token_ISucc ; Token_ISucc ; Token_INil] 2 ) .
  Check ( readerilist 100 [Token_ISucc ; Token_ISucc ; Token_INil] _ ) .
  
Module Type Reader .

  From Coq Require Import Lists.List.
  Import ListNotations.

  Parameter Decode_Object1 : Type .
  Inductive objects : Type -> Type :=
    Object1 : objects Decode_Object1
  | Object2 : forall deco , objects deco -> objects (deco * deco).

  Inductive token : Type :=
    Token_Object1 | Token_Object2 .

  Fixpoint readerObject1 (steps : nat) (xs : list token) : option ( (objects Decode_Object1) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: xs => match x with
                           | Token_Object1 => Some ( Object1 , xs )
                           | Token_Object2 => None
                           end
               end
  end
  
  with readerObject2 (steps : nat) (xs : list token) deco : option ( (objects (deco * deco)) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: xs => match x with
                           | Token_Object2 => None
                           | Token_Object1 => None
                           end
               end
  end.

  Fixpoint reader (steps : nat) (xs : list token) (expec : Type) : option ( {Deco : Type & (objects Deco)} * list token) :=
  match steps with
  | 0 => None
  | S steps' => match (readerObject1 steps' xs) with
                 Some (o , rest) => Some ( existT _ _ o , rest )
               | None => match (readerObject2 steps' xs expec) with
                          Some (o , rest) => Some ( existT _ _ o , rest )
                        | None => None
                        end
               end
  end.
  
(** ---------------------------------  **)
  Fixpoint readerObject1 (steps : nat) (xs : list token) : option ( (objects Decode_Object1) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: xs => match x with
                           | Token_Object1 => Some ( Object1 , xs )
                           | Token_Object2 => None
                           end
               end
  end
  
  with readerObject2 (steps : nat) (xs : list token) deco : option ( (objects (deco * deco)) * list token) :=
  match steps with
  | 0 => None
  | S steps' => match xs with
                 [] => None
               | x :: xs => match x with
                           | Token_Object2 => None
                           | Token_Object1 => None
                           end
               end
  end.

  Fixpoint reader (steps : nat) (xs : list token) (expec : Type) : option ( {Deco : Type & (objects Deco)} * list token) :=
  match steps with
  | 0 => None
  | S steps' => match (readerObject1 steps' xs) with
                 Some (o , rest) => Some ( existT _ _ o , rest )
               | None => match (readerObject2 steps' xs expec) with
                          Some (o , rest) => Some ( existT _ _ o , rest )
                        | None => None
                        end
               end
  end.
*)