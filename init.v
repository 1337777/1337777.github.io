Require Import mathcomp.ssreflect.ssreflect
mathcomp.ssreflect.ssrfun mathcomp.ssreflect.eqtype
mathcomp.ssreflect.ssrbool mathcomp.ssreflect.ssrnat
mathcomp.ssreflect.seq mathcomp.ssreflect.prime.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Definition _Top := tt.

Definition f := fun p => fun a => a + p .

Reset f.

Definition f : nat -> nat -> nat := fun p => fun a => a + p .

Check f.

Eval compute in f 3 2.

About f.

Print f.

Reset f.

About f.

Inductive bool := true : bool | false : bool.

Reset bool.

Print bool_rect.

Check (if true then 3 else 2) .

Eval compute in ((fun b : bool => (if b then 3 else 2)) false) .

Definition andb b1 b2 := if b1 then b2 else false.

Reset andb.

Inductive nat := 
    O : nat 
  | S : nat -> nat.

Reset nat.

Print nat_rect.

Print Grammar constr.

Check fun x => (fun n : nat => n) x.+1 .
Locate ".+1" .

Definition pred n := 
  match n with 
    O => O 
  | S t => t 
  end.

Reset pred.

Fail Definition wrong (n : nat) :=
match n with 0 => true end.

Definition sameOn_bool_nat b n :=
  match b, n with
    | true, S _ => true
    | _, _ => false
  end.

Reset sameOn_bool_nat.
Definition sameOn_bool_nat b n :=
  match b with
    | true => if n is S t then true else false
    | _ => false
  end.

Check
  fix add n m := 
    match n with
      S t => add t (S m)
      | O => m
    end .

Check 
  fun p => fix add n := 
    match n with
      S t => S (add t)
      | O => p
    end .

Definition add := 
  fun p => fix add n := 
    match n with
      S t => S (add t)
      | O => p
    end .

Reset add.

Fixpoint add p n {struct n} := 
    match n with
      S t => S (add p t)
      | O => p
    end .
Print add.

Reset add.

Section Section1.
  Variable p : nat.
  Fixpoint add n := 
      match n with
        S t => S (add t)
        | O => p
      end .
End Section1.
Print add.

Reset Section1.

Print iter.
Definition add (p : nat) : nat -> nat :=
  fun n : nat =>
    iter n (fun acc : nat => S (acc)) p .
Print foldr.

Reset add.

Fail
Fixpoint nat_empty (n : nat) {struct n}: False := 
  if n is S n' then nat_empty n' else nat_empty 0.
Fail Check nat_empty ( 3 : nat ) (** : False **).

Definition pred_for_only_odd (n : nat) := if odd n then Some (n.-1) else None.

Definition odd_for_only_small (n : nat) := if n < 100 then Some (odd n) else None.

Inductive option (A : Type) := None : option A | Some : A -> option A.

Reset option.

About option.
About None.
About Some.

Check pred_for_only_odd : nat -> option nat.
Check odd_for_only_small : nat -> option bool.

Check Some 2.

Check if (37 + 73) < 100 then Some (37+73) else None.
Eval compute in if (37 + 73) < 100 then Some (odd (37 + 73)) else None.
Fail Check if (37 + 73) < 100 then Some (odd (37 + 73)) else (None (A := nat)).
Fail Check if (37 + 73) < 100 then Some (odd (37 + 73)) else (@None nat).
Eval compute in if (37 + 73) < 100 then Some (odd (37 + 73)) else @None _.

Inductive seq (A : Type) := nil : seq A | cons : A -> seq A -> seq A.

Reset seq.

Check cons true (cons false (cons true nil)).
Check cons 2 (cons 1 (cons 3 nil)).
Check 2 :: 1 :: 3 :: nil.
Check [:: 2; 1; 3].
Check fun l : seq nat => [:: 2, 1, 3 & l].

Fixpoint number A (s : seq A) : nat :=
  match s with
      cons _ tl => S (number tl)
    | nil => 0
  end.

Fixpoint map (A : Type) (B : Type) (f : A -> B) (s : seq A) : seq B :=
  if s is e :: tl 
  then f e :: map f tl 
  else nil.

Eval compute in map (fun i : bool => ~~ i) [:: true; false; true].
Eval compute in map (fun i : nat => i.+1) [:: 2; 1; 3].
Eval compute in [seq i.+1 | i <- [:: 2; 1; 3]].

Notation "[ 'seq' E | i <- s ]" := (map (fun i => E) s).

Reset map.

Check map (fun i : bool => ~~ i) [:: true; false; true].
Check map (fun i : bool => ~~ i) .

Inductive unit : Type :=  tt : unit.

Reset unit.

Print nat_rect.

Inductive ilist : nat -> Type := 
    inil : ilist 0 
  | icons : nat -> forall m : nat, ilist m -> ilist (S m).

Definition only7 :=
  fix only7 (m : nat) : (ilist m) :=
    match m as m' return (ilist m') with
      O => inil 
    | S m0 => icons 7 (only7 m0)
    end .

Eval compute in only7 3.

Reset only7.
Definition only7 :=
  nat_rect (ilist) (inil) (fun (m0 : nat) (only7_m0 : ilist m0) => icons 7 only7_m0).

Inductive eq (A : Type) (x : A) : A -> Type :=
  eqrefl : @eq A x x .

About eqrefl.

Check @eqrefl nat (3 + 2) : @eq nat (3 + 2) (S (S (1 + 2))).

Check
  (fun (x : nat) (a : nat) (H : @eq nat x a) =>
     match H as H0 in (@eq _ _ a0) return (@eq nat (S x) (S a0)) with
     | @eqrefl _ _ => @eqrefl nat (S x)
     end)
     : forall x a : nat, @eq nat x a -> @eq nat (S x) (S a) .

Definition more :=
  (fix more (m : nat) (l : ilist m) {struct l} : ilist (S m) :=
     match l as l0 in ilist m0 return ilist (S m0) with
       inil => icons 0 inil
     | icons j m_tl tl => icons (S j) (more m_tl tl)
     end).

Eval compute in @more 3 (icons 6 (icons 7 (icons 5 inil))).

Definition inumber :=
  (fix inumber (m : nat) (l : ilist m) {struct l} : nat :=
     match l with
       inil => 0
     | icons j m_tl tl => S (inumber m_tl tl)
     end).

Definition lemma1 :=
  fix lemma1 (m : nat) (l : ilist m) {struct l} : @eq nat m (inumber l) :=
    match l as l0 in ilist m0 return @eq nat m0 (inumber l0) with

      inil =>
        (** expected (goal) output precise-classification :
        @eq nat 0 (inumber (inil)) , which computationally-is
        @eq nat 0 O  **)
        @eqrefl nat 0

    | icons j m_tl tl =>
        (** expected (goal) output precise-classification :
        @eq nat (S m_tl) (inumber (icons j m_tl tl)) , which computationally-is
        @eq nat (S m_tl) (S (inumber tl)) ;

        now by recursion, the deduction (lemma1 m_tl tl) of classification
        @eq nat (m_tl) ((inumber tl)),
        is present to do some rewrite / cast / transport  **)
        match (lemma1 m_tl tl) as H0 in @eq _ _ a0 return @eq nat (S m_tl) (S a0) with
          @eqrefl _ _ =>
	    (** expected (goal) output precise-classification :
	    @eq nat (S m_tl) (S m_tl)  **)
	    @eqrefl nat (S m_tl)
	end

    end.

Lemma lemma2 : forall (m : nat) (l : ilist m), @eq nat m (inumber l).

(** move => m l .**)
intros m l.

(** elim : l => [ | j m_tl tl lemma2_m_tl_tl ] .**)
induction l as [ | j m_tl tl lemma2_m_tl_tl ]  .

(** move => /= .**)
simpl.

exact (@eqrefl nat 0).

(** move => /= .**)
simpl.

(** move : lemma2_m_tl_tl .**)
revert lemma2_m_tl_tl.

(** move : (inumber tl) .**)
generalize (inumber tl) as a0.

Undo 2.

(** move : (inumber tl) lemma2_m_tl_tl .**)
revert lemma2_m_tl_tl ; generalize (inumber tl) as a0 .

(** move => a0 ; case .**)
destruct 1.

(** apply : eqrefl .**)
apply eqrefl.

(** Qed .**)
Defined.

Print lemma2.

Eval unfold lemma2, ilist_ind, ilist_rect in lemma2.

Goal forall xy : prod nat nat, prime (fst (xy : (nat * nat)%type)) ->
                 odd xy.2 = true -> leq 2 ((snd xy) + xy.1) .

Unset Printing Notations.
Show.

Locate "*".

Eval compute in (fun b : bool => (is_true b : Type)).

Set Printing Coercions.
Show.

Unset Printing Coercions.
Set Printing Notations.
Abort.

Lemma prime_gt1' (p : nat) : prime p <-> (1 < p) /\ prime p .
Admitted.

Goal forall xy : nat * nat, prime xy.1 -> odd xy.2 -> 2 < xy.2 + xy.1 .

move => xy .

Undo.

move => xy => pr_x odd_y .

Undo.

move=> [x y] pr_x odd_y.

Undo.

move=> [x y] /= pr_x odd_y.

Undo.

move=> [x y] /= /prime_gt1-x_gt1 odd_y.

Check @iffRL : forall P Q (eqPQ : P <-> Q), Q -> P. 
Hint View for move/ iffRL|2 (** this 2 refer to the 2 _ prefixing @iffRL _ _ above**).

Undo.

move=> [x y] /= /prime_gt1'-[x_gt1 x_pr] odd_y.

Undo.

move=> [ x [ // | y ] ] /= /prime_gt1-x_gt1.

Undo.

move=> [x [//|y]] /= /prime_gt1-x_gt1 odd_y {odd_y}.

Undo.

by move=> [ x [ // | y ] ] /= /prime_gt1-x_gt1 _ ; apply (ltn_addl _ x_gt1).

Abort.

About ltn_addl.

Goal (forall n, n * 2 = n + n) -> 6 = 3 + 3.

move => /(_ 3).

Abort.
Lemma special1arg : forall (A : Type) (P : A -> Type), 
  forall x : A , (forall x0 : A , P x0) -> P x.
Proof. move => A P x; apply. Qed.

Goal (forall n, n * 2 = n + n) -> 6 = 3 + 3.

move => /(special1arg 3).

move => <- .

Undo 2.
move/(special1arg 3).

Undo 1.
move/(_ 3).

Abort.

Lemma goal1 (x y : nat) (x_gt1 : 1 < x) (odd_y : odd y) : 2 < y + x .

Fail move : y .
move : y odd_y .

Undo.

case : y odd_y => [ | y' ].

Admitted.

Lemma goal2 (x y1 y2 : nat) (x_gt1 : 1 < x)
   (odd_y1y2 : odd (y1 - y2)) : 2 < (y1 - y2) + x .

move : (y1 - y2) odd_y1y2 .

Undo.

apply (goal1) .

Undo.

move : goal1 .
apply .

Undo 2.

apply : goal1 .

Undo 1.

refine ( @goal1 ) || ( refine ( @goal1 _ ) ||
( refine ( @goal1 _ _ ) || ( refine ( @goal1 _ _ _ ) || 
refine ( @goal1 _ _ _ _ ) ) ) ) (** or more **) .

Undo 1.

apply / goal1 .

move : (x_gt1) .

Abort.

Lemma goal2 (x y1 y2 : nat) (x_gt1 : 1 < x)
   (odd_y1y2 : odd (y1 - y2)) : 2 < (y1 - y2) + x .

move : (y1 - y2) => z in odd_y1y2 * .

Undo.

move : (y1 - y2) odd_y1y2 => z odd_y1y1 .

Undo.

move : (y1 - y2) => {y1 y2} z in odd_y1y2 * .

Abort.

Lemma goal3 (x y : nat) (x_gt1 : 1 < x) : odd y -> 2 < y + x .

move : {-1}y (erefl y). (** equational-generalize **)
case => [ | y' ] E (** case **).

Undo.

case E : y => [ | y' ] (** equational-generalize case **).

Abort.

Lemma stamps n : 12 <= n -> exists s4 s5, s4 * 4 + s5 * 5 = n.

move : n {-2}n (leqnn n). (** lessorequal-generalize **)
elim => [ | m IHm ] (** elim , then [  |  ] branching and intros **) .
Show 2 (** shows subgoal 2 which has the stronger induction hypothesis **).

Abort.

Fixpoint addacc (n : nat) (a : nat) {struct n} : nat :=
  if n is S n' then addacc n' (S a) else a .
Definition add10 (n : nat) := addacc n 10 .

Lemma add10S (n : nat) : add10 (S n) = S (add10 n).
rewrite /add10.

move : 10 .

elim : n => [ // | n IHn ] a .

simpl.
apply: (IHn (S a)).

Qed.

Lemma goal2 (x y1 y2 : nat) (x_gt1 : 1 < x)
   (odd_y1y2 : odd (y1 - y2)) : 2 < (y1 - y2) + x .

have : forall (y : nat), odd y -> 2 < y + x .

by move => [ // | y' ] /= _ ; apply : ltn_addl x_gt1 .

by apply.

Restart.

suffices : forall (y : nat), odd y -> 2 < y + x.
  by apply.

by move => [ // | y' ] /= _ ; apply : ltn_addl x_gt1 .

Qed.

Lemma leq_max m n1 n2 : (m <= maxn n1 n2) = (m <= n1) || (m <= n2) .

suff : forall x y, y <= x -> (m <= maxn x y) = (m <= x) || (m <= y).

move : (leq_total n2 n1) => /orP (** this apply-in query the view hints database **) .

case => y_le_x => /(_ _ _ y_le_x) .

by [].
by move => lem_perm; rewrite maxnC orbC.

Restart.

wlog le_n21 : n1 n2 / n2 <= n1 .

by case/orP : (leq_total n2 n1) => y_le_x => /(_ _ _ y_le_x) ;
  last move => lem_perm; rewrite maxnC orbC.

Abort.

Inductive prod (A B : Type) : Type :=
  pair : A -> (B -> prod A B) .

Reset prod.

Check (@pair bool nat (false) (S (S O))).

Definition pair_false_2 : bool * nat .

  split.
    exact: false.
  exact: (S (S O)).
Defined.

Definition fst (A B : Type) (p : prod A B) : A :=
  match p as p0 in prod _ _ return A with
    @pair _ _ a b => a
  end.

Reset fst.

Definition fst (A B : Type) : prod A B -> A .
  case.
  move => a b ; exact : a.
Defined.

Inductive sigT (A : Type) (P : A -> Type) : Type :=
    existT : forall x : A, P x -> @sigT A P .

Reset sigT.

Definition existsT_3_icons675inil : @sigT nat ilist .
  exists 3.
  exact: (icons 6 (icons 7 (icons 5 inil))).
  Show Proof.
Defined.

Inductive sum (A B : Type) : Type :=  
    inl : A -> sum A B 
  | inr : B -> sum A B .

Reset sum.

Inductive unit : Type :=  tt : unit .
Inductive empty : Type :=           .

Definition exfalso (A : Type) : empty -> A :=
  fun x : empty =>
    match x with end.

Definition is_empty (A : Type) := A -> empty .

Reset unit.

Reset _Top.

Inductive nat := 
    O : nat 
  | S : nat -> nat.

Reset nat.

Print nat_rect.

Fail
Inductive empty_nonempty := Convert (empty : empty_nonempty -> False) .

Lemma addn0 : forall m : nat, m + 0 = m.
  elim.

Undo.
move => m.
rewrite -[_ = _]/((fun m' => _) m) .
apply : nat_rect m (** or elim : m **) .

Abort.

Print Coq.Init.Logic.eq.

Check @Logic.eq_refl nat (3 + 2) : @Logic.eq nat (3 + 2) (S (S (1 + 2))).

Print Logic.eq_rect.

Lemma lemma1337 (P : nat -> nat -> Type)
  (c : nat) (pcc : P c (c * c)) (Hc : c + c = 0)
  (Heq : forall (n : nat), c + c = n - n -> c = n * 0) n1 n2 
  : P (n1 * 0) ((n2 * 0) * (n2 * 0)) .

case : ( n1 * _ ) / Heq.

Undo.
rewrite -[ n1 * _ ]Heq.

Undo.
rewrite -Heq.

Undo.

Fail rewrite -{2}Heq.
rewrite -{2}[ in X in P _ X ]Heq.

Undo.

rewrite -{2}[ (_ * 0) in X in P _ X ]Heq
  ?Hc ?subnn // (** 1 branch **)
  -!Heq // (** trunk **) 
  Hc subnn // (** 2 branches **) .

Abort.

Lemma leq_mul2l m n1 n2 :
  (m * n1 <= m * n2) = (eqn m 0) || (leq n1 n2).

rewrite [in LHS]/leq.

rewrite -[n1]/(addn 0 n1).

rewrite -/(leq _ _).

Abort.

Inductive reflect (P : Type) : bool -> Type :=
| ReflectT (p : P) : reflect P true
| ReflectF (np : P -> False) : reflect P false

Reset reflect.

About andP.

Lemma example37 a b : a && b ==> (a == b).

  case : andP .

Abort.

Lemma example13 a b : (a || ~~ a) && (a && b ==> (a == b)).

  case E: (a && _) / andP.

Abort.
Reset reflect.

About idP.
About iffP.

Inductive compare_nat (m n : nat) : bool -> bool -> bool -> Set :=
    CompareNatLt : m < n -> compare_nat m n true false false
  | CompareNatGt : n < m -> compare_nat m n false true false
  | CompareNatEq : m = n -> compare_nat m n false false true .

Reset compare_nat.

Check ltngtP : forall m n, compare_nat m n (m < n) (n < m) (m == n) .

Reset _Top.

Inductive ilist : nat -> Type := 
    inil : ilist 0 
  | icons : nat -> forall m : nat, ilist m -> ilist (S m).

Print ilist_rect.

Definition ihead_ibehead (m : nat) (l : ilist (S m)) : nat * (ilist m).

case Heq : (S _) / l => [ | j m' l' ] .

suff : False by exact: (fun f : False => match f with end).

rewrite -[False]/((fun n : nat => match n with
                      | _.+1 => True
		      | 0 => False
                      end) 0).

rewrite -[0]Heq.
exact : I.

Undo 4. 
by [].

have Heq_injective : m.+1.-1 = m'.+1.-1 by rewrite Heq.

Undo. case : Heq => Heq_injective.

rewrite [m]Heq_injective.
exact: (j , l').
Defined.

Lemma addn_classifier (m : nat) (P : nat -> nat -> Prop) (H: P 0 m)
 (H0 : forall  p : nat, P p (addn p m) -> P p.+1 (addn p m).+1)
 : forall n : nat, P n (addn n m) .

elim; [apply: H | apply: H0]. Qed.

Lemma exF x y z: addn z (addn y x) = addn (addn z y) x.

elim/addn_classifier : z / (addn z _) .

by [].
by move => p -> .

Abort.

Reset _Top.

Definition edivn_rec d :=
  fix loop m q := if m - d is m'.+1 then loop m' q.+1 else (q, m).

Definition edivn m d := if d > 0 then edivn_rec d.-1 m 0 else (0, m).

Lemma edivn_recE d m q :
  edivn_rec d m q = if m - d is m'.+1 then edivn_rec d m' q.+1 else (q,m).

 by case: m. Qed.

Lemma edivnP m d (ed := edivn m d) :
  ((d > 0) ==> (ed.2 < d)) && (m == ed.1 * d + ed.2).

case: d => [|d /=] in ed *; first by rewrite eqxx.

rewrite -[edivn m d.+1]/(edivn_rec d m 0) in ed *.
rewrite -[m]/(0 * d.+1 + m).

elim: m {-2}m 0 (leqnn m) @ed => [[]//=|n IHn [//=|m]] q le_mn.

About subn_if_gt.
About negbT.

rewrite edivn_recE subn_if_gt;
case : {-1}(d <= m) (erefl (d <= m)) => [le_dm | /negbT lt_md]; last first.

by rewrite /= ltnS ltnNge lt_md eqxx.

have /(IHn _ q.+1) : m - d <= n by rewrite (leq_trans (leq_subr d m)).

by rewrite /= mulSnr -addnA -subSS subnKC.
Qed.
