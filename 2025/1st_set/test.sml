datatype bop = Plus | Minus | Mult | Or | And | Xor | Eq | Lt | Gt
datatype typ = Bool | Int | Arrow of typ * typ

datatype expr =
    IntLit of int
  | BoolLit of bool
  | Bop of bop * expr * expr
  | ITE of expr * expr * expr
  | LetIn of string * expr * expr
  | Var of string
  | Fun of string * typ * expr
  | App of expr * expr

type 'a env = (string * 'a) list

datatype value =
    VInt of int
  | VBool of bool
  | VClo of value env * string * expr

exception TypeError of string
exception RunTimeError of string

(* ---------------------------------------------------------------- *)
fun sameType (Int,  Int)  = true
  | sameType (Bool, Bool) = true
  | sameType (Arrow(a1,b1), Arrow(a2,b2)) =
      sameType (a1,a2) andalso sameType (b1,b2)
  | sameType _            = false

fun lookup _ [] = NONE
  | lookup x ((y,v)::r) = if x = y then SOME v else lookup x r

fun insert x v e = (x,v)::e
fun empty () : 'a env = []

(* ----------------------------- typeCheck ------------------------- *)
fun typeCheck g (IntLit _)  = Int
  | typeCheck g (BoolLit _) = Bool
  | typeCheck g (Var x) =
      (case lookup x g of 
         SOME t => t 
       | NONE   => raise TypeError "unbound")
  | typeCheck g (Bop (op,e1,e2)) =
      let
        val t1 = typeCheck g e1
        val t2 = typeCheck g e2
      in
        case op of
            Plus  => if sameType (t1,Int)  andalso sameType (t2,Int)
                     then Int  else raise TypeError ""
          | Minus => if sameType (t1,Int)  andalso sameType (t2,Int)
                     then Int  else raise TypeError ""
          | Mult  => if sameType (t1,Int)  andalso sameType (t2,Int)
                     then Int  else raise TypeError ""
          | And   => if sameType (t1,Bool) andalso sameType (t2,Bool)
                     then Bool else raise TypeError ""
          | Or    => if sameType (t1,Bool) andalso sameType (t2,Bool)
                     then Bool else raise TypeError ""
          | Xor   => if sameType (t1,Bool) andalso sameType (t2,Bool)
                     then Bool else raise TypeError ""
          | Eq    => if sameType (t1,t2) then Bool else raise TypeError ""
          | Lt    => if sameType (t1,Int)  andalso sameType (t2,Int)
                     then Bool else raise TypeError ""
          | Gt    => if sameType (t1,Int)  andalso sameType (t2,Int)
                     then Bool else raise TypeError ""
      end
  | typeCheck g (ITE (c,t,e)) =
      (case typeCheck g c of
          Bool =>
            let
              val tt = typeCheck g t
              val te = typeCheck g e
            in
              if sameType (tt,te) then tt
              else raise TypeError ""
            end
        | _ => raise TypeError "")
  | typeCheck g (LetIn (x,e1,e2)) =
      let val tx = typeCheck g e1
      in
        typeCheck (insert x tx g) e2
      end
  | typeCheck g (Fun (x,a,b)) =
      let val rt = typeCheck (insert x a g) b
      in
        Arrow (a, rt)
      end
  | typeCheck g (App (f,a)) =
      (case typeCheck g f of
          Arrow (pt, rt) =>
            if sameType (typeCheck g a, pt) then rt
            else raise TypeError ""
        | _ => raise TypeError "nonfun")

(* ------------------------------- eval --------------------------- *)
fun eval r (IntLit n)  = VInt n
  | eval r (BoolLit b) = VBool b
  | eval r (Var x) =
      (case lookup x r of 
         SOME v => v
       | NONE   => raise RunTimeError "unbound")
  | eval r (Bop (op,e1,e2)) =
      let
        val v1 = eval r e1
        val v2 = eval r e2
      in
        case (op,v1,v2) of
            (Plus ,VInt a,VInt b)   => VInt (a + b)
          | (Minus,VInt a,VInt b)   => VInt (a - b)
          | (Mult ,VInt a,VInt b)   => VInt (a * b)
          | (And  ,VBool a,VBool b) => VBool (a andalso b)
          | (Or   ,VBool a,VBool b) => VBool (a orelse b)
          | (Xor  ,VBool a,VBool b) => VBool (a <> b)
          | (Eq   ,VInt a,VInt b)   => VBool (a = b)
          | (Eq   ,VBool a,VBool b) => VBool (a = b)
          | (Lt   ,VInt a,VInt b)   => VBool (a < b)
          | (Gt   ,VInt a,VInt b)   => VBool (a > b)
          | _ => raise RunTimeError "mismatch"
      end
  | eval r (ITE (c,t,e)) =
      (case eval r c of
          VBool true  => eval r t
        | VBool false => eval r e
        | _ => raise RunTimeError "cond")
  | eval r (LetIn (x,e1,e2)) =
      let val v1 = eval r e1
      in
        eval (insert x v1 r) e2
      end
  | eval r (Fun (x,_,b)) = VClo (r,x,b)
  | eval r (App (f,a)) =
      (case eval r f of
          VClo (env,param,body) =>
            let val av = eval r a
            in
              eval (insert param av env) body
            end
        | _ => raise RunTimeError "nonfun")
