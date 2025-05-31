(* Binary operators *)
datatype bop = Plus | Minus | Mult
             | Or | And | Xor
             | Eq | Lt | Gt

(* Types *)
datatype typ =
    Bool                              (* Boolean Type *)
  | Int                               (* Integer Type *)
  | Arrow of typ * typ                (* Function Type *)

(* Expressions *)
datatype expr =
    IntLit of int                     (* Integer Literals *)
  | BoolLit of bool                   (* Boolean Literals *)
  | Bop of bop * expr * expr          (* Binary Operators *)
  | ITE of expr * expr * expr         (* If-then-else expressions *)
  | LetIn of string * expr * expr     (* Let-in expressions *)
  | Var of string                     (* Variables *)
  | Fun of string * typ * expr        (* Anonymous Functions *)
  | App of expr * expr                (* Function Applications *)

(* Environments as association lists *)
type 'a env = (string * 'a) list

(* Values *)
datatype value =
    VInt of int                       (* Integer Values *)
  | VBool of bool                     (* Boolean Values *)
  | VClo of value env * string * expr (* Closures *)

(* Exceptions *)
exception TypeError of string
exception RunTimeError of string
exception Unimplemented of string

(* Empty environment *)
fun empty () : 'a env =
  []

(* Look up a variable in the environment *)
fun lookup (x : string) (e : 'a env) : 'a option =
  case e of
    [] => NONE
  | (name, value) :: rest =>
      if name = x then SOME value
      else lookup x rest

(* Insert a new binding (x,v) in the environment *)
fun insert (x : string) (v : 'a) (e : 'a env) : 'a env =
  (x, v) :: e

(* Type checker *)
fun typeCheck (te : typ env) (exp : expr) : typ =
  case exp of
    IntLit _ => Int
  | BoolLit _ => Bool
  | Var x =>
      (case lookup x te of
         SOME t => t
       | NONE => raise (TypeError ("Unbound variable: " ^ x)))
  | Bop (bop, e1, e2) =>
      let
        val t1 = typeCheck te e1
        val t2 = typeCheck te e2
      in
        case bop of
          Plus => (case (t1, t2) of
                     (Int, Int) => Int
                   | _ => raise (TypeError "Arithmetic operators expect Int arguments"))
        | Minus => (case (t1, t2) of
                      (Int, Int) => Int
                    | _ => raise (TypeError "Arithmetic operators expect Int arguments"))
        | Mult => (case (t1, t2) of
                     (Int, Int) => Int
                   | _ => raise (TypeError "Arithmetic operators expect Int arguments"))
        | Or => (case (t1, t2) of
                   (Bool, Bool) => Bool
                 | _ => raise (TypeError "Logical operators expect Bool arguments"))
        | And => (case (t1, t2) of
                    (Bool, Bool) => Bool
                  | _ => raise (TypeError "Logical operators expect Bool arguments"))
        | Xor => (case (t1, t2) of
                    (Bool, Bool) => Bool
                  | _ => raise (TypeError "Logical operators expect Bool arguments"))
        | Eq => (case (t1, t2) of
                   (Int, Int) => Bool
                 | (Bool, Bool) => Bool
                 | _ => raise (TypeError "Comparison operators expect arguments of same type"))
        | Lt => (case (t1, t2) of
                   (Int, Int) => Bool
                 | _ => raise (TypeError "Comparison operators expect Int arguments"))
        | Gt => (case (t1, t2) of
                   (Int, Int) => Bool
                 | _ => raise (TypeError "Comparison operators expect Int arguments"))
      end
  | ITE (e1, e2, e3) =>
      let
        val t1 = typeCheck te e1
        val t2 = typeCheck te e2
        val t3 = typeCheck te e3
      in
        case t1 of
          Bool =>
            if t2 = t3 then t2
            else raise (TypeError "Branches must have the same type")
        | _ => raise (TypeError "Condition must be Bool")
      end
  | LetIn (x, e1, e2) =>
      let
        val t1 = typeCheck te e1
        val te' = insert x t1 te
      in
        typeCheck te' e2
      end
  | Fun (x, t, e) =>
      let
        val te' = insert x t te
        val t_body = typeCheck te' e
      in
        Arrow (t, t_body)
      end
  | App (e1, e2) =>
      let
        val t1 = typeCheck te e1
        val t2 = typeCheck te e2
      in
        case t1 of
          Arrow (t_arg, t_ret) =>
            if t2 = t_arg then t_ret
            else raise (TypeError "Function argument type mismatch")
        | _ => raise (TypeError "Application requires a function")
      end

(* Interpreter *)
fun eval (ve : value env) (exp : expr) : value =
  case exp of
    IntLit n => VInt n
  | BoolLit b => VBool b
  | Var x =>
      (case lookup x ve of
         SOME v => v
       | NONE => raise (RunTimeError ("Unbound variable: " ^ x)))
  | Bop (bop, e1, e2) =>
      let
        val v1 = eval ve e1
        val v2 = eval ve e2
      in
        case (v1, v2) of
          (VInt n1, VInt n2) =>
            (case bop of
               Plus => VInt (n1 + n2)
             | Minus => VInt (n1 - n2)
             | Mult => VInt (n1 * n2)
             | Eq => VBool (n1 = n2)
             | Lt => VBool (n1 < n2)
             | Gt => VBool (n1 > n2)
             | _ => raise (RunTimeError "Operand type mismatch"))
        | (VBool b1, VBool b2) =>
            (case bop of
               Or => VBool (b1 orelse b2)
             | And => VBool (b1 andalso b2)
             | Xor => VBool (b1 <> b2)
             | Eq => VBool (b1 = b2)
             | _ => raise (RunTimeError "Operand type mismatch"))
        | _ => raise (RunTimeError "Operand type mismatch")
      end
  | ITE (e1, e2, e3) =>
      let
        val v1 = eval ve e1
      in
        case v1 of
          VBool true => eval ve e2
        | VBool false => eval ve e3
        | _ => raise (RunTimeError "Condition must be Bool")
      end
  | LetIn (x, e1, e2) =>
      let
        val v1 = eval ve e1
        val ve' = insert x v1 ve
      in
        eval ve' e2
      end
  | Fun (x, t, e) =>
      VClo (ve, x, e)
  | App (e1, e2) =>
      let
        val v1 = eval ve e1
        val v2 = eval ve e2
      in
        case v1 of
          VClo (closure_env, x, body) =>
            let
              val new_env = insert x v2 closure_env
            in
              eval new_env body
            end
        | _ => raise (RunTimeError "Application requires a function")
      end