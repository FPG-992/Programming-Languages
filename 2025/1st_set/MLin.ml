(* MLin.ml *)

(* Binary operators *)
type bop =
  | Plus  | Minus | Mult | Div
  | Or    | And   | Xor
  | Eq    | Lt    | Gt

(* Types *)
type typ =
  | Bool
  | Int
  | Arrow of typ * typ

(* Expressions *)
type expr =
  | IntLit   of int
  | BoolLit  of bool
  | Bop      of bop * expr * expr
  | ITE      of expr * expr * expr
  | LetIn    of string * expr * expr
  | Var      of string
  | Fun      of string * typ * expr
  | App      of expr * expr

(* Environments *)
type 'a env = (string * 'a) list

(* Values *)
type value =
  | VInt  of int
  | VBool of bool
  | VClo  of value env * string * expr

exception TypeError    of string
exception RunTimeError of string

(* Environment helpers *)
let empty () : 'a env = []

let rec lookup (x : string) (e : 'a env) : 'a option =
  match e with
  | [] -> None
  | (y, v) :: rest -> if x = y then Some v else lookup x rest

let insert (x : string) (v : 'a) (e : 'a env) : 'a env =
  (x, v) :: e

(* Type checker *)
let rec typeCheck (gamma : typ env) (exp : expr) : typ =
  match exp with
  | IntLit _ -> Int
  | BoolLit _ -> Bool
  | Var x ->
      (match lookup x gamma with Some t -> t | None -> raise (TypeError ("Unbound variable " ^ x)))
  | Bop (op, e1, e2) ->
      let t1 = typeCheck gamma e1 in
      let t2 = typeCheck gamma e2 in
      (match op with
       | Plus | Minus | Mult | Div ->
           if t1 = Int && t2 = Int then Int else raise (TypeError "Arithmetic op expects Int * Int")
       | Or | And | Xor ->
           if t1 = Bool && t2 = Bool then Bool else raise (TypeError "Boolean op expects Bool * Bool")
       | Eq | Lt | Gt ->
           if t1 = Int && t2 = Int then Bool else raise (TypeError "Comparison op expects Int * Int"))
  | ITE (c, e1, e2) ->
      (match typeCheck gamma c with
       | Bool ->
           let t1 = typeCheck gamma e1 in
           let t2 = typeCheck gamma e2 in
           if t1 = t2 then t1 else raise (TypeError "Branches of if must have same type")
       | _ -> raise (TypeError "Condition of if must be Bool"))
  | LetIn (x, e1, e2) ->
      let t1 = typeCheck gamma e1 in
      typeCheck ((x, t1) :: gamma) e2
  | Fun (x, paramTy, body) ->
      let gamma' = (x, paramTy) :: gamma in
      let bodyTy = typeCheck gamma' body in
      Arrow (paramTy, bodyTy)
  | App (eFun, eArg) ->
      (match typeCheck gamma eFun with
       | Arrow (tArg, tRes) ->
           let t2 = typeCheck gamma eArg in
           if tArg = t2 then tRes else raise (TypeError "Function applied to wrong argument type")
       | _ -> raise (TypeError "Trying to apply a non-function"))

(* Interpreter *)
let rec eval (sigma : value env) (exp : expr) : value =
  match exp with
  | IntLit n -> VInt n
  | BoolLit b -> VBool b
  | Var x ->
      (match lookup x sigma with Some v -> v | None -> raise (RunTimeError ("Unbound variable " ^ x)))
  | Bop (op, e1, e2) ->
      let v1 = eval sigma e1 in
      let v2 = eval sigma e2 in
      (match (op, v1, v2) with
       | (Plus, VInt a, VInt b) -> VInt (a + b)
       | (Minus, VInt a, VInt b) -> VInt (a - b)
       | (Mult, VInt a, VInt b) -> VInt (a * b)
       | (Div, VInt a, VInt 0) -> raise (RunTimeError "Division by zero")
       | (Div, VInt a, VInt b) -> VInt (a / b)
       | (And, VBool p, VBool q) -> VBool (p && q)
       | (Or, VBool p, VBool q) -> VBool (p || q)
       | (Xor, VBool p, VBool q) -> VBool (p <> q)
       | (Eq, VInt a, VInt b) -> VBool (a = b)
       | (Lt, VInt a, VInt b) -> VBool (a < b)
       | (Gt, VInt a, VInt b) -> VBool (a > b)
       | _ -> raise (RunTimeError "Operand type mismatch"))
  | ITE (c, e1, e2) ->
      (match eval sigma c with
       | VBool true -> eval sigma e1
       | VBool false -> eval sigma e2
       | _ -> raise (RunTimeError "Condition of if must be Bool"))
  | LetIn (x, e1, e2) ->
      let v1 = eval sigma e1 in
      eval ((x, v1) :: sigma) e2
  | Fun (x, _, body) ->
      VClo (sigma, x, body)
  | App (eFun, eArg) ->
      (match eval sigma eFun with
       | VClo (captEnv, x, body) ->
           let argVal = eval sigma eArg in
           eval ((x, argVal) :: captEnv) body
       | _ -> raise (RunTimeError "Trying to apply a non-function"))
