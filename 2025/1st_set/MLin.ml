(* ---------- MLin.ml (Corrected Implementation + Tests) ---------- *)

(* Binary operators *)
type bop =
  | Plus  | Minus | Mult | Div    (* Added Div here *) 
  | Or    | And   | Xor
  | Eq    | Lt    | Gt

(* Types *)
type typ =
  | Bool                     (* Boolean Type *)
  | Int                      (* Integer Type *)
  | Arrow of typ * typ       (* Function Type *)

(* Expressions *)
type expr =
  | IntLit   of int                    (* Integer Literals *)
  | BoolLit  of bool                   (* Boolean Literals *)
  | Bop      of bop * expr * expr      (* Binary Operators *)
  | ITE      of expr * expr * expr     (* If-then-else expressions *)
  | LetIn    of string * expr * expr   (* Let-in expressions *)
  | Var      of string                 (* Variables *)
  | Fun      of string * typ * expr    (* Anonymous Functions *)
  | App      of expr * expr            (* Function Applications *)

(* Environments as association lists *)
type 'a env = (string * 'a) list

(* Values *)
type value =
  | VInt  of int                       (* Integer Values *)
  | VBool of bool                      (* Boolean Values *)
  | VClo  of value env * string * expr (* Closures *)

(* Exceptions *)
exception TypeError    of string
exception RunTimeError of string

(* Environment helpers *)
let empty () : 'a env = []           
(* ^ Empty list: no bindings *) 

let rec lookup (x : string) (e : 'a env) : 'a option =
  match e with
  | [] -> None
  | (y, v) :: rest ->
      if x = y then Some v else lookup x rest  
      (* First match wins *) 

let insert (x : string) (v : 'a) (e : 'a env) : 'a env =
  (x, v) :: e                            
  (* Shadow old binding by consing new one *) 

(* Type checker *)
let rec typeCheck (gamma : typ env) (exp : expr) : typ =
  match exp with
  | IntLit _ -> Int

  | BoolLit _ -> Bool

  | Var x ->
      (match lookup x gamma with
       | Some t -> t
       | None   -> raise (TypeError ("Unbound variable " ^ x)))
      (* Variable must be in gamma *) 

  | Bop (op, e1, e2) ->
      let t1 = typeCheck gamma e1 in
      let t2 = typeCheck gamma e2 in
      (match op with
       | Plus  | Minus | Mult | Div ->
           if t1 = Int && t2 = Int then Int
           else raise (TypeError "Arithmetic op expects Int * Int")
       | Or    | And   | Xor ->
           if t1 = Bool && t2 = Bool then Bool
           else raise (TypeError "Boolean op expects Bool * Bool")
       | Eq    | Lt    | Gt  ->
           if t1 = Int && t2 = Int then Bool
           else raise (TypeError "Comparison op expects Int * Int"))
      (* Arithmetic ops: Int×Int→Int; Boolean ops: Bool×Bool→Bool; Comparisons: Int×Int→Bool *) 

  | ITE (c, e1, e2) ->
      (match typeCheck gamma c with
       | Bool ->
           let t1 = typeCheck gamma e1 in
           let t2 = typeCheck gamma e2 in
           if t1 = t2 then t1
           else raise (TypeError "Branches of if must have same type")
       | _ -> raise (TypeError "Condition of if must be Bool"))
      (* Condition must be Bool, and branches must agree *) 

  | LetIn (x, e1, e2) ->
      let t1 = typeCheck gamma e1 in
      typeCheck ((x, t1) :: gamma) e2
      (* Extend gamma with x:T1 when checking e2 *) 

  | Fun (x, paramTy, body) ->
      let gamma' = (x, paramTy) :: gamma in
      let bodyTy = typeCheck gamma' body in
      Arrow (paramTy, bodyTy)
      (* Function type = paramTy → bodyTy *) 

  | App (eFun, eArg) ->
      (match typeCheck gamma eFun with
       | Arrow (tArg, tRes) ->
           let t2 = typeCheck gamma eArg in
           if tArg = t2 then tRes
           else raise (TypeError "Function applied to wrong argument type")
       | _ -> raise (TypeError "Trying to apply a non-function"))
      (* eFun must be Arrow(tArg,tRes), and tArg = typeCheck eArg *) 

(* Interpreter *)
let rec eval (sigma : value env) (exp : expr) : value =
  match exp with
  | IntLit n -> VInt n

  | BoolLit b -> VBool b

  | Var x ->
      (match lookup x sigma with
       | Some v -> v
       | None   -> raise (RunTimeError ("Unbound variable " ^ x)))
      (* Variable must be in sigma *) 

  | Bop (op, e1, e2) ->
      let v1 = eval sigma e1 in
      let v2 = eval sigma e2 in
      (match (op, v1, v2) with
       | (Plus ,  VInt a,   VInt b)   -> VInt (a + b)
       | (Minus,  VInt a,   VInt b)   -> VInt (a - b)
       | (Mult ,  VInt a,   VInt b)   -> VInt (a * b)
       | (Div  ,  VInt a,   VInt 0)   -> raise (RunTimeError "Division by zero")
       | (Div  ,  VInt a,   VInt b)   -> VInt (a / b)
       | (And  ,  VBool p,  VBool q)  -> VBool (p && q)
       | (Or   ,  VBool p,  VBool q)  -> VBool (p || q)
       | (Xor  ,  VBool p,  VBool q)  -> VBool (p <> q)
       | (Eq   ,  VInt a,   VInt b)   -> VBool (a = b)
       | (Lt   ,  VInt a,   VInt b)   -> VBool (a < b)
       | (Gt   ,  VInt a,   VInt b)   -> VBool (a > b)
       | _ -> raise (RunTimeError "Operand type mismatch"))
      (* Pattern-match (op, v1, v2); Div recognized *) 

  | ITE (c, e1, e2) ->
      (match eval sigma c with
       | VBool true  -> eval sigma e1
       | VBool false -> eval sigma e2
       | _           -> raise (RunTimeError "Condition of if must be Bool"))
      (* Must evaluate c to a boolean at runtime *) 

  | LetIn (x, e1, e2) ->
      let v1 = eval sigma e1 in
      eval ((x, v1) :: sigma) e2
      (* Extend sigma with (x,v1) when evaluating e2 *) 

  | Fun (x, _, body) ->
      VClo (sigma, x, body) 
      (* Build closure capturing sigma *) 

  | App (eFun, eArg) ->
      (match eval sigma eFun with
       | VClo (captEnv, x, body) ->
           let argVal = eval sigma eArg in
           eval ((x, argVal) :: captEnv) body
       | _ -> raise (RunTimeError "Trying to apply a non-function"))
      (* eFun must evaluate to a closure *) 

(* ---------- Test harness below ---------- *)

type testCase = {
  name          : string;
  exp           : expr;
  expectedType  : typ;
  expectedValue : value
}

let tests = [
  {
    name = "Test 1: let z = 42 in if x > 5 then z - 20 else z + 1";
    exp =
      LetIn ("z", IntLit 42,
        LetIn ("x", IntLit 6,
          ITE(
            Bop (Gt, Var "x", IntLit 5),
            Bop (Minus, Var "z", IntLit 20),
            Bop (Plus,  Var "z", IntLit 1)
          )
        )
      );
    expectedType  = Int;
    expectedValue = VInt 22
  };
  {
    name = "Test2: let x = 10 in x + 5";
    exp =
      LetIn ("x", IntLit 10,
        Bop (Plus, Var "x", IntLit 5)
      );
    expectedType  = Int;
    expectedValue = VInt 15
  };
  {
    name = "Test3: let foo = fun x => if x = 0 then 0 else x + 2 in foo 40 - foo 0";
    exp =
      LetIn ("foo",
        Fun ("x", Int,
          ITE(
            Bop (Eq, Var "x", IntLit 0),
            IntLit 0,
            Bop (Plus, Var "x", IntLit 2)
          )
        ),
        Bop (Minus,
          App (Var "foo", IntLit 40),
          App (Var "foo", IntLit 0)
        )
      );
    expectedType  = Int;
    expectedValue = VInt 42
  };
  {
    name = "Test4: let x = true in let z = if x then 10 else 20 in let bar = fun y => 2*y in bar z";
    exp =
      LetIn ("x", BoolLit true,
        LetIn ("z",
          ITE (Var "x", IntLit 10, IntLit 20),
          LetIn ("bar",
            Fun ("y", Int, Bop (Mult, IntLit 2, Var "y")),
            App (Var "bar", Var "z")
          )
        )
      );
    expectedType  = Int;
    expectedValue = VInt 20
  }
]

let assertTest (name : string) (condition : bool) =
  if condition then
    print_endline (name ^ " -- PASS")
  else
    print_endline (name ^ " -- FAIL")

let runTest (t : testCase) : unit =
  let actualTy  = typeCheck [] t.exp in
  let actualVal = eval [] t.exp in
  assertTest (t.name ^ " (type)")  (actualTy = t.expectedType);
  assertTest (t.name ^ " (value)") (actualVal = t.expectedValue)

let runAllTests () =
  List.iter runTest tests

(* Finally, run all tests! *)
let () = runAllTests ()
