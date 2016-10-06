open Syntax

type t =
|Int
|Float
|Bool
|Unit
|Broken

let rec type_check t =
  match t with
  |Int i-> Int
  |Float f -> Float
  |Bool b-> Bool
  |Add (t1,t2)-> if (type_check t1, type_check t2) == (Int, Int) then Int
  |Sub (t1,t2)-> if (type_check t1, type_check t2) == (Int, Int) then Int
  |Mul (t1,t2)-> if (type_check t1, type_check t2) == (Int, Int) then Int
  |Div (t1,t2)-> if (type_check t1, type_check t2) == (Int, Int) then Int
  |Let (id,t)-> Unit
  |Var str-> Broken
  |If(t1,t2,t3)-> let (t1, t2, t3) = (type_check t1, type_check t2, type_check t3)
		  in if t1 == Bool and t2 == t3 then t2 else Broken
  |Eq (t1,t2)-> let (t1, t2) = (type_check t1, type_check t2)
		in (match (t1, t2) with
		|(Int, Int) -> Bool
		|_ -> Broken)
  |Neq (t1,t2)-> let (t1, t2) = (type_check t1, type_check t2)
		in (match (t1, t2) with
		|(Int, Int) -> Bool
		|_ -> Broken)
  |Lt (t1,t2)-> let (t1, t2) = (type_check t1, type_check t2)
		in (match (t1, t2) with
		|(Int, Int) -> Bool
		|_ -> Broken)
  |Le (t1,t2)-> let (t1, t2) = (type_check t1, type_check t2)
		in (match (t1, t2) with
		|(Int, Int) -> Bool
		|_ -> Broken)
  |Or (t1,t2)-> let (t1, t2) = (type_check t1, type_check t2)
		in (match (t1, t2) with
		|(Bool, Bool) -> Bool
		|_ -> Broken)
  |And (t1,t2)-> let (t1, t2) = (type_check t1, type_check t2)
		in (match (t1, t2) with
		|(Bool, Bool) -> Bool
		|_ -> Broken)
  |Not t1-> let (t1, t2) = (type_check t1, type_check t2)
		in (match t1 with
		|Bool -> Bool
		|_ -> Broken)
  |Load str->Unit

