(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)

let bool_to_int x = if x then 1 else 0
let int_to_bool x = if x == 0 then false else true

let boolop_with_ret_to_int op = fun lhs rhs -> bool_to_int (op (int_to_bool lhs) (int_to_bool rhs))
let boolop_to_int op = fun lhs rhs -> bool_to_int (op lhs rhs)

let str_to_op op = match op with
    | "+" -> ( + )
    | "-" -> ( - )
    | "*" -> ( * )
    | "/" -> ( / )
    | "%" -> ( mod )
    | "!!" -> boolop_with_ret_to_int ( || )
    | "&&" -> boolop_with_ret_to_int ( && )
    | "==" -> boolop_to_int ( == )
    | "!=" -> boolop_to_int ( != )
    | "<=" -> boolop_to_int ( <= )
    | "<" -> boolop_to_int ( < )
    | ">=" -> boolop_to_int ( >= )
    | ">" -> boolop_to_int ( > )
    | _ -> failwith "unsupported op"


let rec eval s e = match e with
    | Const x -> x
    | Var n -> s n
    | Binop (op, e1, e2) -> let r1 = eval s e1 in
                            let r2 = eval s e2 in
                            str_to_op op r1 r2


    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
let parse_binops op =  ostap(- $(op)), (fun x y -> Binop (op, x, y))

    ostap (
      expr:
        !(Ostap.Util.expr
           (fun x -> x)
           (Array.map (fun (a, ops) -> a, List.map parse_binops ops)
                [|
                  `Lefta, ["!!"];
                  `Lefta, ["&&"];
                  `Nona , ["=="; "!="; "<="; ">="; "<"; ">"];
                  `Lefta, ["+"; "-"];
                  `Lefta, ["*"; "/"; "%"];
                |]
           )
           primary
         );
      primary: x:IDENT { Var x } | n:DECIMAL { Const n } | -"(" expr -")"
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval config stmt = 
	let (state, istream, ostream) = config in match stmt with
		| Read variable_name -> (match istream with
			  | head::tail -> (Expr.update variable_name head state, tail, ostream))

		| Write expr -> (state, istream, ostream @ [Expr.eval state expr])

		| Assign (variable_name, exp) -> (Expr.update variable_name (Expr.eval state exp) state, istream, ostream)

		| Seq (l, r) -> eval (eval config l) r;;

    (* Statement parser *)
    ostap (
      stmt:
			x:IDENT ":=" e:!(Expr.expr) {Assign(x, e)}
			| "read" "(" x:IDENT ")" {Read x}
			| "write" "(" e:!(Expr.expr) ")" {Write e};

		parse: s:stmt ";" rest:parse {Seq(s, rest)} | stmt
      )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
