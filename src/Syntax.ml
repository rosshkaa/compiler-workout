(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
    
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

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
let rec eval s e = match e with
    | Const x -> x
    | Var n -> s n
    | Binop (op, e1, e2) -> let r1 = eval s e1 in
                            let r2 = eval s e2 in
                            str_to_op op r1 r2

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
    let rec eval config statement = 
      let (state, input, output) = config in
      match statement with
        | Read variable_name -> (match input with
          | head::tail -> (Expr.update variable_name head state, tail, output))
        | Write expression -> (state, input, output @ [Expr.eval state expression])
        | Assign (variable_name, expression) -> (Expr.update variable_name (Expr.eval state expression) state, input, output)
        | Seq (statement1, statement2) -> eval (eval config statement1) statement2;;
                                                         
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o