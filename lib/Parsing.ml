(**********)
(* Lexing *)
(**********)

type token = Backslash | Str of string | Dot | LPar | RPar | HALT

let token_to_string = function
  | Backslash -> "\\"
  | Str s -> s
  | Dot -> "."
  | LPar -> "("
  | RPar -> ")"
  | HALT -> "HALT"

let recognize_symbol = function
  | '\\' -> Some Backslash
  | '.' -> Some Dot
  | '(' -> Some LPar
  | ')' -> Some RPar
  | _ -> None

let valid_iden_part c = Core.Char.is_alphanum c || c = '_' || c = '-' || c = '\''

let rec tokenize = function
  | [] -> []
  | c :: cs as tokens -> (
      match recognize_symbol c with
      | Some t -> t :: tokenize cs
      | None ->
          if Core.Char.is_whitespace c then
            tokenize cs
          else if valid_iden_part c then
            let iden_raw, cs' = Util.span tokens ~f:valid_iden_part in
            Str (Core.String.of_char_list iden_raw) :: tokenize cs'
          else
            [ HALT ])

(***********)
(* Parsing *)
(***********)

type 'elem parsing_error = NotEOF | UnexpectedEOF | UnexpectedElement of { got : 'elem; expected : 'elem option }

let parsing_error_to_string = function
  | NotEOF -> "Input has not been fully consumed"
  | UnexpectedEOF -> "Expected more input, but it has been fully consumed"
  | UnexpectedElement e -> (
      match e.expected with
      | None -> "Unexpected token: " ^ token_to_string e.got
      | Some expected -> "Unexpected token: got " ^ token_to_string e.got ^ ", but expected " ^ token_to_string expected
      )

type ('a, 'elem) parser = Parser of ('elem list -> ('a * 'elem list, 'elem parsing_error) result)

let run_parser (Parser g) = g
let ( let+ ) (Parser p) f = Parser (fun l -> match p l with Ok (x, l') -> Ok (f x, l') | Error err -> Error err)

let ( let* ) (Parser p) f =
  Parser (fun l -> match p l with Ok (x, l') -> run_parser (f x) l' | Error err -> Error err)

let return x = Parser (fun l -> Ok (x, l))
let fail err = Parser (Fun.const (Error err))

let ap pf px =
  let* f = pf in
  let* x = px in
  return (f x)

let ( <* ) pa pb =
  let* a = pa in
  let* _ = pb in
  return a

let liftA2 f px py =
  let* x = px in
  let* y = py in
  return (f x y)

let ( <|> ) (Parser pa) (Parser pb) =
  Parser
    (fun l ->
      let res = pa l in
      Result.fold ~ok:(Fun.const res) ~error:(Fun.const (pb l)) res)

(* Simple combinators *)

let get = Parser (fun l -> match l with [] -> Error UnexpectedEOF | x :: xs -> Ok (x, xs))
let ok = Parser (fun l -> Ok ((), l))
let eof = Parser (fun l -> match l with [] -> Ok ((), []) | _ :: _ -> Error NotEOF)

let satisfy ~pred =
  let* c = get in
  if pred c then
    return c
  else
    fail (UnexpectedElement { got = c; expected = None })

let element elem = satisfy ~pred:(( = ) elem)

(* Context-free grammar of lambda expressions

term ::= \ iden+ . term
       | app

app ::= app var
      | var

var = ( expr )
    | iden

iden = A-Za-z0-9_-'

*)

open Lambda

let rec parse_term =
  lazy
    ((let* _ = element Backslash in
      let* binders = Lazy.force parse_idens in
      let* _ = element Dot in
      let* next = Lazy.force parse_term in
      return (unfold_binders binders next))
    <|> Lazy.force parse_app)

and parse_app =
  let rec parse_app' lhs =
    (let* rhs = Lazy.force parse_var in
     parse_app' (App (lhs, rhs)))
    <|> return lhs
  in
  lazy
    (let* lhs = Lazy.force parse_var in
     parse_app' lhs)

and parse_var =
  lazy
    ((let* _ = element LPar in
      let* e = Lazy.force parse_term in
      let* _ = element RPar in
      return e)
    <|> let+ iden = parse_iden in
        Var iden)

and parse_idens =
  let rec star acc =
    (let* iden = parse_iden in
     star (iden :: acc))
    <|> return (List.rev acc)
  in
  lazy
    (let* iden = parse_iden in
     star [ iden ])

and parse_iden =
  let* t = get in
  match t with Str x -> return x | t -> fail (UnexpectedElement { got = t; expected = Some (Str "<identifier>") })

(** Parses a string into a lambda expression, outputs a [result]. *)
let parse str = str |> Core.String.to_list |> tokenize |> run_parser (Lazy.force parse_term <* eof) |> Result.map fst

(** Parses a string into a lambda expression. Fails is the result is an [Error]. *)
let parse_exn str = Result.get_ok (parse str)
