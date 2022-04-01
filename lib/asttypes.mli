(** source locations, for reporting errors *)
type location = Lexing.position * Lexing.position

type constant =
  | Cunit (** () *)
  | Cbool of bool (** true, false *)
  | Cint of int (** 42 *)
  | Cstring of string (** "abc" *)

type unop =
  | Unot (** not *)
  | Uminus (** - *)

type binop =
  | Beq | Bneq | Blt | Ble | Bgt | Bge (** =, <>, <, <=, >, >= *)
  | Badd | Bsub | Bmul | Bdiv (** +, -, *, / *)
  | Band | Bor (** &&, || *)
  | Bconcat (** ^ *)

type is_rec = bool

type ident =
  string (** an identifier: x, y, z... *)
