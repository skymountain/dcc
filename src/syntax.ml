type id = string
type idlist = id list

module Env = Env.Make (struct type t = id let compare = compare end)

type binaryop =
  | BPlus
  | BMult
  | BLt
  | BMinus

type unaryop =
  | UMinus

type const =
  | CInt of int
  | CBool of bool
  | CUnit
  | CNil

type pat =
  | PatWildcard
  | PatConst of const
  | PatIdent of id
  | PatAs of pat * id
  | PatCons of pat * pat
  | PatOr of pat * pat

type expr =
  | EVar of id
  | EConst of const
  | EBin of binaryop * expr * expr
  | ECons of expr * expr
  | EFun of idlist * expr
  | EApp of expr * expr
  | ELet of letdecl * expr
  | EMatch of expr * (pat * expr) list
  | EIf of expr * expr * expr
  | EShift
  | EReset

and letdecl =
  | DLet of id * idlist * expr
  | DLetRec of id * idlist * expr

type program =
  | PExpr of expr
  | PLet of letdecl

let anon_id = ""
