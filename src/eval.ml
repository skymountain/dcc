module Env = Syntax.Env

let (|>) = BatPervasives.(|>)

type exval =
  | VInt of int
  | VBool of bool
  | VUnit
  | VFun of Syntax.idlist * Syntax.expr * exval Env.t ref
  | VNil
  | VCons of exval * exval

let rec is_vlist = function
  | VNil -> true
  | VCons (_, rval) -> is_vlist rval
  | VInt _ | VBool _ | VUnit | VFun _ -> false

let vcons_val = function
  | VCons (x,y) -> Some (x,y)
  | VInt _ | VBool _ | VUnit | VFun _ | VNil -> None

let rec string_of_exval = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VFun _ -> "<fun>"
  | VNil -> "[]"
  | VCons (lval, rval) when is_vlist rval -> begin
      let rec string_of_list = function
        | VNil -> "]"
        | VCons (lval, rval) -> begin
            "; " ^ string_of_exval lval ^ string_of_list rval
          end
        | VInt _ | VBool _ | VUnit | VFun _ as v -> string_of_exval v
      in
      Printf.sprintf "[%s%s" (string_of_exval lval) (string_of_list rval)
    end
  | VCons (lval, rval) -> begin
      let lstr = string_of_exval lval in
      let lstr =
        match lval with
        | VCons _ -> "( " ^ lstr ^ " )"
        | VInt _ | VBool _ | VUnit | VFun _ | VNil -> lstr
      in
      Printf.sprintf "%s :: %s" lstr (string_of_exval rval)
    end

exception Error of string

let err s = raise (Error s)

let eval_binary_expr bin lval rval = match (bin, lval, rval) with
  | (Syntax.BPlus, VInt li, VInt ri) -> VInt (li + ri)
  | (Syntax.BPlus,
     (VInt _ | VBool _ | VFun _ | VUnit | VNil | VCons _),
     (VInt _ | VBool _ | VFun _ | VUnit | VNil | VCons _)) ->
      err "both operands of + must be integers"

  | (Syntax.BMult, VInt li, VInt ri) -> VInt (li * ri)
  | (Syntax.BMult,
     (VInt _ | VBool _ | VFun _ | VUnit | VNil | VCons _),
     (VInt _ | VBool _ | VFun _ | VUnit | VNil | VCons _)) ->
      err "both operands of * must be integers"

  | (Syntax.BMinus, VInt li, VInt ri) -> VInt (li - ri)
  | (Syntax.BMinus,
     (VInt _ | VBool _ | VFun _ | VUnit | VNil | VCons _),
     (VInt _ | VBool _ | VFun _ | VUnit | VNil | VCons _)) ->
      err "both operands of - must be integers"
      
  | (Syntax.BLt, VInt li, VInt ri) -> VBool (li < ri)
  | (Syntax.BLt,
     (VInt _ | VBool _ | VFun _ | VUnit | VNil | VCons _),
     (VInt _ | VBool _ | VFun _ | VUnit | VNil | VCons _)) ->
      err "both operands of < must be integers"

let eval_const = function
  | Syntax.CInt i -> VInt i
  | Syntax.CBool b -> VBool b
  | Syntax.CUnit -> VUnit
  | Syntax.CNil -> VNil

let check_pattern =
  let add_id ids id =
    if List.for_all ((<>) id) ids then
      Some (id::ids)
    else
      None
  in
  let rec iter ids = function
  | Syntax.PatWildcard | Syntax.PatConst _ -> Some ids
  | Syntax.PatIdent id -> add_id ids id
  | Syntax.PatAs (pat, id) -> begin
      add_id ids id |> BatOption.bind (fun ids -> iter ids pat)
    end
  | Syntax.PatCons (lpat, rpat) -> begin
      iter ids lpat |> BatOption.bind (fun ids -> iter ids rpat)
    end
  | Syntax.PatOr (lpat, rpat) -> begin
      iter ids lpat |> BatOption.bind
        (fun lids ->
           iter ids rpat |> BatOption.bind
             (fun rids ->
                if BatList.make_compare compare lids rids = 0 then
                  Some lids
                else
                  None))
    end
  in
  fun ids pat -> not (iter ids pat = None)

let rec eval_pattern env v = function
  | Syntax.PatWildcard -> Some env
  | Syntax.PatConst c -> begin match v, c with
    | VInt i, Syntax.CInt j when i = j -> Some env
    | VBool b, Syntax.CBool b' when b = b' -> Some env
    | VUnit, Syntax.CUnit -> Some env
    | VNil, Syntax.CNil -> Some env
    | (VInt _ | VBool _ | VUnit | VFun _ | VNil | VCons _),
      (Syntax.CInt _ | Syntax.CBool _ | Syntax.CUnit | Syntax.CNil)
        -> None
    end
  | Syntax.PatIdent id -> Some (Env.add id v env)
  | Syntax.PatAs (pat, id) -> eval_pattern (Env.add id v env) v pat
  | Syntax.PatCons (lpat, rpat) -> begin
      vcons_val v |> BatOption.bind
        (fun (lval, rval) ->
           eval_pattern env lval lpat |> BatOption.bind
             (fun env -> eval_pattern env rval rpat))
    end
  | Syntax.PatOr (lpat, rpat) -> begin
      match eval_pattern env v lpat with
      | None -> eval_pattern env v rpat
      | env -> env
    end

let rec eval_expr env = function
  | Syntax.EVar id -> begin
      Env.find_default_fun env id
        (fun () -> err (Printf.sprintf "unbounded variable %s is used" id))
    end

  | Syntax.EConst c -> eval_const c
  | Syntax.EBin (bin, lexpr, rexpr) -> begin
      let lval = eval_expr env lexpr in
      let rval = eval_expr env rexpr in
      eval_binary_expr bin lval rval
    end

  | Syntax.ECons (lexpr, rexpr) -> begin
      VCons (eval_expr env lexpr, eval_expr env rexpr)
    end

  | Syntax.EFun (ids, expr) -> VFun (ids, expr, ref env)
  | Syntax.EApp (fexpr, aexpr) -> begin
      let aval = eval_expr env aexpr in
      match eval_expr env fexpr with
      | VInt _ | VBool _ | VUnit | VNil | VCons _ -> begin
          err "the left of application isn't function"
        end
      | VFun (ids, expr, env) -> begin
          let env = !env in
          let id, ids = List.hd ids, List.tl ids in
          let env = Env.add id aval env in
          if BatList.is_empty ids then eval_expr env expr
          else VFun (ids, expr, ref env)
        end
    end

  | Syntax.ELet (ldecl, expr) -> begin
      let _, _, env = eval_let_decl env ldecl in
      eval_expr env expr
    end

  | Syntax.EMatch (expr, matchs) -> begin
      let rec iter_patmatch env v = function
        | [] -> err "matching failure"
        | (pat, expr)::matchs -> begin
            match eval_pattern env v pat with
            | Some env -> eval_expr env expr
            | None -> iter_patmatch env v matchs
          end
      in
      let ids = BatList.of_enum (Env.keys env) in
      if List
        .map fst matchs |> List.for_all (check_pattern ids) then
        iter_patmatch env (eval_expr env expr) matchs
      else
        err "incorrect patterns are included"
    end

  | Syntax.EIf (cexpr, texpr, eexpr) -> begin
      match eval_expr env cexpr with
      | VBool b -> eval_expr env (if b then texpr else eexpr)
      | VInt _ | VFun _ | VUnit | VNil | VCons _ -> begin
          err "conditional isn't boolean expression"
        end
    end

  | Syntax.EShift | Syntax.EReset -> failwith "not implemented"

and eval_let_decl env = function
  | Syntax.DLet (id, ids, expr) -> begin
        let v =
          if BatList.is_empty ids then eval_expr env expr
          else VFun (ids, expr, ref env)
        in
        (id, v, Env.add id v env)
    end

  | Syntax.DLetRec (id, ids, expr) -> begin
      let denv = ref Env.empty in
      let v = match BatList.is_empty ids with
        | true -> begin
            let open Syntax in
            match expr with
            | EFun (ids, expr) -> VFun (ids, expr, denv)
            | EVar _ | EConst _ | EBin _ | ECons _ | EApp _
            | ELet _ | EMatch _ | EIf _ | EShift | EReset ->
                err "recursive definition isn't function"
          end
        | false -> VFun (ids, expr, denv)
      in
      let env = Env.add id v env in
      denv := env;
      (id, v, env)
    end

let eval_program env = function
  | Syntax.PExpr expr -> ("-", eval_expr env expr, env)
  | Syntax.PLet ldecl -> eval_let_decl env ldecl
