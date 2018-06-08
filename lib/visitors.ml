open Cil
open Cilprinter
open Printf

module E = Errormsg

module IntSet =
  Set.Make(
      struct
        let compare = compare
        type t = int
      end
    )

(* class collectTypeLabels result = object
 *   inherit nopCilVisitor
 *
 *   method vstmt stmt =
 *     let str, stmt = Cdiff.stmt_to_typelabel stmt in
 *     result := IntSet.add str !result;
 *     SkipChildren
 * end *)

class ast_printer id = object
  inherit nopCilVisitor
  method vinst i =
    E.log "%d instr:\n%a\n---\n" id d_instr i;
    DoChildren

  method vstmt s =
    E.log "%d stmt:\n%a\n---\n" id d_stmt s;
    DoChildren
end


type var_contexts = (string, string list) Hashtbl.t

class var_collector = object(self)
  inherit nopCilVisitor

  val contexts : var_contexts = Hashtbl.create 3

  method add_context var_name context =
    let new_context =
      match Hashtbl.find_opt contexts var_name with
      | None -> [context]
      | Some contexts -> context :: contexts
    in
    Hashtbl.replace contexts var_name new_context

  method vstmt stmt =
    let output_instruction operation instruction =
      match instruction with
      | Set ((Var (v), offset), exp, loc)
        | Call (Some (Var (v), offset), exp, _, loc) ->
         let var_name =
           match offset with
           | Field (fieldinfo, offset) ->
              Printf.sprintf "%s.%s" v.vname fieldinfo.fname
           | Index (exp, offset) ->
              let exp_doc = Pretty.dprintf "%a" d_exp exp in
              let exp_string = Pretty.sprint 80000 exp_doc in
              Printf.sprintf "%s[%s]" v.vname exp_string
           | NoOffset -> v.vname
         in
         let context_doc = Pretty.dprintf "%s: %a" operation d_exp exp in
         let context = Pretty.sprint 80000 context_doc in
         self#add_context var_name context
      | _ -> E.log "Other instruction"
    in
    let output_exp operation exp =
      match exp with
      | Lval (Var (v), _) ->
         let context_doc = Pretty.dprintf "%s: %a" operation d_exp exp in
         let context = Pretty.sprint 80000 context_doc in
         self#add_context v.vname context
      | _ -> ()
    in
    match stmt.skind with
    | Instr (instructions) ->
       List.iter (output_instruction "Assign") instructions;
       DoChildren
    | Return (Some (exp), loc) -> output_exp "Return" exp; DoChildren
    | If (exp, b1, b2, loc)-> output_exp "If" exp; DoChildren
    | _ -> DoChildren

  method get_contexts = contexts
end
