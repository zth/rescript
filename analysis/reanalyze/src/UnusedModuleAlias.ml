open DeadCommon
open Common

let declarations = Hashtbl.create 1
let used = Hashtbl.create 1

let add ~path ~loc name =
  let aliasPath = name :: path in
  Hashtbl.replace declarations aliasPath loc;
  Hashtbl.replace used aliasPath false

let rec prefix p q =
  match (p, q) with
  | [], _ -> true
  | x :: xs, y :: ys when Name.equal x y -> prefix xs ys
  | _ -> false

let mark_path_usage path =
  Hashtbl.iter
    (fun aliasPath _loc ->
      if
        prefix aliasPath path
        ||
        match path with
        | _ :: tl -> prefix aliasPath tl
        | [] -> false
      then Hashtbl.replace used aliasPath true)
    declarations

let report_unused () =
  Hashtbl.iter
    (fun aliasPath loc ->
      match Hashtbl.find_opt used aliasPath with
      | Some true -> ()
      | _ ->
          let aliasName = Path.toModuleName ~isType:false aliasPath |> Name.toString in
          Log_.warning ~loc
            (DeadWarning
               {
                 deadWarning = WarningUnusedModuleAlias;
                 path = Path.withoutHead aliasPath;
                 message =
                   Format.asprintf
                     "@{<info>%s@} is a module alias that is never used" aliasName;
                 lineAnnotation = None;
                 shouldWriteLineAnnotation = false;
               }))
    declarations

let clear () =
  Hashtbl.reset declarations;
  Hashtbl.reset used
