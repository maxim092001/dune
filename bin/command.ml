open Import

let doc = "Execute command."
let info = Cmd.info ~doc "command"

module Cmd_arg = struct
  type t =
    | Expandable of Dune_lang.String_with_vars.t * string
    | Terminal of string

  let parse s =
    match Arg.conv_parser Arg.dep s with
    | Ok (File sw) when Dune_lang.String_with_vars.has_pforms sw -> Expandable (sw, s)
    | _ -> Terminal s
  ;;

  let pp pps = function
    | Expandable (_, s) -> Format.fprintf pps "%s" s
    | Terminal s -> Format.fprintf pps "%s" s
  ;;

  let conv = Arg.conv ((fun s -> Ok (parse s)), pp)

  let expand t ~root ~sctx =
    let open Memo.O in
    match t with
    | Terminal s -> Memo.return s
    | Expandable (sw, _) ->
      let+ path, _ =
        Target.expand_path_from_root root sctx sw
        |> Action_builder.evaluate_and_collect_facts
      in
      let context = Dune_rules.Super_context.context sctx in
      (* TODO Why are we stringifying this path? *)
      Path.to_string (Path.build (Path.Build.relative (Context.build_dir context) path))
  ;;
end

let build_prog ~no_rebuild ~name p =
  if no_rebuild
  then
    if Path.exists p
    then Memo.return p
    else
      User_error.raise
        [ Pp.concat
            ~sep:Pp.space
            [ Pp.text "Command"
            ; User_message.command name
            ; Pp.text "isn't built yet. You need to build it first or remove the"
            ; User_message.command "--no-build"
            ; Pp.text "option."
            ]
        ]
  else
    let open Memo.O in
    let+ () = Build_system.build_file p in
    p
;;

let not_found ~dir:_ ~name =
  (* let open Memo.O in *)
  (* let+ hints =
    (* Good candidates for the "./x.exe" instead of "x.exe" error are
       executables present in the current directory. Note: we do not
       check directory targets here; even if they do indeed include a
       matching executable, they would be located in a subdirectory of
       [dir], so it's unclear if that's what the user wanted. *)
    let+ candidates =
      let+ filename_set = Build_system.files_of ~dir:(Path.build dir) in
      Filename_set.filenames filename_set
      |> Filename.Set.to_list
      |> List.filter ~f:(fun filename -> Filename.extension filename = ".exe")
      |> List.map ~f:(fun filename -> "./" ^ filename)
    in
    User_message.did_you_mean name ~candidates
  in *)
  User_error.raise
    (* ~hints *)
    [ Pp.concat
        ~sep:Pp.space
        [ Pp.text "Program"; User_message.command name; Pp.text "not found!" ]
    ]
;;

let get_path_and_build_if_necessary sctx ~dir ~name =
  let open Memo.O in
  (* TODO: Add no_rebuild? *)
  let no_rebuild = false in
  match Filename.analyze_program_name name with
  | In_path ->
    Super_context.resolve_program_memo sctx ~dir ~loc:None name
    >>= (function
     | Error (_ : Action.Prog.Not_found.t) -> not_found ~dir ~name
     | Ok p -> build_prog ~no_rebuild ~name p)
    (* TODO remove all mentions of exe *)
  | Relative_to_current_dir ->
    let path = Path.relative_to_source_in_build_or_external ~dir name in
    Build_system.file_exists path
    >>= (function
     | true -> Memo.return (Some path)
     | false ->
       if not (Filename.check_suffix name ".exe")
       then Memo.return None
       else (
         let path = Path.extend_basename path ~suffix:".exe" in
         Build_system.file_exists path
         >>| function
         | true -> Some path
         | false -> None))
    >>= (function
     | Some path -> build_prog ~no_rebuild ~name path
     | None -> not_found ~dir ~name)
  | Absolute ->
    (match
       let name = Path.of_string name in
       if Path.exists name
       then Some name
       else if not Sys.win32
       then None
       else (
         (* Remove suffix *)
         let prog = Path.extend_basename name ~suffix:Bin.exe in
         Option.some_if (Path.exists prog) prog)
     with
     | Some prog -> Memo.return prog
     | None -> not_found ~dir ~name)
;;

module Command_context = struct
  type t =
    { name : Cmd_arg.t
    ; args : Cmd_arg.t list
    ; env : Env.t Memo.t
    ; sctx : Super_context.t Memo.t
    ; get_path_and_build_if_necessary : name:string -> Path.t Memo.t
    }

  let init ~common ~context ~name ~args =
    let open Fiber.O in
    let+ setup = Import.Main.setup () in
    let open Memo.O in
    let sctx =
      let+ setup = setup in
      Import.Main.find_scontext_exn setup ~name:context
    in
    let dir =
      let+ sctx = sctx in
      let context = Dune_rules.Super_context.context sctx in
      Path.Build.relative (Context.build_dir context) (Common.prefix_target common "")
    in
    let env = Memo.bind sctx ~f:Super_context.context_env in
    let get_path_and_build_if_necessary ~name =
      let* sctx = sctx
      and+ dir = dir in
      get_path_and_build_if_necessary sctx ~name ~dir
    in
    { sctx; env; name; args; get_path_and_build_if_necessary }
  ;;

  let run_once t common config =
    Scheduler.go ~common ~config
    @@ fun () ->
    let open Fiber.O in
    let* path, args, env =
      let* { sctx; env; name; args; get_path_and_build_if_necessary } = t in
      build_exn (fun () ->
        let open Memo.O in
        let* env = env
        and* sctx = sctx in
        let expand = Cmd_arg.expand ~root:(Common.root common) ~sctx in
        let* path =
          let* name = expand name in
          get_path_and_build_if_necessary ~name
        in
        let+ args = Memo.parallel_map ~f:expand args in
        path, args, env)
    in
    let prog = Path.to_string path in
    restore_cwd_and_execve (Common.root common) prog args env
  ;;
end

let term =
  let+ builder = Common.Builder.term
  and+ context = Common.context_arg ~doc:{|Run the command in this build context.|}
  and+ command =
    Arg.(required & pos 0 (some Cmd_arg.conv) None (Arg.info [] ~docv:"Command name"))
  and+ args = Arg.(value & pos_right 0 Cmd_arg.conv [] (Arg.info [] ~docv:"ARGS")) in
  let common, config = Common.init builder in
  let command_context = Command_context.init ~common ~context ~name:command ~args in
  Command_context.run_once command_context common config
;;

let command = Cmd.v info term
