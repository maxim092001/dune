open Import
open Dune_lang.Decoder

type t =
  { deps : Dep_conf.t Bindings.t
  ; name : Loc.t * string
  ; loc : Loc.t
  ; action : Loc.t * Dune_lang.Action.t
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let decode =
  fields
    (let* deps = field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty in
     String_with_vars.add_user_vars_to_decoding_env
       (Bindings.var_names deps)
       (let+ loc = loc
        and+ name = field "name" (located string)
        and+ action = field "action" (located Dune_lang.Action.decode_dune_file) in
        { name; loc; deps; action }))
;;
