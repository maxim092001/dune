open Import

type t =
  { deps : Dep_conf.t Bindings.t
  ; name : Loc.t * string
  ; loc : Loc.t
  }

include Stanza.S with type t := t

val decode : t Dune_lang.Decoder.t
