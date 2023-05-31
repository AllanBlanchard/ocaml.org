open Ocamlorg.Import

type member = { name : string; github : string; role : string }
[@@deriving yaml, show { with_path = false }]

type contact = { name : string; link : string; icon : string }
[@@deriving yaml, show { with_path = false }]

type team = {
  id : string;
  name : string;
  description : string;
  contacts : contact list;
  members : member list; [@default []]
  subteams : team list; [@default []]
}
[@@deriving yaml, show { with_path = false }]

type metadata = {
  teams : team list;
  working_groups : team list; [@key "working-groups"]
}
[@@deriving yaml]

type t = { teams : team list; working_groups : team list }
[@@deriving stable_record ~version:metadata, show { with_path = false }]

let decode s = Result.map of_metadata (metadata_of_yaml s)

let all () =
  let file = "governance.yml" in
  let result =
    let ( let* ) = Result.bind in
    let* yaml = Utils.yaml_file file in
    decode yaml
  in
  result
  |> Result.map_error (function `Msg err -> `Msg (file ^ ": " ^ err))
  |> Result.get_ok ~error:(fun (`Msg msg) -> Exn.Decode_error msg)

let template () =
  let t = all () in
  Format.asprintf
    {|
type member = { name : string; github : string; role : string }

type contact = { name : string; link : string; icon : string }

type team = {
  id : string;
  name : string;
  description : string;
  contacts : contact list;
  members : member list;
  subteams : team list;
}

let teams = %a

let working_groups = %a
|}
    (Fmt.brackets (Fmt.list pp_team ~sep:Fmt.semi))
    t.teams
    (Fmt.brackets (Fmt.list pp_team ~sep:Fmt.semi))
    t.working_groups
