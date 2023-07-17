open Opium

let hello _req = Response.of_plain_text "Hello World" |> Lwt.return

type person = { name : string; age: int}

let name_play person = match person.name with
| "lucas" -> if person.age < 10 then "oi crianca" else "oi adulto\n" 
| _ -> "seila quem eé vc\n"

let handler req =
  let open Yojson.Safe.Util in
  let%lwt content = Body.to_string req.Request.body in
  let json = Yojson.Safe.from_string content in
  let person_json = {name = member "name" json |> to_string; age = member "age" json |> to_int} in
  Response.of_plain_text (name_play person_json) |> Lwt.return

let greet req =
  let name = Router.param req "name" in
  Printf.sprintf "Hello, %s" name |> Response.of_plain_text |> Lwt.return

let () =
  App.empty
  |> App.port 8080
  |> App.post "/" handler
  |> App.get "/" hello
  |> App.get "/greet/:name" greet
  |> App.run_command
  |> ignore