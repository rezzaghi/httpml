open Opium

let hello _req = Response.of_plain_text "Hello World" |> Lwt.return

type person = { name : string; age: int}

let map_response person = match person.name with
| "lucas" -> if person.age < 12 then "Access Denied\n" else "Permission granted\n" 
| _ -> "Unknown\n"

let handler req =
  let open Yojson.Safe.Util in
  let%lwt content = Body.to_string req.Request.body in
  let json = Yojson.Safe.from_string content in
  let person_json = {name = member "name" json |> to_string; age = member "age" json |> to_int} in
  map_response person_json |> Response.of_plain_text  |> Lwt.return

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