open Dream

let handle_get _request =
  response @@ json "{\"message\": \"Hello from GET!\"}"

let handle_post request =
  let%lwt body = body request in
  response @@ json (Printf.sprintf "{\"received\": %s}" body)

let () =
  run ~port:3000
  @@ router [
    get "/hello" handle_get;
    post "/echo" handle_post;
  ]

