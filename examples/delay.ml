open Dream_middleware_ext.Delay

(* make delay middleware *)
let delay = make_delay ~secs:5.

let () =
  Dream.run ~port:9090 @@ delay (* apply delay middleware *)
  @@ Dream.router
       [ Dream.scope "/" [] [ Dream.get "/delay" (fun _ -> Dream.respond "") ] ]
