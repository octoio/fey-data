let run () =
  Dream.run ~error_handler:Dream.debug_error_handler ~interface:"0.0.0.0" ~port:7777
  @@ Dream.logger
  @@ Dream.router
       [ (Dream.post "/validate"
          @@ fun request ->
          let%lwt body = Dream.body request in
          try
            let config = Data.Entity_j.entity_definition_internal_of_string body in
            let result =
              Data.Entity_v.validate_entity_definition_internal
                [ `Field "entity_definition" ]
                config
            in
            match result with
            | None -> Dream.respond ~status:`OK "Valid"
            | Some error ->
              Dream.respond
                ~status:`OK
                (Atdgen_runtime.Util.Validation.string_of_error error)
          with
          | Atdgen_runtime.Oj_run.Error error -> Dream.respond ~status:`OK error
          | e -> Dream.respond ~status:`OK (Printexc.to_string e))
       ]
;;
