open Tromp

let process mode lambda =
  match Parsing.parse lambda with
  | Ok term -> (
      match mode with
      | "text" | "t" ->
          term |> Rendering.Text.render |> Rendering.Text.diagram |> print_endline;
          `Ok ()
      | _ -> `Error (false, "unknown mode supplied"))
  | Error err -> `Error (false, Parsing.parsing_error_to_string err)

open Cmdliner
open Cmdliner.Term.Syntax

let mode =
  let doc = "Choose the rendering mode" in
  Arg.(value & opt string "text" & info [ "m"; "mode" ] ~doc ~docv:"MODE")

let lambda =
  let doc = "A well-formed lambda expression" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"LAMBDA")

let tromp =
  let doc = "A tool for rendering Tromp diagrams of lambda expressions" in
  Cmd.v (Cmd.info "tromp" ~version:"0.1.0" ~doc)
  @@ Term.ret
  @@
  let+ m = mode and+ l = lambda in
  process m l

let main () = Cmd.eval tromp

let () =
  if !Sys.interactive then
    ()
  else
    exit (main ())
