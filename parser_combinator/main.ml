module Parser = struct
  open Base

  let match_literal expected =
    let len = String.length expected in
    fun input ->
      if String.equal expected (String.sub input ~pos:0 ~len) then Ok(String.subo input ~pos:len, ())
      else Error(input)
end


let _ =
  let open Parser in
  let parse_joe = match_literal "Hello Joe!" in
  assert (Ok("", ()) = parse_joe "Hello Joe!");
  assert (Ok(" Hello Robert!", ()) = parse_joe("Hello Joe! Hello Robert!"));
  assert (Error("Hello Mike!") = parse_joe("Hello Mike!"))
