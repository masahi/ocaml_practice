let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

module Parser = struct
  open Base

  let match_literal expected =
    let len = String.length expected in
    fun input ->
      if String.equal expected (String.sub input ~pos:0 ~len) then Ok(String.subo input ~pos:len, ())
      else Error(input)

  let identifier input =
    let char_list = String.to_list input in
    match char_list with
    | [] -> Error(input)
    | hd :: tl ->
      if Char.is_alpha hd then
        let consumed =
          hd :: List.take_while tl ~f:(fun c -> Char.is_alphanum c || Char.equal c '-')
        in
        Ok(String.subo input ~pos:(List.length consumed), string_of_chars consumed)
      else Error(input)

end


let _ =
  let open Parser in
  let parse_joe = match_literal "Hello Joe!" in
  assert (Ok("", ()) = parse_joe "Hello Joe!");
  assert (Ok(" Hello Robert!", ()) = parse_joe("Hello Joe! Hello Robert!"));
  assert (Error("Hello Mike!") = parse_joe("Hello Mike!"))

let _ =
  let open Parser in
  assert (Ok("", "i-am-an-identifier") = identifier("i-am-an-identifier"));
  assert (Ok(" entirely an identifier", "not") = identifier("not entirely an identifier"));
  assert (Error("!not at all an identifier") = identifier("!not at all an identifier"))
