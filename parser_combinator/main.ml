let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

module Parser = struct
  open Base

  type 'a t = string -> ((string * 'a), string) Caml.result

  module Let_Syntax = struct
    let (let*) (parser:'a t) (f: 'a -> 'b t) =
      fun input -> match parser input with
        | Ok(next_input, result) ->
          let next_parser = f result in
          next_parser next_input
        | Error(input) -> Error(input)
    let (let+) (parser:'a t) (f: 'a -> 'b) =
      fun input -> match parser input with
        | Ok(next_input, result) -> Ok(next_input, f result)
        | Error(input) -> Error(input)
  end

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

  let pair parser1 parser2 =
    let open Let_Syntax in
    let* result1 = parser1 in
    let+ result2 = parser2 in
    (result1, result2)

  let left parser1 parser2 =
    let open Let_Syntax in
    let+ (result1, _) = pair parser1 parser2 in
    result1

  let right parser1 parser2 =
    let open Let_Syntax in
    let+ (_, result2) = pair parser1 parser2 in
    result2

end

let _ =
  let open Parser in
  let parse_joe = match_literal "Hello Joe!" in
  assert (Ok("", ()) = parse_joe "Hello Joe!");
  assert (Ok(" Hello Robert!", ()) = parse_joe "Hello Joe! Hello Robert!" );
  assert (Error("Hello Mike!") = parse_joe "Hello Mike!")

let _ =
  let open Parser in
  assert (Ok("", "i-am-an-identifier") = identifier "i-am-an-identifier");
  assert (Ok(" entirely an identifier", "not") = identifier "not entirely an identifier");
  assert (Error("!not at all an identifier") = identifier "!not at all an identifier")

let _ =
  let open Parser in
  let tag_opener = pair (match_literal "<") identifier in
  assert (Ok("/>", ((), "my-first-element")) = tag_opener "<my-first-element/>");
  assert (Error("oops") = tag_opener("oops"));
  assert (Error("!oops") = tag_opener("<!oops"))

let _ =
  let open Parser in
  let tag_opener = right (match_literal "<") identifier in
  assert (Ok("/>", "my-first-element") = tag_opener "<my-first-element/>");
  assert (Error("oops") = tag_opener "oops");
  assert (Error("!oops") = tag_opener "<!oops")
