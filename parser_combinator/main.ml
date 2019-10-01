(* Ported from rust version in https://bodil.lol/parser-combinators/ *)

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
     if (String.length input) >= len && String.equal expected (String.sub input ~pos:0 ~len) then Ok(String.subo input ~pos:len, ())
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

  let zero_or_more parser =
    let rec consume input = match parser input with
      | Ok(next_input, first_item) ->
        let (next_input, items) = consume next_input in
        next_input, (first_item :: items)
      | _ -> input, []
    in
    fun input -> Ok(consume input)

  let one_or_more parser =
    let open Let_Syntax in
    let+ (first_item, items) = pair parser (zero_or_more parser) in
    first_item :: items

  let any_char input =
    match String.to_list input with
    | [] -> Error(input)
    | hd :: tl -> Ok(string_of_chars tl, hd)

  let pred parser predicate =
    fun input -> match parser input with
      | Ok(next_input, item) when predicate item -> Ok(next_input, item)
      | _ -> Error(input)

  let whitespace_char () =
    pred any_char (fun c -> Char.is_whitespace c)

  let space0 () =
    zero_or_more (whitespace_char ())

  let space1 () =
    one_or_more (whitespace_char ())

  let quoted_string () =
    let open Let_Syntax in
    let+ chars =
      right (match_literal "\"") (left
                                    (zero_or_more (pred any_char (fun c -> not (Char.equal c '"'))))
                                    (match_literal "\""))
    in
    string_of_chars chars

  let attribute_pair () =
    pair identifier (right (match_literal "=") (quoted_string ()))

  let attributes () =
    zero_or_more (right (space1 ()) (attribute_pair ()))

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

let _ =
  let open Parser in
  let parser = zero_or_more (match_literal "ha") in
  assert (Ok("", [(); (); ()]) = parser "hahaha");
  assert (Ok("ahah", []) = parser "ahah");
  assert (Ok("", []) = parser "")

let _ =
  let open Parser in
  let parser = one_or_more (match_literal "ha") in
  assert (Ok("", [(); (); ()]) = parser "hahaha");
  assert (Error("ahah") = parser "ahah");
  assert (Error("") = parser "")

let _ =
  let open Parser in
  let parser = pred any_char (fun c -> c = 'o') in
  assert (Ok("mg", 'o') = parser "omg");
  assert (Error("lol") = parser "lol")

let _ =
  let open Parser in
  assert (Ok("", "Hello Joe!") =  (quoted_string ()) "\"Hello Joe!\"")

let _ =
  let open Parser in
  assert (Ok("", [("one", "1"); ("two", "2")]) = (attributes ()) " one=\"1\" two=\"2\"")
