let sub_str str start =
  let len = String.length str in
  String.sub str start (len - start)

let match_literal expected =
  let len = String.length expected in
  fun input ->
    if expected = String.sub input 0 len then Ok(sub_str input len, ())
    else Error(input)

let _ =
  let parse_joe = match_literal "Hello Joe!" in
  assert (Ok("", ()) = parse_joe "Hello Joe!");
  assert (Ok(" Hello Robert!", ()) = parse_joe("Hello Joe! Hello Robert!"));
  assert (Error("Hello Mike!") = parse_joe("Hello Mike!"))
