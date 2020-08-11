open Codelib

let test compile =
  begin
    let a = compile "a" in
    assert (a "a");
    assert (not (a "b"));
    assert (not (a ""));
    assert (not (a "aa"));

    let ab = compile "ab" in
    assert (not (ab "a"));
    assert (not (ab "b"));
    assert (not (ab ""));
    assert (ab "ab");

    let aab = compile "aab" in
    assert (not (aab "a"));
    assert (not (aab "b"));
    assert (not (aab ""));
    assert (aab "aab");
    assert (not (aab "aaab"));

    let aorb = compile "a|b" in
    assert (aorb "a");
    assert (aorb "b");
    assert (not (aorb "c"));
    assert (not (aorb "ab"));
    assert (not (aorb ""));

    let opta = compile "a?" in
    assert (opta "a");
    assert (opta "");
    assert (not (opta "b"));
    assert (not (opta "aa"));

    let astar = compile "a*" in
    assert (astar "");
    assert (astar "a");
    assert (astar "aaaa");
    assert (not (astar "aaaab"));
    assert (not (astar "a*"));

    let abstar = compile "(ab)*" in
    assert (abstar "");
    assert (abstar "ab");
    assert (abstar "ababab");
    assert (not (abstar "ababa"));

    let aorbstar = compile "(a|b)*" in
    assert (aorbstar "");
    assert (aorbstar "a");
    assert (aorbstar "b");
    assert (aorbstar "babababa");
    assert (not (aorbstar "bababacba"));

    Printf.printf "compiling a.*b\n";
    let adotstarb = compile "a.*b" in
    assert (adotstarb "ab");
    assert (adotstarb "aasdfasdfasdb");
    assert (not (adotstarb "aasdfasdfasdbc"));
    assert (not (adotstarb "caasdfasdfasdb"));
  end

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let nfa_unstaged s =
  let accept = Nfa.accept (Regex.compile (Regex.parse s)) in
  fun s -> accept (explode s)

let nfa_staged s =
  let cde = (Nfa_staged.accept (Regex.compile (Regex.parse s))) in
  print_code Format.std_formatter cde; print_newline ();
  let f = Runnative.run cde in
  fun str -> (f (explode str))

let () =
  begin
    print_string "testing unstaged NFA..."; flush stdout;
    test nfa_unstaged;
    print_endline "OK!";

    print_string "testing staged NFA..."; flush stdout;
    test nfa_staged;
    print_endline "OK!";
  end
