type token = A | B
type state = S | T | U
type ('a, 'b) automation = {
  finals: 'b list;
  trans: ('b * ('a * 'b) list) list
}
