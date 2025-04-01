type box_symbol = Horizontal | Vertical | UpLeft | VerticalRight | HorizonralDown | VerticalHorizontal | Space

(** Translates a [box_symbol] into its respective unicode code point. *)
let draw = function
  | Horizontal -> "\u{2500}" (* ─ *)
  | Vertical -> "\u{2502}" (* │ *)
  | UpLeft -> "\u{2518}" (* ┘ *)
  | VerticalRight -> "\u{251C}" (* ├ *)
  | HorizonralDown -> "\u{252C}" (* ┬ *)
  | VerticalHorizontal -> "\u{253C}" (* ┼ *)
  | Space -> " "
