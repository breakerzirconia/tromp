open Lambda

(** Fills the smaller list with [filler] values to equalize the sizes of the two lists. *)
let equalize_sizes l1 l2 filler =
  let cmp = List.compare_lengths l1 l2 in
  if cmp > 0 then
    (l1, l2 @ List.init (List.length l1 - List.length l2) (Fun.const (filler l2)))
  else if cmp < 0 then
    (l1 @ List.init (List.length l2 - List.length l1) (Fun.const (filler l1)), l2)
  else
    (l1, l2)

module Text = struct
  type box_symbol = Horizontal | Vertical | UpLeft | VerticalRight | HorizonralDown | VerticalHorizontal | Space

  (** Translates a [box_symbol] into its respective box drawing. *)
  let draw = function
    | Horizontal -> "\u{2500}" (* ─ *)
    | Vertical -> "\u{2502}" (* │ *)
    | UpLeft -> "\u{2518}" (* ┘ *)
    | VerticalRight -> "\u{251C}" (* ├ *)
    | HorizonralDown -> "\u{252C}" (* ┬ *)
    | VerticalHorizontal -> "\u{253C}" (* ┼ *)
    | Space -> " "

  type image = box_symbol list list
  type lambda_enriched = { term : lambda; free_spikes : string list; grid : image; width : int }

  let horizontals = [ Horizontal; HorizonralDown; VerticalHorizontal ]
  let contains_no_horizontals l = List.is_empty (List.filter (Fun.flip List.mem horizontals) l)
  let spikes = [ Vertical; VerticalHorizontal ]

  (** Generates a horizontal bar corresponding to the binder in a lambda abstraction. Free variables have spikes that
      point up, bound variables stop at their respective horizontal bars. *)
  let generate_binding top binder free_spikes =
    let rec go top binder free_spikes new_free_spikes acc =
      match top with
      | [] -> (List.rev new_free_spikes, List.rev acc)
      | u :: us -> (
          match u with
          | HorizonralDown | Horizontal | Space -> go us binder free_spikes new_free_spikes (Horizontal :: acc)
          | _ -> (
              match free_spikes with
              | spike :: remaining_spikes ->
                  if spike = binder then
                    go us binder remaining_spikes new_free_spikes (HorizonralDown :: acc)
                  else
                    go us binder remaining_spikes (spike :: new_free_spikes) (VerticalHorizontal :: acc)
              | [] -> failwith "Impossible, if there is a spike, it is a free variable that should have been tracked"))
    in
    go top binder free_spikes [] []

  (** Constructs a [lambda_enriched] from a [lambda] that contains the information about its free variables, the
      rendered grid of box drawings, and the width. *)
  let rec render lam =
    match lam with
    | Var x -> { term = lam; free_spikes = [ x ]; grid = [ [ Vertical ] ]; width = 1 }
    | App (Var x, Var y) ->
        { term = lam; free_spikes = [ x; y ]; grid = [ [ VerticalRight; Horizontal; UpLeft ] ]; width = 3 }
    | App (lhs, rhs) ->
        let lhs_enriched = render lhs in
        let rhs_enriched = render rhs in
        let lhs_equalized, rhs_equalized =
          equalize_sizes lhs_enriched.grid rhs_enriched.grid (fun im ->
              Vertical :: List.init (List.length (List.hd im) - 1) (Fun.const Space))
        in
        let fusion = List.map2 (fun l r -> l @ [ Space ] @ r) lhs_equalized rhs_equalized in
        let fusion_width = lhs_enriched.width + 1 + rhs_enriched.width in
        let connected_fusion =
          fusion
          @ [
              (VerticalRight :: List.init lhs_enriched.width (Fun.const Horizontal))
              @ [ UpLeft ]
              @ List.init (rhs_enriched.width - 1) (Fun.const Space);
            ]
        in
        {
          term = lam;
          free_spikes = lhs_enriched.free_spikes @ rhs_enriched.free_spikes;
          grid = connected_fusion;
          width = fusion_width;
        }
    | Abs (x, e) -> (
        let e_enriched = render e in
        match e_enriched.grid with
        | top :: rest ->
            let new_free_spikes, new_top = generate_binding top x e_enriched.free_spikes in
            {
              term = lam;
              free_spikes = new_free_spikes;
              grid =
                (if contains_no_horizontals top then
                   new_top :: rest
                 else
                   new_top :: e_enriched.grid);
              width = e_enriched.width;
            }
        | [] -> failwith "Impossible, any rendered lambda expression has a nonempty grid")

  (** Concatenates the grid into one string, forming a unicode Tromp diagram. *)
  let diagram lam_enr = String.concat "\n" (List.map (fun row -> String.concat "" (List.map draw row)) lam_enr.grid)
end

module ImageHybrid = struct

  (* false = empty, true = filled in *)
  type image = bool list list

  type lambda_enriched = {
    term : lambda;
    free_spikes : string list;
    grid : image;
    width : int;
    height : int;
    left_padding : bool;
    right_padding : bool;
  }

  (** Generates a horizontal bar corresponding to the binder in a lambda abstraction. Free variables have spikes that
      point up, bound variables stop at their respective horizontal bars. *)
  let generate_binding top binder free_spikes =
    let rec go top binder free_spikes new_free_spikes acc =
      match top with
      | [] -> (List.rev new_free_spikes, List.rev acc)
      | u :: us -> (
          if Bool.not u then
            go us binder free_spikes new_free_spikes (false :: acc)
          else
            match free_spikes with
            | spike :: remaining_spikes ->
                if spike = binder then
                  go us binder remaining_spikes new_free_spikes (false :: acc)
                else
                  go us binder remaining_spikes (spike :: new_free_spikes) (true :: acc)
            | [] -> failwith "Impossible, if there is a spike, it is a free variable that should have been tracked")
    in
    go top binder free_spikes [] []

  (** Constructs a [lambda_enriched] from a [lambda] that contains the information about its free variables, the
      rendered grid of block elements, the dimensions, and padding. *)
  let render lam =
    let rec go lam =
      match lam with
      | Var x ->
          {
            term = lam;
            free_spikes = [ x ];
            grid = [ [ false; true; false ] ];
            width = 3;
            height = 1;
            left_padding = true;
            right_padding = true;
          }
      | App (lhs, rhs) ->
          let lhs_enriched = go lhs in
          let rhs_enriched = go rhs in
          let lhs_equalized, rhs_equalized =
            equalize_sizes lhs_enriched.grid rhs_enriched.grid (fun im ->
                false :: true :: List.init (List.length (List.hd im) - 2) (Fun.const false))
          in
          let fusion = List.map2 (fun l r -> l @ [ false ] @ r) lhs_equalized rhs_equalized in
          let connected_fusion =
            fusion
            @ [
                (false :: List.init (lhs_enriched.width + 2) (Fun.const true))
                @ List.init (rhs_enriched.width - 2) (Fun.const false);
                false :: true :: List.init (lhs_enriched.width + rhs_enriched.width - 1) (Fun.const false);
              ]
          in
          {
            term = lam;
            free_spikes = lhs_enriched.free_spikes @ rhs_enriched.free_spikes;
            grid = connected_fusion;
            width = lhs_enriched.width + 1 + rhs_enriched.width;
            height = Int.max lhs_enriched.height rhs_enriched.height + 2;
            left_padding = lhs_enriched.left_padding;
            right_padding = rhs_enriched.left_padding;
          }
      | Abs (x, e) -> (
          let e_enriched = go e in
          match e_enriched.grid with
          | top :: _ ->
              let new_free_spikes, new_top = generate_binding top x e_enriched.free_spikes in
              {
                term = lam;
                free_spikes = new_free_spikes;
                grid = new_top :: List.init (List.length top) (Fun.const true) :: e_enriched.grid;
                width = e_enriched.width;
                height = e_enriched.height + 2;
                left_padding = false;
                right_padding = false;
              }
          | [] -> failwith "Impossible, any rendered lambda expression has a nonempty grid")
    in
    let initially_rendered = go lam in
    let grid_ref = ref initially_rendered.grid in
    let left_padding_delta = ref 0 in
    let right_padding_delta = ref 0 in
    let height_delta = ref 0 in
    if initially_rendered.left_padding then (
      left_padding_delta := !left_padding_delta - 1;
      grid_ref := List.map List.tl !grid_ref);
    if initially_rendered.right_padding then (
      right_padding_delta := !right_padding_delta - 1;
      grid_ref := List.map Util.ini !grid_ref);
    if List.is_empty initially_rendered.free_spikes then (
      grid_ref := List.tl !grid_ref;
      height_delta := !height_delta - 1);
    {
      term = lam;
      free_spikes = initially_rendered.free_spikes;
      grid = !grid_ref;
      width = initially_rendered.width + !left_padding_delta + !right_padding_delta;
      height = initially_rendered.height + !height_delta;
      left_padding = false;
      right_padding = false;
    }

  (** Concatenates the grid into one string, forming a unicode Tromp diagram. *)
  let diagram lam_enr =
    String.concat "\n"
      (List.map
        (fun row -> String.concat "" (List.map (fun p -> if p then "\u{2588}" else " ") row))
        lam_enr.grid)
end
