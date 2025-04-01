open Lambda

module Text = struct
  open BoxDrawings

  type image = box_symbol list list
  type lambda_enriched = { term : lambda; free_spikes : string list; grid : image; width : int }

  (** Fills the smaller list with [filler] values to equalize the sizes of the two lists. *)
  let equalize_sizes filler l1 l2 =
    let cmp = List.compare_lengths l1 l2 in
    if cmp > 0 then
      (l1, l2 @ List.init (List.length l1 - List.length l2) (Fun.const (filler l2)))
    else if cmp < 0 then
      (l1 @ List.init (List.length l2 - List.length l1) (Fun.const (filler l1)), l2)
    else
      (l1, l2)

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
          equalize_sizes
            (fun im -> Vertical :: List.init (List.length (List.hd im) - 1) (Fun.const Space))
            lhs_enriched.grid rhs_enriched.grid
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
