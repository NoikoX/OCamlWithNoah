let rec dupl l = match l with | [] -> [] |
h::t -> h::h::dupl t;;