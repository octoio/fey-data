(* The values both the checks of this pipeline and the game read. Written here once: the Rust
   emitter puts them in fey_data's generated/constants.rs and fey-rs uses them from there. *)

(* How far scenery (and any light the client places) keeps from an anchor: a spawn point, a
   portal, the quest board or the middle of a zone. The footprint of a lamp (a pack model, 0.45)
   plus an adventurer's (0.4) plus the width of one more body (0.8) so a hero can stand there and
   turn without the lamp in them: 1.65, rounded up. *)
let decor_clearance = 2.0
