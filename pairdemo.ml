(* Demo of debugging widget *)

;; open Widget
;; open Gctx



let (b1, b1c, b1n) = button "Click here"
let (b2, b2c, b2n) = button "And here"

;; b1n.add_event_listener
     (mouseclick_listener
        (fun () -> b2c.set_label "Thank you!"))
;; b2n.add_event_listener
     (mouseclick_listener
        (fun () -> b1c.set_label "Thank you!"))

let w = debug (hpair (debug b1) (debug (debug b2)))

(** Run the event loop to process user events. *)
;; Eventloop.run w
