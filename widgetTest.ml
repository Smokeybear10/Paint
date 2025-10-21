;; open Assert
;; open Widget

(* Testing code for the widget library. These unit tests make sure
   that the non-visual behavior of the widgets is correctly
   implemented.  *)

(* We give you some unit tests that already pass (for the widgets that
   we have implemented) and some tests that will only pass
   once you have completed tasks 1 and 5.  *)

(* Create a 'dummy' event: a mouse click at location (5,5) *)
let gc = Gctx.top_level
let click55 = Gctx.make_test_event Gctx.MouseDown (5,5)

(* These first tests should already pass. But you should take a look at 
   how they are implemented so that you can understand how the widget library 
  works and write your own test cases. *)

;; print_endline "--------- Tests for provided code  --------------------"

(* Label widget: the string in the label is the same as the input *)
;; run_test "label creation" (fun () ->
    let _, lc1 = label "l1" in
    lc1.get_label () = "l1")

(* Label widget: the string of the label can be updated *)
;; run_test "label string change" (fun () ->
    let _, lc1 = label "l1" in
    lc1.set_label "l2";
    lc1.get_label () = "l2")

(* Label widget: labels do not all share the same string *)
;; run_test "label local space" (fun () ->
    let _, lc1 = label "l1" in
    let _, lc2 = label "l2" in
    lc1.set_label "l3";
    lc2.get_label () = "l2")

(* Space widget *)

;; run_test "space size" (fun () ->
    let w1 = space (10,10) in
    w1.size () = (10,10))

(* Border widget *)

;; run_test "border size" (fun () ->
    let w1 = border (space (10,10)) in
    w1.size () = (14,14))

(* Hpair widget *)

;; run_test "hpair size" (fun () ->
    let w1 = space (10,20) in
    let w2 = space (30,50) in
    let w = hpair w1 w2 in
    w.size() = (40, 50))

(* Notifiers and event handling for above *)

(* The notifier's size is inherited from the contained widget *)
;; run_test "notifier size" (fun () ->
    let w, _ = notifier (space (10,20)) in
    w.size () = (10,20))

(* the default handle function for the notifier does nothing *)
;; run_test "notifier handle (nothing)" (fun () ->
    let w, nc = notifier (space (10,10)) in
    w.handle gc click55;
    true)

(* add an event listener that changes state on mouseclicks. When the widget is 
   sent a mouseclick event, the state actually changes. *)
;; run_test "notifier handle (mouse down)" (fun () ->
    let w, nc = notifier (space (10,10)) in
    let state = {contents = false} in
    nc.add_event_listener
      (mouseclick_listener
        (fun () -> state.contents <- true));
    w.handle gc click55;
    state.contents)

(* The border widget propagates events to the contained widget (a notifier 
   in this case, which changes the contents of some state. *)
;; run_test "border handle (mouseclick)" (fun () ->
    let w1, nc = notifier (space (10,10)) in
    let w = border w1 in
    let state = {contents = false} in
    nc.add_event_listener
      (mouseclick_listener
         (fun () -> state.contents <- true));
    w.handle gc click55;
    state.contents)

(* The hpair widget propagates events to the left widget.
   The click is at location 5,5  (ascii art not to scale)

        0       10     20     30
      0 +--------+-------------+
        | x      |             |  
        |        |             | 
     20 +--------+             |
                 |             |
     30          +-------------+  
*)
;; run_test "hpair handle click in left widget" (fun () ->
    let w1, nc = notifier (space (10,20)) in
    let w2 = space (20,30) in
    let w = hpair w1 w2 in
    let state = {contents = false} in
    nc.add_event_listener
      (mouseclick_listener
         (fun () -> state.contents <- true));
    w.handle gc click55;
    state.contents)

(* The hpair widget propagates events to the right widget.
   The click is at 15,15
   
        0       10     20     30
      0 +--------+-------------+
        |        |             |  
        |        | x           | 
     20 +--------+             |
                 |             |
     30          +-------------+  
*)
;; run_test "hpair handle click in right widget" (fun () ->
    let w1 = space (10,20) in
    let w2, nc = notifier (space (20,30)) in
    let w = hpair w1 w2 in
    let state = {contents = false} in
    nc.add_event_listener
      (mouseclick_listener
         (fun () -> state.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (15,15));
    state.contents)

(* click as above: make sure the left widget listener *not* called *)
;; run_test "hpair handle only one click" (fun () ->
    let w1, nc1 = notifier (space (10,20)) in
    let w2, nc2 = notifier (space (20,30)) in
    let w = hpair w1 w2 in
    let state1 = {contents = false} in
    let state2 = {contents = false} in
    nc1.add_event_listener
      (mouseclick_listener (fun () -> state1.contents <- true));
    nc2.add_event_listener
      (mouseclick_listener (fun () -> state2.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (15,15));
    state2.contents && not state1.contents)

(* The hpair widget ignores clicks in the dead space.
   The click is at 5,25

        0       10     20     30
      0 -----------------------+
        |        |             |
        |        |             |
     20 ---------+             |
         x       |             |
     30          +-------------+
*)
;; run_test "hpair handle dead space" (fun () ->
    let w1, nc1 = notifier (space (10,20)) in
    let w2, nc2 = notifier (space (20,30)) in
    let w = hpair w1 w2 in
    let state1 = {contents = false} in
    let state2 = {contents = false} in
    nc1.add_event_listener
      (mouseclick_listener (fun () -> state1.contents <- true));
    nc2.add_event_listener
      (mouseclick_listener (fun () -> state2.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (5,25));
    not state1.contents && not state1.contents)

(* canvas *)

(* Canvas widget can handle clicks within its boundaries *)
;; run_test "canvas handle" (fun () ->
    let state = {contents = false} in
    let cw, nc = canvas (10,10) (fun g -> ()) in
    nc.add_event_listener
      (mouseclick_listener (fun () -> state.contents <- true));
    cw.handle gc click55;
    state.contents)

(* Canvas widgit size is correct: the canvas has a 2-pixel wide
   border on all four sides.  *)
;; run_test "canvas size" (fun () ->
    let cw, nc = canvas (10,10) (fun g -> ()) in
    cw.size () = (14,14))

(* Below, we provide some tests for the widgets that you are required to write
 * as part of this assignment. The autograder will run these tests. *)



;; print_endline "--------- Task 1: vpair --------------------"
(* [vpair] tests *)


(*  Test the size method for vpair: second widget is wider

      0     10    20    30
      +------+
      |      |
   20 +------+-----+-----+
      |                  |
      |                  |
      |                  |
      |                  |
   70 +------+-----+-----+
*)
;; run_test "vpair size" (fun () ->
    let w1 = space (10,20) in
    let w2 = space (30,50) in
    let w = vpair w1 w2 in
    w.size() = (30, 70))

(* Test the size methof for vpair: fist widget is wider

       0     10    20    30     40    50
    0 +------+-----+-----+-----+------+
      |                               |
      |                               |
   30 +------+-----+-----+-----+------+
      |      |
      |      |
   50 +------+
*)
;; run_test "vpair wide" (fun () ->
    let w1 = space (50,30) in
    let w2 = space (10,20) in
    let w = vpair w1 w2 in
    w.size() = (50, 50))

(*    0     10    20
      +------+
      |x     |
   20 +------+-----+
      |            |
      |            |
      |            |
   50 +------+-----+
*)
;; run_test "vpair handle click in top widget" (fun () ->
    let w1, nc = notifier (space (10,20)) in
    let w2 = space (20,30) in
    let w = vpair w1 w2 in
    let state = {contents = false} in
    nc.add_event_listener (mouseclick_listener (fun () -> state.contents <- true));
    w.handle gc click55;
    state.contents)

(*    0     10    20
      +------+
      |      |
   20 +------+-----+
      |            |
      |            |
      |x           |
   50 +------+-----+
*)
;; run_test "vpair handle click in bottom widget" (fun () ->
    let w1 = space (10,20) in
    let w2, nc = notifier (space (20,30)) in
    let w = vpair w1 w2 in
    let state = {contents = false} in
    nc.add_event_listener
      (mouseclick_listener
         (fun () -> state.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (5,40));
    state.contents)

(* As above, but make sure that only the second listener of the 
   vpair runs *)
;; run_test "vpair handle only one click" (fun () ->
    let w1, nc1 = notifier (space (10,20)) in
    let w2, nc2 = notifier (space (20,30)) in
    let w = vpair w1 w2 in
    let state1 = {contents = false} in
    let state2 = {contents = false} in
    nc1.add_event_listener
      (mouseclick_listener (fun () -> state1.contents <- true));
    nc2.add_event_listener
      (mouseclick_listener (fun () -> state2.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (5,40));
    state2.contents && not state1.contents)

(*    0     10    20
      +------+      
      |      |   x
   20 +------+-----+
      |            |
      |            |
      |            |
   30 +------+-----+
*)
;; run_test "vpair handle dead space" (fun () ->
    let w1, nc1 = notifier (space (10,20)) in
    let w2, nc2 = notifier (space (20,30)) in
    let w = vpair w1 w2 in
    let state1 = {contents = false} in
    let state2 = {contents = false} in
    nc1.add_event_listener
      (mouseclick_listener (fun () -> state1.contents <- true));
    nc2.add_event_listener
      (mouseclick_listener (fun () -> state2.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (15,5));
    not state1.contents && not state1.contents)

(* [list_layout] tests *)

;; print_endline "--------- Task 1: list_layout ------------------"



;; run_test "hlist size empty" (fun () ->
    let w = hlist [] in
    w.size () = (0,0))

(*

      0       10     20     40
    0 +--------+-------------+
      |        |             |  
      |        |             | 
   20 +--------+             |
               |             |
   50          +-------------+  

*)

;; run_test "hlist size nonempty" (fun () ->
    let w1 = space (10,20) in
    let w2 = space (30,50) in
    let w = hlist [w1; w2] in
    w.size() = (40, 50))


;; run_test "vlist size empty" (fun () ->
    let w = vlist [] in
    w.size () = (0,0))

(*    0     10    30
      +------+      
      |      |   
   20 +------+-----+
      |            |
      |            |
      |            |
   70 +------+-----+
*)

;; run_test "vlist size nonempty" (fun () ->
    let w1 = space (10,20) in
    let w2 = space (30,50) in
    let w = vlist [w1; w2] in
    w.size() = (30, 70))


(* Here is a good place to add test cases for the handle operations of hlist and
   vlist. You should make sure that hlist and vlist work correctly before moving 
   on to the next step of the homework. If you have bugs in this portion they are
   very difficult to figure out later! *)

;; print_endline "--------- Task 5: checkbox ---------------"


(* [make_controller] tests *)
;; run_test "make_controller get_value returns init value" (fun () ->
    let vc = make_controller 1 in
    vc.get_value () = 1)

;; run_test "make_controller get_value returns correct value after change"
     (fun () ->
       let vc = make_controller 1 in
       let init = vc.get_value () in
       vc.change_value 2;
       init = 1 && vc.get_value () = 2)

;; run_test "make_controller change_listeners are triggered on change"
     (fun () ->
      let vc = make_controller 1 in
      let t = {contents = 0} in
      vc.add_change_listener (fun v -> t.contents <- v);
      vc.change_value 2;
      t.contents = 2)

(* [checkbox] tests *)
;; run_test "checkbox init true" (fun () ->
    let w, cc = checkbox true "checkbox" in
    cc.get_value ())

;; run_test "checkbox init false" (fun () ->
    let w, cc = checkbox false "checkbox" in
    not (cc.get_value ()))

;; run_test "checkbox click" (fun () ->
    let w, cc = checkbox false "checkbox" in
    w.handle gc click55;
    cc.get_value())

;; run_test "checkbox click click" (fun () ->
    let w, cc = checkbox false "checkbox" in
    w.handle gc click55;
    w.handle gc click55;
    not (cc.get_value()))

;; run_test "checkbox click click click" (fun () ->
    let w, cc = checkbox false "checkbox" in
    w.handle gc click55;
    w.handle gc click55;
    w.handle gc click55;
    (cc.get_value()))

;; run_test "checkbox listener" (fun () ->
    let w, cc = checkbox false "checkbox" in
    let state = {contents = false} in
    cc.add_change_listener (fun b -> state.contents <- b);
    w.handle gc click55;
    state.contents)

;; print_endline "--------- Task 6: your own widget ---------------"

(* Here is a place for you to add test cases for your own widget. There
   is no fixed number of test cases that you need to write for the grading 
   rubric, but creating your own tests is a good way to be sure that you 
   have implemented the assignment correctly! (Or debug it if you have not.)
*)


