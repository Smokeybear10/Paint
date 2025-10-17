(** A library of widgets for building GUIs. *)

(********************)
(** The widget type *)
(********************)

(** A widget is an object that provides three services:
    - it can repaint itself (given an appropriate graphics context)
    - it can handle events
    - it knows its dimensions  *)
type widget = {
  repaint: Gctx.gctx -> unit;
  handle: Gctx.gctx -> Gctx.event -> unit;
  size: unit -> Gctx.dimension
}

(************************)
(**   Layout Widgets    *)
(************************)

(** A simple widget that just occupies space *)
let space (p: Gctx.dimension) : widget = {
  repaint = (fun _ -> ());
  handle = (fun _ _ -> ());
  size = (fun _ -> p);
}


(* TODO: You should finish implementing border in Task 0 *)

(** A widget that adds a one-pixel border to an existing widget *)
let border (w: widget) : widget = {
  repaint = (fun (g: Gctx.gctx) ->
    let (width, height) = w.size () in
    let x = width + 3 in    (* not "+ 4" because we start at 0! *)
    let y = height + 3 in
    Gctx.draw_rect g (0, 0) (x, y);
    let g = Gctx.translate g (2,2) in
    w.repaint g);
  handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
    w.handle (Gctx.translate g (2,2)) e);
  size = (fun () ->
    let (width, height) = w.size () in
    (width + 4, height + 4));
}

(* A helper function that determines whether a given event is within a
   region of a widget whose upper-left hand corner is (0,0) with width
   w and height h.  *)
let event_within (g: Gctx.gctx) (e: Gctx.event)
    ((w, h): Gctx.dimension) : bool =
  let (mouse_x, mouse_y) = Gctx.event_pos e g in
  mouse_x >= 0 && mouse_x < w && mouse_y >= 0 && mouse_y < h

(** The hpair widget lays out two widgets horizontally, aligned at
   their top edges. *)
let hpair (w1:widget) (w2:widget) : widget = {
  repaint = (fun (g: Gctx.gctx) ->
      let (x1, _) = w1.size () in
      w1.repaint g;
      w2.repaint (Gctx.translate g (x1,0)));
  handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
      if event_within g e (w1.size ())
      then w1.handle g e
      else
        let g2 = (Gctx.translate g (fst (w1.size ()), 0)) in
        if event_within g2 e (w2.size ())
        then w2.handle g2 e
        else ());
   size = (fun () ->
      let (x1, y1) = w1.size () in
      let (x2, y2) = w2.size () in
      (x1 + x2, max y1 y2));
}

(** The vpair widget lays out two widgets vertically, aligned at their
    left edges.

   TODO: You will implement vpair in Task 1. *)
let vpair (w1: widget) (w2: widget) : widget = {
  repaint = (fun (g: Gctx.gctx) ->
      let (_, y1) = w1.size () in
      w1.repaint g;
      w2.repaint (Gctx.translate g (0,y1)));
  handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
      if event_within g e (w1.size ())
      then w1.handle g e
      else
        let g2 = (Gctx.translate g (0, snd (w1.size ()))) in
        if event_within g2 e (w2.size ())
        then w2.handle g2 e
        else ());
   size = (fun () ->
      let (x1, y1) = w1.size () in
      let (x2, y2) = w2.size () in
      (max x1 x2, y1 + y2));
}

(* TIP: the OCaml List module provides a function fold_right
   (List.fold_right) that behaves like the "fold" function we've seen
   on previous homeworks except that it takes its arguments in a
   different order.

   Also, if you look at the List interface, you will see that there is
   a fold_left function. You may want to think about what this does,
   and how it's different from the fold you're used to.  *)

(* TODO: You will implement list_layout in Task 1. *)
let list_layout (pair: widget -> widget -> widget)
         (ws: widget list) : widget =
  List.fold_right pair ws (space (0,0))

let hlist (ws: widget list) : widget = list_layout hpair ws
let vlist (ws: widget list) : widget = list_layout vpair ws


(*****************************)
(**       Label Widgets      *)
(*****************************)

(* Throughout the paint program, we will find the need to associate some value
   with a widget, and also to provide a way to update that value. The simplest
   example of this is a label widget, where the value we're dealing with is a
   string (which is displayed by the label).

   Because both the widget and the label_controller share the same, mutable
   value, the constructor must create both together.  *)

(** A record of functions that allows us to read and write the string
    associated with a label. *)
type label_controller = {
  get_label : unit -> string;
  set_label : string -> unit
}

(** Construct a label widget and its controller. *)
let label (s: string) : widget * label_controller =
  let r = {contents = s} in {
    repaint = (fun (g: Gctx.gctx) ->
        Gctx.draw_string g (0,0) r.contents);
    handle = (fun _ _ -> ());
    size = (fun () -> Gctx.text_size r.contents)
  },{
    get_label = (fun () -> r.contents);
    set_label = (fun (s: string) -> r.contents <- s);
  }

(*****************************************)
(**           Debugging support          *)
(*****************************************)

(** A wrapper widget for debugging other widgets that shows events as they 
    happen. 
    
    To use the widget to debug a widget called w, replace w with 
    (debug w) where you use the widget in your GUI. When running the GUI, 
    a bordered red rectangle will appear around the child widget, and the 
    most recent Gctx.event type and position are displayed. pairdemo.ml 
    gives a simple demonstration of how the debug widget can be used.
*)

let debug (w: widget) : widget =
  let current_label = {contents = ""} in
  let current_size () = 
    let (wwidth, wheight) = w.size () in
    let (labelwidth, labelheight) = Gctx.text_size current_label.contents in
    let outerx = (max (wwidth+3) (labelwidth+5)) + 11 in  
    let outery = labelheight + wheight + 13 in
    (outerx, outery) in
  let innerg g = Gctx.translate g (7,7) in
  {
  repaint = (fun (g: Gctx.gctx) ->
    let (wwidth, wheight) = w.size () in
    let (outerx, outery) = current_size () in
    let dark = {Gctx.r=200;g=0;b=0} in 
    let light = {Gctx.r=255;g=235;b=235} in
    let gborder = Gctx.with_color g dark in
    let ginnerpane = Gctx.with_color (Gctx.translate g (1,1)) light in
    let gw = innerg g in
    let gwbg = Gctx.with_color gw Gctx.white in
    let ginnerborder = Gctx.with_color gw dark in
    Gctx.draw_line gborder (0,0) (outerx-1,0);
    Gctx.draw_line gborder (0,0) (0, outery-1);
    Gctx.draw_line gborder (outerx-1,0) (outerx-1, outery-1);
    Gctx.draw_line gborder (0, outery-1) (outerx-1, outery-1);
    Gctx.fill_rect ginnerpane (0,0) (outerx - 3, outery - 3);
    Gctx.fill_rect gwbg (0,0) (wwidth, wheight);
    Gctx.draw_line ginnerborder (-1,-1) (wwidth, -1);
    Gctx.draw_line ginnerborder (-1,-1) (-1, wheight);
    Gctx.draw_line ginnerborder (wwidth,-1) (wwidth, wheight);
    Gctx.draw_line ginnerborder (-1, wheight) (wwidth, wheight);
    Gctx.draw_string g (3,wheight+9) current_label.contents;
    w.repaint gw);
  handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
    let (x,y) = Gctx.event_pos e (innerg g) in
    let (width,height) = w.size () in
    if x >= 0 && y >= 0 && x < width && y < height then begin
      current_label.contents <- Gctx.string_of_event (innerg g) e
    end;
    w.handle (innerg g)  e);
  size = (fun () -> current_size ());
}



(*****************************************)
(**    Event Listeners and Notifiers     *)
(*****************************************)

(** An event listener processes events as they "flow" through the widget
    hierarchy.

    The file notifierdemo.ml in the GUI demo project gives a longer
    explanation of what notifiers and event_listeners are. *)

type event_listener = Gctx.gctx -> Gctx.event -> unit

(* Below we define two special forms of event_listeners. *)

(** Performs an action upon receiving a mouse click. *)
let mouseclick_listener (action: unit -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    if Gctx.event_type e = Gctx.MouseDown then action ()

(** Performs an action upon receiving a key press. *)
let key_listener (action: char -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    begin match Gctx.event_type e with
      | Gctx.KeyPress key -> action key
      | _ -> ()
    end

(** A notifier_controller is associated with a notifier widget.  It
   allows the program to add event listeners to the notifier. *)
type notifier_controller = {
  add_event_listener: event_listener -> unit
}

(** A notifier widget is a widget "wrapper" that doesn't take up any
   extra screen space -- it extends an existing widget with the
   ability to react to events. It maintains a list of of
   event_listeners that eavesdrop on the events propagated through the
   notifier widget.

   When an event comes in to the notifier, it is passed to each
   event_listener in turn, and then passed to the child widget. *)
let notifier (w: widget) : widget * notifier_controller =
  let listeners = {contents = []} in {
    repaint = w.repaint;
    handle =
      (fun (g: Gctx.gctx) (e: Gctx.event) ->
         List.iter (fun h -> h g e) listeners.contents;
         w.handle g e);
    size = w.size
  },{
    add_event_listener =
      fun (newl: event_listener) ->
        listeners.contents <- newl :: listeners.contents
  }


(*****************************************)
(**               Button                 *)
(*****************************************)

(** A button has a string, which can be controlled by the
   corresponding label_controller, and notifier, which can be
   controlled by the notifier_controller to add listeners (e.g., a
   mouseclick_listener) that will perform an action when the button is
   pressed. *)
let button (s: string)
         : widget * label_controller * notifier_controller =
  let (w, lc) = label s in
  let (w', nc) = notifier w in
  (w', lc, nc)


(*****************************************)
(**               Canvas                 *)
(*****************************************)

(** A bare_canvas widget just provides a region of the screen where
   low-level painting operations can be carried out directly. *)
let bare_canvas (dim: Gctx.dimension) (f : Gctx.gctx -> unit)
         : widget = {
  repaint = f;
  handle = (fun _ _ -> ());
  size = (fun _ -> dim)
}

(** A canvas is a bordered widget with a notifier_controller. New
   event listeners can be added using the notifier_controller. The
   interior of the canvas will be redrawn by calling a user-specified
   function, provided as a parameter of the canvas widget
   constructor. *)
let canvas (dim: Gctx.dimension) (f : Gctx.gctx -> unit)
         : widget * notifier_controller =
  let w = bare_canvas dim f in
  notifier (border w)


(*****************************************)
(**              Checkbox                *)
(*****************************************)
(* TODO: Task 5 requires you to develop a checkbox widget *)


(** A checkbox is a controller for a boolean value associated with a widget.
   Other widgets might store other data -- a slider might store an integer, for
   example, and the label_controller we saw above is specialized to strings.

   Here we introduce a general-purpose value_controller, which stores a generic
   value. This controller can read (via get_value) and write the value (via
   change_value). It also allows change listeners to be registered by the
   application. All of the added listeners are triggered whenever this value is
   changed.

   We will use this value_controller as part of the checkbox implementation, and
   you are free to use it (if needed) for whatever widget you create in
   Task 6. *)
type 'a value_controller = {
  add_change_listener : ('a -> unit) -> unit;
  get_value           : unit -> 'a;
  change_value        : 'a -> unit
}

(** TODO: The first part of task 5 requires you to implement the following
    generic function. This function takes a value of type 'a and returns a
    value controller for it. Carefully consider what state needs to be
    associated with any value controller. *)
let make_controller (v: 'a) : 'a value_controller =
  let current_value = {contents = v} in
  let listeners = {contents = []} in
  {
    add_change_listener = (fun listener ->
      listeners.contents <- listener :: listeners.contents);
    get_value = (fun () -> current_value.contents);
    change_value = (fun new_val ->
      current_value.contents <- new_val;
      List.iter (fun listener -> listener new_val) listeners.contents)
  }

(** Once we have a notion of value controller, it is handy to have a
    helper function that can be used to update the value stored by
    the value controller (which will, in turn, trigger any associated
    listeners). We've defined this helper function for you.
*)
let update_value (vc : 'a value_controller) (f : 'a -> 'a) : unit =
  let v = vc.get_value () in
  vc.change_value (f v)

(** TODO: Finally, we can assemble a checkbox widget by re-using our previously
    defined components.  A checkbox is a label, a canvas, notifier, and value
    controller that are laid out visually using hpair and "wired together"
    by installing the appropriate mouseclick_listener.

    For some inspiration about how to implement this checkbox widget by
    composing together functionality from other widgets, please take a look at
    the lighbulb.ml demo from lecture.

    Don't forget to use make_controller function you defined above when
    implementing the checkbox, and our provided update_value helper can also
    be of use.

    If your checkbox implementation does not work, do _not_ comment it
    out, because your code will not compile upon submission. Instead,
    you can replace the function body with

      failwith "Checkbox: unimplemented"

    before submitting your code. *)
let checkbox (init: bool) (s: string) : widget * bool value_controller =
  let vc = make_controller init in
  let checkbox_canvas = canvas (22, 22) (fun gc ->
    (* Draw checkbox box *)
    Gctx.draw_rect gc (3, 3) (16, 16);
    (* Draw checkmark if checked *)
    if vc.get_value () then begin
      let check_gc = Gctx.with_color gc {Gctx.r=50; g=100; b=200} in
      Gctx.draw_line check_gc (5, 5) (17, 17);
      Gctx.draw_line check_gc (17, 5) (5, 17)
    end
  ) in
  let (check_w, check_nc) = checkbox_canvas in
  let (label_w, _) = label s in
  let combined_w = hpair check_w (hpair (space (4, 10)) label_w) in
  let (notifier_w, notifier_nc) = notifier combined_w in
  notifier_nc.add_event_listener (mouseclick_listener (fun () ->
    update_value vc (fun v -> not v)
  ));
  (notifier_w, vc)


(*****************************************)
(**          Additional widgets          *)
(*****************************************)

(** TODO: In Task 6 you may choose to add a radio_button widget, a
    slider, or (after discussing your idea with course staff via a
    private Ed post) some other widget of your choice. For instance,
    you may consider implementing a slider for background color or
    radio buttons for mode selection.

   The simplest way to approach this problem is to build a new widget by
   composing from more primitive widgets.  For example, a slider can
   be built from a value controller and a canvas, suitably connected. *)

(** A slider widget that lets users select a value in a range by dragging *)
let slider (min_val: int) (max_val: int) (init_val: int) (label_text: string)
    : widget * int value_controller =
  let vc = make_controller init_val in
  let slider_width = 160 in
  let slider_height = 28 in

  let slider_canvas = canvas (slider_width, slider_height) (fun gc ->
    (* Draw slider track outline *)
    Gctx.draw_rect gc (5, 10) (slider_width - 35, 10);

    (* Calculate slider position based on current value *)
    let current_val = vc.get_value () in
    let range = max_val - min_val in
    let normalized =
      float_of_int (current_val - min_val) /. float_of_int range in
    let bar_width =
      int_of_float (normalized *. float_of_int (slider_width - 35)) in

    (* Draw filled portion of slider with blue color *)
    let fill_gc = Gctx.with_color gc {Gctx.r=80; g=120; b=220} in
    Gctx.fill_rect fill_gc (6, 11) (max 1 (bar_width - 1), 8);

    (* Draw slider thumb *)
    let thumb_x = 5 + bar_width in
    Gctx.fill_rect gc (thumb_x - 2, 8) (4, 14);

    (* Draw value label *)
    let value_str = string_of_int current_val in
    Gctx.draw_string gc (slider_width - 28, 10) value_str
  ) in

  let (slider_w, slider_nc) = slider_canvas in
  let (label_w, _) = label label_text in
  let combined_w = hpair label_w slider_w in
  let (notifier_w, notifier_nc) = notifier combined_w in

  (* Handle mouse events for dragging *)
  let handle_slider_event (g: Gctx.gctx) (e: Gctx.event) : unit =
    let (mouse_x, _) = Gctx.event_pos e g in
    (* Adjust for label width - get position relative to slider canvas *)
    let label_width = fst (Gctx.text_size label_text) in
    let slider_x = mouse_x - label_width in

    if slider_x >= 5 && slider_x <= slider_width - 5 then
      let slider_range = float_of_int (slider_width - 10) in
      let normalized = float_of_int (slider_x - 5) /. slider_range in
      let value_range = float_of_int (max_val - min_val) in
      let new_val = min_val + int_of_float (normalized *. value_range) in
      let clamped_val = max min_val (min max_val new_val) in
      vc.change_value clamped_val
  in

  notifier_nc.add_event_listener (fun g e ->
    match Gctx.event_type e with
    | Gctx.MouseDown -> handle_slider_event g e
    | Gctx.MouseDrag -> handle_slider_event g e
    | _ -> ()
  );

  (notifier_w, vc)

