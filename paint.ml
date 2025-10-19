(** The main paint application *)


;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position  (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 3, 4, 5 and maybe 6. *)
type shape =
  | Line of {color: color; p1: point; p2: point; thickness: int}
  | Points of {color: color; points: point list}
  | Ellipse of {color: color; center: point; rx: int; ry: int; thickness: int}

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location
        of the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 3 and 4, and maybe 6. *)
type mode =
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseMode of point

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (** The shape currently being previewed, if any. *)
  mutable preview : shape option;

  (** The currently selected line thickness. *)
  mutable thickness : int;

  (* TODO: You will need to add new state for Tasks 2, 5, and *)
  (* possibly 6 *)
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  preview = None;
  thickness = 1;
  (* TODO: You will need to add new state for Tasks 2, 5, and maybe 6 *)
}


(** This function creates a graphics context with the appropriate
    pen color and thickness. *)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) (t: int) : gctx =
  let g = with_color g c in
  let g = with_thickness g t in
  g


(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3,
   4, and possibly 5 or 6. For example, if the user is performing some
   operation that provides "preview" (see Task 2) the repaint function
   must also show the preview. *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line (with_params g l.color l.thickness) l.p1 l.p2
      | Points ps -> draw_points (with_params g ps.color 1) ps.points
      | Ellipse e ->
        let eg = with_params g e.color e.thickness in
        draw_ellipse eg e.center e.rx e.ry
    end in
  Deque.iterate draw_shape paint.shapes;
  (* Draw the preview shape if there is one *)
  begin match paint.preview with
    | None -> ()
    | Some shape -> draw_shape shape
  end

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 300) repaint


(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
       (* This case occurs when the mouse has been clicked in the
          canvas, but before the button has been released. How we
          process the event depends on the current mode of the paint
          canvas.  *)
      (begin match paint.mode with
          | LineStartMode ->
            (* The paint_canvas was waiting for the first click of a line,
               so change it to LineEndMode, recording the starting point of
               the line. *)
            paint.mode <- LineEndMode p
          | LineEndMode _ ->
            (* This should not happen with drag-and-drop *)
            ()
          | PointMode ->
            (* Start drawing points - initial preview with one point *)
            paint.preview <-
              Some (Points {color=paint.color; points=[p]})
          | EllipseMode _ ->
            (* Start drawing an ellipse - change to EllipseMode *)
            paint.mode <- EllipseMode p
       end)
    | MouseDrag ->
      (* In this case, the mouse has been clicked, and it's being dragged
         with the button down. Initially there is nothing to do, but you'll
         need to update this part for Task 2, 3, 4 and maybe 6. *)
      (begin match paint.mode with
          | LineEndMode p1 ->
            (* Update the preview line from p1 to current position *)
            paint.preview <-
              Some (Line {color=paint.color; p1=p1; p2=p;
                          thickness=paint.thickness})
          | PointMode ->
            (* Add the current point to the preview points list *)
            let points_list =
              begin match paint.preview with
              | Some (Points ps) -> ps.points
              | _ -> []
              end in
            (* Always add current point to make drawing smoother *)
            paint.preview <-
              Some (Points {color=paint.color; points=p::points_list})
          | EllipseMode p1 ->
            (* Update the preview ellipse using bounding box method *)
            let (x1, y1) = p1 in
            let (x2, y2) = p in
            let cx = (x1 + x2) / 2 in
            let cy = (y1 + y2) / 2 in
            let rx = abs (x2 - x1) / 2 in
            let ry = abs (y2 - y1) / 2 in
            paint.preview <-
              Some (Ellipse {color=paint.color; center=(cx, cy);
                             rx=rx; ry=ry; thickness=paint.thickness})
          | _ -> ()
       end)
    | MouseUp ->
      (* In this case there was a mouse button release event. TODO: Tasks 2,
         3, 4, and possibly 6 need to do something different here. *)
      (begin match paint.mode with
          | LineEndMode p1 ->
            (* Complete the line from p1 to current position *)
            let line =
              Line {color=paint.color; p1=p1; p2=p;
                    thickness=paint.thickness} in
            Deque.insert_tail line paint.shapes;
            paint.preview <- None;
            paint.mode <- LineStartMode
          | PointMode ->
            (* Complete the points - extract from preview and add to shapes *)
            (begin match paint.preview with
              | Some (Points ps) ->
                Deque.insert_tail (Points ps) paint.shapes;
                paint.preview <- None
              | _ -> ()
             end)
          | EllipseMode p1 ->
            (* Complete the ellipse using bounding box method *)
            let (x1, y1) = p1 in
            let (x2, y2) = p in
            let cx = (x1 + x2) / 2 in
            let cy = (y1 + y2) / 2 in
            let rx = abs (x2 - x1) / 2 in
            let ry = abs (y2 - y1) / 2 in
            let ellipse =
              Ellipse {color=paint.color; center=(cx, cy);
                       rx=rx; ry=ry; thickness=paint.thickness} in
            Deque.insert_tail ellipse paint.shapes;
            paint.preview <- None;
            paint.mode <- EllipseMode (0, 0)
          | _ -> ()
       end)

    | MouseMove ->
      (* For point mode, capture points during mouse move *)
      (begin match paint.mode with
          | PointMode ->
            (begin match paint.preview with
              | Some (Points ps) ->
                (* Add point during move to make drawing smoother *)
                paint.preview <-
                  Some (Points {color=paint.color; points=p::ps.points})
              | _ -> ()
             end)
          | _ -> ()
       end)
    | _ -> ()
    (* This catches the KeyPress event (where the user typed a key
       when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the paint
    program -- the buttons, color selectors, etc., and lays them out
    in the top - level window. *)
(* TODO: Tasks 1, 4, 5, and 6 involve adding new buttons or changing
   the layout of the Paint GUI. Initially the layout is ugly because
   we use only the hpair widget demonstrated in Lecture. Task 1 asks
   you to make improvements to make the layout more appealing. You may
   choose to arrange the buttons and other GUI elements of the paint
   program however you like, so long as it is easily apparent how to
   use the interface; the sample screenshot in the homework
   description shows one possible design. Also, feel free to improve
   the visual components of the GUI; for example, our solution puts
   borders around the buttons and uses a custom "color button" that
   changes its appearance based on whether or not the color is
   currently selected. *)

(** Create a radio button widget that shows selection state *)
let radio_button (label_text: string) (is_selected: unit -> bool)
    (on_click: unit -> unit) : widget =
  let (w, nc) = canvas (100, 28) (fun gc ->
    (* Draw radio circle *)
    draw_ellipse gc (12, 14) 8 8;
    (* Fill circle if selected *)
    if is_selected () then begin
      let fill_gc = with_color gc {r=50; g=100; b=200} in
      fill_rect fill_gc (8, 10) (8, 8)
    end;
    (* Draw label - centered vertically *)
    draw_string gc (28, 10) label_text
  ) in
  nc.add_event_listener (mouseclick_listener on_click);
  border w

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(** This function runs when the Undo button is clicked.
    It simply removes the last shape from the shapes deque. *)
(* TODO: You need to modify this in Task 3 and 4, and potentially in
   Task 2 (depending on your implementation). *)

let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)

;; nc_undo.add_event_listener (mouseclick_listener undo)

(** Helper function to check if in LineStartMode *)
let is_line_mode () : bool =
  match paint.mode with
  | LineStartMode | LineEndMode _ -> true
  | _ -> false

(** Helper function to check if in PointMode *)
let is_point_mode () : bool =
  match paint.mode with
  | PointMode -> true
  | _ -> false

(** Helper function to check if in EllipseMode *)
let is_ellipse_mode () : bool =
  match paint.mode with
  | EllipseMode _ -> true
  | _ -> false

(** Create the mode buttons *)
let w_line =
  radio_button "Line" is_line_mode (fun () -> paint.mode <- LineStartMode)
let w_point =
  radio_button "Point" is_point_mode (fun () -> paint.mode <- PointMode)
let w_ellipse =
  radio_button "Ellipse" is_ellipse_mode
    (fun () -> paint.mode <- EllipseMode (0, 0))

(** Create the thickness slider *)
let (w_thickness_slider, vc_thickness_slider) =
  slider 1 10 1 "Thickness:"

(** Create the thickness checkbox *)
let (w_thickness, vc_thickness) = checkbox false "Thick lines"

(** Add a change listener to update paint.thickness from slider *)
;; vc_thickness_slider.add_change_listener (fun value ->
  (* Only apply slider value if checkbox is checked *)
  if vc_thickness.get_value () then
    paint.thickness <- value
)

(** Add a change listener to checkbox *)
;; vc_thickness.add_change_listener (fun checked ->
  if checked then
    (* When checked, use slider value *)
    paint.thickness <- vc_thickness_slider.get_value ()
  else
    (* When unchecked, use thin lines *)
    paint.thickness <- 1
)

(** A spacer widget *)
let spacer : widget = space (10,10)

(** Create a styled undo button *)
let w_undo_styled =
  let (w, nc) = canvas (60, 28) (fun gc ->
    (* Draw button background *)
    fill_rect gc (2, 2) (56, 24);
    (* Draw button text - centered vertically *)
    let text_gc = with_color gc white in
    draw_string text_gc (12, 10) "Undo"
  ) in
  nc.add_event_listener (mouseclick_listener undo);
  border w

(** The drawing mode selector row *)
let mode_selector : widget =
  hlist [w_point; space (8,10); w_line; space (8,10);
         w_ellipse; space (20,10); w_undo_styled]

(** The thickness controls row *)
let thickness_controls : widget =
  hlist [border w_thickness; space (15,10);
         border w_thickness_slider]

(** The mode toolbar with all controls *)
let mode_toolbar : widget =
  vlist [mode_selector; space (8,10); thickness_controls]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color
   and some buttons for changing it. Both the indicator and the buttons
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created
    with; they are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)
let color_button (c: color) : widget =
  let w,nc = colored_square 28 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  border w

(** The color selection toolbar. Contains the color indicator and
    buttons for several different colors. *)
(* TODO: Task 1 - This code contains a great deal of boilerplate.  You
     should come up with a better, more elegant, more concise solution... *)
   let color_toolbar : widget =
   hlist [color_button black;
          space (3,10);
          color_button white;
          space (3,10);
          color_button red;
          space (3,10);
          color_button green;
          space (3,10);
          color_button blue;
          space (3,10);
          color_button yellow;
          space (3,10);
          color_button cyan;
          space (3,10);
          color_button magenta;
          space (40,10);
          color_indicator]

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets. *)
(* TODO: Task 1 (and others) involve modifing this layout to add new
   buttons and make the layout more aesthetically appealing. *)
let paint_widget =
   vlist [paint_canvas; space (8,10); mode_toolbar; space (8,10); color_toolbar]
