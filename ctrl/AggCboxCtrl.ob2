MODULE AggCboxCtrl;
IMPORT
  bas := AggBasics,
  ctl := AggCtrl,
  acs := AggConvStroke,
  agt := AggGSVText,
  col := AggColor,
  ams := AggMathStroke;

TYPE
  cbox_ctrl_impl_ptr* = POINTER TO cbox_ctrl_impl;
  cbox_ctrl_impl* = RECORD(ctl.ctrl)
    text_thickness*,
    text_height*   ,
    text_width*    : bas.double;

    label* : ARRAY 128 OF bas.char;
    status*: BOOLEAN;

    vx, vy: ARRAY 32 OF bas.double;

    text      : agt.gsv_text_ptr;
    text_poly : acs.conv_stroke_ptr;

    idx: bas.int32;
    avertex: bas.int32;
  END;

  cbox_ctrl_ptr* = POINTER TO cbox_ctrl;
  cbox_ctrl* = RECORD(cbox_ctrl_impl)
    text_color-,
    inactive_color-,
    active_color-: col.aggclr_ptr;
  END;

PROCEDURE (cbox: cbox_ctrl_impl_ptr) ConstructCBox*(x, y: bas.double; l: ARRAY OF bas.char; flip_y: BOOLEAN);
BEGIN
  cbox.ConstructCtrl(x, y, x + 9.0 * 1.5, y + 9.0 * 1.5, flip_y);

  NEW(cbox.text);
  NEW(cbox.text_poly);

  cbox.text.Construct;
  cbox.text_poly.ConstructVS(cbox.text);

  cbox.text_thickness:=1.5;
  cbox.text_height   :=9.0;
  cbox.text_width    :=0.0;

  cbox.status := FALSE;

  COPY(l, cbox.label);
END ConstructCBox;

PROCEDURE (cbox: cbox_ctrl_impl_ptr) Destruct*();
BEGIN
  cbox.text_poly.Destruct();
  cbox.text.Destruct();

  cbox.text_poly := NIL;
  cbox.text := NIL;
END Destruct;


PROCEDURE (cbox: cbox_ctrl_impl_ptr) in_rect*(x, y: bas.double): BOOLEAN;
BEGIN
  cbox.inverse_transform_xy(x, y);

  RETURN
    (x >= cbox.x1) &
    (y >= cbox.y1) &
    (x <= cbox.x2) &
    (y <= cbox.y2) ;
END in_rect;

PROCEDURE (cbox: cbox_ctrl_impl_ptr) on_mouse_button_down*(x, y: bas.double): BOOLEAN;
BEGIN
  cbox.inverse_transform_xy(x, y);

  IF (x >= cbox.x1) &
     (y >= cbox.y1) &
     (x <= cbox.x2) &
     (y <= cbox.y2) THEN
    cbox.status := ~cbox.status;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END on_mouse_button_down;

PROCEDURE (cbox: cbox_ctrl_impl_ptr) num_paths*(): bas.int32;
BEGIN
  RETURN 3;
END num_paths;

PROCEDURE (cbox: cbox_ctrl_impl_ptr) rewind*(path_id: bas.int32u);
VAR
  d2, t: bas.double;
BEGIN
  cbox.idx := path_id;

  CASE path_id OF
    0: (* Border *)
      cbox.avertex:=0;

      cbox.vx[0] := cbox.x1;
      cbox.vy[0] := cbox.y1;
      cbox.vx[1] := cbox.x2;
      cbox.vy[1] := cbox.y1;
      cbox.vx[2] := cbox.x2;
      cbox.vy[2] := cbox.y2;
      cbox.vx[3] := cbox.x1;
      cbox.vy[3] := cbox.y2;
      cbox.vx[4] := cbox.x1 + cbox.text_thickness;
      cbox.vy[4] := cbox.y1 + cbox.text_thickness;
      cbox.vx[5] := cbox.x1 + cbox.text_thickness;
      cbox.vy[5] := cbox.y2 - cbox.text_thickness;
      cbox.vx[6] := cbox.x2 - cbox.text_thickness;
      cbox.vy[6] := cbox.y2 - cbox.text_thickness;
      cbox.vx[7] := cbox.x2 - cbox.text_thickness;
      cbox.vy[7] := cbox.y1 + cbox.text_thickness;

    | 1: (* Text *)
      cbox.text.set_text       (cbox.label);
      cbox.text.set_start_point(cbox.x1 + cbox.text_height * 2.0, cbox.y1 + cbox.text_height / 5.0);
      cbox.text.set_size       (cbox.text_height, cbox.text_width);

      cbox.text_poly.set_width(cbox.text_thickness);
      cbox.text_poly.set_line_join(ams.round_join);
      cbox.text_poly.set_line_cap (ams.round_cap);

      cbox.text_poly.rewind(0);

    | 2: (* Active item *)
      cbox.avertex := 0;

      d2 := (cbox.y2 - cbox.y1) / 2.0;
      t  := cbox.text_thickness * 1.5;

      cbox.vx[0] := cbox.x1 + cbox.text_thickness;
      cbox.vy[0] := cbox.y1 + cbox.text_thickness;
      cbox.vx[1] := cbox.x1 + d2;
      cbox.vy[1] := cbox.y1 + d2 - t;
      cbox.vx[2] := cbox.x2 - cbox.text_thickness;
      cbox.vy[2] := cbox.y1 + cbox.text_thickness;
      cbox.vx[3] := cbox.x1 + d2 + t;
      cbox.vy[3] := cbox.y1 + d2;
      cbox.vx[4] := cbox.x2 - cbox.text_thickness;
      cbox.vy[4] := cbox.y2 - cbox.text_thickness;
      cbox.vx[5] := cbox.x1 + d2;
      cbox.vy[5] := cbox.y1 + d2 + t;
      cbox.vx[6] := cbox.x1 + cbox.text_thickness;
      cbox.vy[6] := cbox.y2 - cbox.text_thickness;
      cbox.vx[7] := cbox.x1 + d2 - t;
      cbox.vy[7] := cbox.y1 + d2;
   END;
END rewind;

PROCEDURE (cbox: cbox_ctrl_impl_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  cmd: SET;
BEGIN
  cmd := bas.path_cmd_line_to;

  CASE cbox.idx OF
  0:

    IF (cbox.avertex = 0) OR
       (cbox.avertex = 4) THEN
     cmd := bas.path_cmd_move_to;
    END;

    IF cbox.avertex >= 8 THEN
      cmd := bas.path_cmd_stop;
    END;

    x := cbox.vx[cbox.avertex];
    y := cbox.vy[cbox.avertex];

    INC(cbox.avertex);
  | 1:
    cmd := cbox.text_poly.vertex(x, y);
  | 2:

   IF cbox.status THEN
     IF cbox.avertex = 0 THEN
      cmd := bas.path_cmd_move_to;
     END;

     IF cbox.avertex >= 8 THEN
       cmd := bas.path_cmd_stop;
     END;

     x := cbox.vx[cbox.avertex];
     y := cbox.vy[cbox.avertex];

     INC(cbox.avertex);
   ELSE
     cmd := bas.path_cmd_stop;
   END;
   
  ELSE
    cmd := bas.path_cmd_stop;
  END;

  IF ~bas.is_stop(cmd) THEN
    cbox.transform_xy(x, y);
  END;
  RETURN cmd;
END vertex;

PROCEDURE (cbox: cbox_ctrl_ptr) ConstructCBox*(x, y: bas.double; l: ARRAY OF bas.char; flip_y: BOOLEAN);
BEGIN
  cbox.ConstructCBox^(x, y, l, flip_y);
  NEW(cbox.colors, 3);

  NEW(cbox.text_color);
  NEW(cbox.inactive_color);
  NEW(cbox.active_color);

  cbox.text_color.ConstructDbl    (0.0, 0.0, 0.0, 1.0);
  cbox.inactive_color.ConstructDbl(0.0, 0.0, 0.0, 1.0);
  cbox.active_color.ConstructDbl  (0.4, 0.0, 0.0, 1.0);

  cbox.colors[0] := cbox.text_color;
  cbox.colors[1] := cbox.inactive_color;
  cbox.colors[2] := cbox.active_color;
END ConstructCBox;

PROCEDURE (cbox: cbox_ctrl_ptr) set_text_color*(c: col.aggclr);
BEGIN
  cbox.text_color^ := c;
END set_text_color;

PROCEDURE (cbox: cbox_ctrl_ptr) set_inactive_color*(c: col.aggclr);
BEGIN
  cbox.inactive_color^ := c;
END set_inactive_color;

PROCEDURE (cbox: cbox_ctrl_ptr) set_active_color*(c: col.aggclr);
BEGIN
  cbox.active_color^ := c;
END set_active_color;

END AggCboxCtrl.
PROCEDURE (cbox: cbox_ctrl_ptr)