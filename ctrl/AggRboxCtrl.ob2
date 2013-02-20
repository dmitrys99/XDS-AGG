MODULE AggRboxCtrl;

IMPORT
  bas := AggBasics,
  ctl := AggCtrl,
  acs := AggConvStroke,
  agt := AggGSVText,
  col := AggColor,
  age := AggEllipse,
  math:= AggMath,
  ams := AggMathStroke;

TYPE
  rbox_ctrl_impl_ptr* = POINTER TO rbox_ctrl_impl;
  rbox_ctrl_impl* = RECORD(ctl.ctrl)
    border_width*,
    border_extra*,
    text_thickness*,
    text_height*,
    text_width*: bas.double;

    items: ARRAY 32 OF POINTER TO ARRAY OF CHAR;

    num_items : bas.int32;
    cur_item* : bas.int32;

    xs1,
    ys1,
    xs2,
    ys2,
    dy: bas.double;

    vx,
    vy : ARRAY 32 OF bas.double;

    draw_item: bas.int32;

    ellipse     : age.ellipse_ptr;
    ellipse_poly: acs.conv_stroke_ptr;
    text        : agt.gsv_text_ptr;
    text_poly   : acs.conv_stroke_ptr;

    idx,
    avertex: bas.int32;
  END;

 rbox_ctrl_ptr* = POINTER TO rbox_ctrl;
 rbox_ctrl* = RECORD(rbox_ctrl_impl)
   background_color,
   border_color    ,
   text_color      ,
   inactive_color  ,
   active_color    : col.aggclr_ptr;
 END;


PROCEDURE (rb: rbox_ctrl_impl_ptr) calc_rbox*();
BEGIN
  rb.xs1 := rb.x1 + rb.border_width;
  rb.ys1 := rb.y1 + rb.border_width;
  rb.xs2 := rb.x2 - rb.border_width;
  rb.ys2 := rb.y2 - rb.border_width;
END calc_rbox;


PROCEDURE (rb: rbox_ctrl_impl_ptr) ConstructCtrl*(x1, y1, x2, y2: bas.double; flip_y: BOOLEAN);
BEGIN
  rb.ConstructCtrl^(x1, y1, x2, y2, flip_y);

  NEW(rb.ellipse);
  rb.ellipse.Construct();

  NEW(rb.text);
  rb.text.Construct();

  NEW(rb.ellipse_poly);
  rb.ellipse_poly.ConstructVS(rb.ellipse);

  NEW(rb.text_poly);
  rb.text_poly.ConstructVS(rb.text);

  rb.border_width   := 1.0;
  rb.border_extra   := 0.0;
  rb.text_thickness := 1.5;
  rb.text_height    := 9.0;
  rb.text_width     := 0.0;

  rb.num_items := 0;
  rb.cur_item  := -1;

  rb.idx     := 0;
  rb.avertex := 0;

  rb.calc_rbox();
END ConstructCtrl;


PROCEDURE (rb: rbox_ctrl_impl_ptr) add_item*(VAR text: ARRAY OF CHAR);
BEGIN
  IF rb.num_items < 32 THEN

    IF LEN(text) > 255 THEN
      NEW(rb.items[rb.num_items], 255);
    ELSE
      NEW(rb.items[rb.num_items], LEN(text) + 1);
    END;

    COPY(text, rb.items[rb.num_items]^);
    INC(rb.num_items);
  END;
END add_item;

PROCEDURE (rb: rbox_ctrl_impl_ptr) in_rect*(x, y: bas.double): BOOLEAN;
BEGIN
  rb.inverse_transform_xy(x, y);

  RETURN
    (x >= rb.x1) &
    (x <= rb.x2) &
    (y >= rb.y1) &
    (y <= rb.y2);
END in_rect;

PROCEDURE (rb: rbox_ctrl_impl_ptr) on_mouse_button_down*(x, y: bas.double): BOOLEAN;
VAR
  i: bas.int32;
  xp, yp: bas.double;
BEGIN
  rb.inverse_transform_xy(x, y);

  FOR i := 0 TO rb.num_items - 1 DO
    xp := rb.xs1 + rb.dy / 1.3;
    yp := rb.ys1 + rb.dy * i + rb.dy / 1.3;

    IF math.calc_distance(x, y, xp, yp) <= rb.text_height / 1.5 THEN
      rb.cur_item := i;
      RETURN TRUE;
    END;

  END;

  RETURN FALSE;
END on_mouse_button_down;

PROCEDURE (rb: rbox_ctrl_impl_ptr) on_arrow_keys*(left, right, down, up: BOOLEAN): BOOLEAN;
BEGIN
  IF rb.cur_item >= 0 THEN

    IF up OR right THEN
      INC(rb.cur_item);

      IF rb.cur_item >= rb.num_items THEN
        rb.cur_item := 0;
      END;

      RETURN TRUE;
    END;

    IF down OR left THEN
      DEC(rb.cur_item);

      IF rb.cur_item < 0 THEN
        rb.cur_item := rb.num_items - 1;
      END;

      RETURN TRUE;
    END;

  END;

  RETURN FALSE;
END on_arrow_keys;

PROCEDURE (rb: rbox_ctrl_impl_ptr) num_paths*(): bas.int32;
BEGIN
  RETURN 5;
END num_paths;

PROCEDURE (rb: rbox_ctrl_impl_ptr) rewind*(path_id: bas.int32u);
BEGIN
  rb.idx := path_id;
  rb.dy  := rb.text_height * 2.0;

  rb.draw_item := 0;

  CASE path_id OF
    0 : (* Background *)
      rb.avertex := 0;

      rb.vx[0] := rb.x1 - rb.border_extra;
      rb.vy[0] := rb.y1 - rb.border_extra;
      rb.vx[1] := rb.x2 + rb.border_extra;
      rb.vy[1] := rb.y1 - rb.border_extra;
      rb.vx[2] := rb.x2 + rb.border_extra;
      rb.vy[2] := rb.y2 + rb.border_extra;
      rb.vx[3] := rb.x1 - rb.border_extra;
      rb.vy[3] := rb.y2 + rb.border_extra;

  | 1 : (* Border *)
     rb.avertex := 0;

     rb.vx[0] := rb.x1;
     rb.vy[0] := rb.y1;
     rb.vx[1] := rb.x2;
     rb.vy[1] := rb.y1;
     rb.vx[2] := rb.x2;
     rb.vy[2] := rb.y2;
     rb.vx[3] := rb.x1;
     rb.vy[3] := rb.y2;
     rb.vx[4] := rb.x1 + rb.border_width;
     rb.vy[4] := rb.y1 + rb.border_width;
     rb.vx[5] := rb.x1 + rb.border_width;
     rb.vy[5] := rb.y2 - rb.border_width;
     rb.vx[6] := rb.x2 - rb.border_width;
     rb.vy[6] := rb.y2 - rb.border_width;
     rb.vx[7] := rb.x2 - rb.border_width;
     rb.vy[7] := rb.y1 + rb.border_width;

  | 2 : (* Text *)
     rb.text.set_text       (rb.items[0]^);
     rb.text.set_start_point(rb.xs1 + rb.dy * 1.5, rb.ys1 + rb.dy / 2.0);
     rb.text.set_size       (rb.text_height, rb.text_width);

     rb.text_poly.set_width    (rb.text_thickness);
     rb.text_poly.set_line_join(ams.round_join);
     rb.text_poly.set_line_cap (ams.round_cap);

     rb.text_poly.rewind(0);

  | 3 : (* Inactive items *)
     rb.ellipse.init(
      rb.xs1 + rb.dy / 1.3,
      rb.ys1 + rb.dy / 1.3,
      rb.text_height / 1.5,
      rb.text_height / 1.5, 32, FALSE);

     rb.ellipse_poly.set_width(rb.text_thickness);
     rb.ellipse_poly.rewind(0);

  | 4 : (* Active Item *)
    IF rb.cur_item >= 0 THEN
      rb.ellipse.init(
       rb.xs1 + rb.dy / 1.3,
       rb.ys1 + rb.dy * rb.cur_item + rb.dy / 1.3,
       rb.text_height / 2.0,
       rb.text_height / 2.0, 32, FALSE);

      rb.ellipse.rewind(0);
    END;
  END; (* CASE *)
END rewind;

PROCEDURE (rb: rbox_ctrl_impl_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  cmd: SET;

BEGIN
  cmd := bas.path_cmd_line_to;

  CASE rb.idx OF
  0:

    IF rb.avertex = 0 THEN
      cmd := bas.path_cmd_move_to;
    END;

    IF rb.avertex >= 4 THEN
      cmd := bas.path_cmd_stop;
    END;

    x := rb.vx[rb.avertex];
    y := rb.vy[rb.avertex];

    INC(rb.avertex);

  | 1:
    IF (rb.avertex = 0) OR (rb.avertex = 4) THEN
      cmd := bas.path_cmd_move_to;
    END;

    IF rb.avertex >= 8 THEN
      cmd := bas.path_cmd_stop;
    END;

    x := rb.vx[rb.avertex];
    y := rb.vy[rb.avertex];

    INC(rb.avertex);

  | 2:
    cmd := rb.text_poly.vertex(x, y);

    IF bas.is_stop(cmd) THEN
      INC(rb.draw_item);

      IF rb.draw_item < rb.num_items THEN
        rb.text.set_text (rb.items[rb.draw_item]^);
        rb.text.set_start_point(
         rb.xs1 + rb.dy * 1.5,
         rb.ys1 + rb.dy * (rb.draw_item + 1) - rb.dy / 2.0);

        rb.text_poly.rewind(0);
        cmd := rb.text_poly.vertex(x, y);
      END;

    END;

  | 3:
    cmd := rb.ellipse_poly.vertex(x, y);

    IF bas.is_stop(cmd) THEN
      INC(rb.draw_item);

      IF rb.draw_item < rb.num_items THEN
        rb.ellipse.init(
          rb.xs1 + rb.dy / 1.3,
          rb.ys1 + rb.dy * rb.draw_item + rb.dy / 1.3,
          rb.text_height / 1.5,
          rb.text_height / 1.5, 32, FALSE);

        rb.ellipse_poly.rewind(0);
        cmd := rb.ellipse_poly.vertex(x, y);
      END;

    END;

  | 4:
    IF rb.cur_item >= 0 THEN
      cmd := rb.ellipse.vertex(x, y)
    ELSE
      cmd := bas.path_cmd_stop;
    END;
  ELSE
    cmd := bas.path_cmd_stop;
  END;

  IF ~bas.is_stop(cmd) THEN
    rb.transform_xy(x, y);
  END;

  RETURN cmd;
END vertex;

PROCEDURE (rb: rbox_ctrl_ptr) ConstructCtrl*(x1, y1, x2, y2: bas.double; flip_y: BOOLEAN);
BEGIN
  rb.ConstructCtrl^(x1, y1, x2, y2, flip_y);

  NEW(rb.colors, 5);

  NEW(rb.background_color);
  NEW(rb.border_color);
  NEW(rb.text_color);
  NEW(rb.inactive_color);
  NEW(rb.active_color);

  rb.background_color.ConstructDbl(1.0, 1.0, 0.9, 1.0);
  rb.border_color.ConstructDbl    (0.0, 0.0, 0.0, 1.0);
  rb.text_color.ConstructDbl      (0.0, 0.0, 0.0, 1.0);
  rb.inactive_color.ConstructDbl  (0.0, 0.0, 0.0, 1.0);
  rb.active_color.ConstructDbl    (0.4, 0.0, 0.0, 1.0);

  rb.colors[0] := rb.background_color;
  rb.colors[1] := rb.border_color;
  rb.colors[2] := rb.text_color;
  rb.colors[3] := rb.inactive_color;
  rb.colors[4] := rb.active_color;

END ConstructCtrl;

PROCEDURE (rb: rbox_ctrl_ptr) set_background_color*(c: col.aggclr_ptr);
BEGIN
  rb.background_color := c;
END set_background_color;

PROCEDURE (rb: rbox_ctrl_ptr) set_border_color*(c: col.aggclr_ptr);
BEGIN
  rb.border_color := c;
END set_border_color;

PROCEDURE (rb: rbox_ctrl_ptr) set_text_color*(c: col.aggclr_ptr);
BEGIN
  rb.text_color := c;
END set_text_color;

PROCEDURE (rb: rbox_ctrl_ptr) set_inactive_color*(c: col.aggclr_ptr);
BEGIN
  rb.inactive_color := c;
END set_inactive_color;

PROCEDURE (rb: rbox_ctrl_ptr) set_active_color*(c: col.aggclr_ptr);
BEGIN
  rb.active_color := c;
END set_active_color;


END AggRboxCtrl.
PROCEDURE (rb: rbox_ctrl_ptr)