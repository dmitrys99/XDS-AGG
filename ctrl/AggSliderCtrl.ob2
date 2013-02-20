MODULE AggSliderCtrl;

IMPORT
  bas := AggBasics,
  ctl := AggCtrl,
  col := AggColor,
  age := AggEllipse,
  aps := AggPathStorage,
  acs := AggConvStroke,
  agt := AggGSVText,
  math:= AggMath,
  ams := AggMathStroke,
  prt := Printf;

TYPE
  slider_ctrl_impl_ptr* = POINTER TO slider_ctrl_impl;
  slider_ctrl_impl* = RECORD(ctl.ctrl)
    border_width  -,
    border_extra  -,
    text_thickness*,
    value,
    preview_value,

    min-,
    max-: bas.double;

    num_steps*: bas.int32;
    descending*: BOOLEAN;

    label*: ARRAY 64 OF bas.char;

    xs1,
    ys1,
    xs2,
    ys2,
    pdx: bas.double;

    mouse_move: BOOLEAN;

    vx,
    vy: ARRAY 32 OF bas.double;

    ellipse: age.ellipse_ptr;

    idx,
    avertex: bas.int32;

    text     : agt.gsv_text_ptr;
    text_poly: acs.conv_stroke_ptr;
    storage  : aps.path_storage_ptr;
  END;

  slider_ctrl_ptr* = POINTER TO slider_ctrl;
  slider_ctrl* = RECORD(slider_ctrl_impl)
    background_color-     ,
    triangle_color-       ,
    text_color-           ,
    pointer_preview_color-,
    pointer_color-        : col.aggclr_ptr;
  END;

PROCEDURE (sc: slider_ctrl_impl_ptr) calc_box();
BEGIN
  sc.xs1 := sc.x1 + sc.border_width;
  sc.ys1 := sc.y1 + sc.border_width;
  sc.xs2 := sc.x2 - sc.border_width;
  sc.ys2 := sc.y2 - sc.border_width;
END calc_box;


PROCEDURE (sc: slider_ctrl_impl_ptr) ConstructCtrl*(x1, y1, x2, y2: bas.double; flip_y: BOOLEAN);
BEGIN
  sc.ConstructCtrl^(x1, y1, x2, y2, flip_y);

  NEW(sc.ellipse);
  sc.ellipse.Construct();

  NEW(sc.text);
  sc.text.Construct();

  NEW(sc.text_poly);
  sc.text_poly.ConstructVS(sc.text);

  NEW(sc.storage);
  sc.storage.Construct();

  sc.border_width   := 1.0;
  sc.border_extra   := (y2 - y1) / 2;
  sc.text_thickness := 1.0;
  sc.pdx            := 0.0;
  sc.mouse_move     := FALSE;
  sc.value          := 0.5;
  sc.preview_value  := 0.5;
  sc.min            := 0.0;
  sc.max            := 1.0;
  sc.num_steps      := 0;
  sc.descending     := FALSE;

  sc.label := "";

  sc.calc_box;

END ConstructCtrl;

PROCEDURE (sc: slider_ctrl_impl_ptr) Destruct*();
BEGIN
  sc.ellipse.Destruct();
  sc.text.Destruct();
  sc.text_poly.Destruct();
  sc.storage.Destruct();

  sc.ellipse := NIL;
  sc.text := NIL;
  sc.text_poly := NIL;
  sc.storage := NIL;
END Destruct;

PROCEDURE (sc: slider_ctrl_impl_ptr) set_border_width*(t: bas.double; extra: bas.double);
BEGIN
  sc.border_width := t;
  sc.border_extra := extra;

  sc.calc_box();
END set_border_width;

PROCEDURE (sc: slider_ctrl_impl_ptr) set_range*(min, max: bas.double);
BEGIN
  sc.min := min;
  sc.max := max;
END set_range;

PROCEDURE (sc: slider_ctrl_impl_ptr) get_value*(): bas.double;
BEGIN
  RETURN sc.value * (sc.max - sc.min) + sc.min;
END get_value;

PROCEDURE (sc: slider_ctrl_impl_ptr) normalize_value(preview_value_flag: BOOLEAN): BOOLEAN;
VAR
  ret: BOOLEAN;
  step: bas.int32;
BEGIN
  ret := TRUE;

  IF sc.num_steps # 0 THEN
    step := ENTIER(sc.preview_value * sc.num_steps);
    ret := sc.value # (step / sc.num_steps);
    sc.value := step / sc.num_steps;
  ELSE
    sc.value := sc.preview_value;
  END;

  IF preview_value_flag THEN
    sc.preview_value := sc.value;
  END;

  RETURN ret;
END normalize_value;

PROCEDURE (sc: slider_ctrl_impl_ptr) set_value*(v: bas.double);
BEGIN
  sc.preview_value := (v - sc.min) / (sc.max - sc.min);

  IF sc.preview_value > 1.0 THEN
    sc.preview_value := 1.0;
  END;

  IF sc.preview_value < 0.0 THEN
    sc.preview_value := 0.0;
  END;

  IF sc.normalize_value(TRUE) THEN END;
END set_value;

PROCEDURE (sc: slider_ctrl_impl_ptr) in_rect*(x, y: bas.double): BOOLEAN;
BEGIN
  sc.inverse_transform_xy(x, y);

  RETURN
   (x >= sc.x1) &
   (x <= sc.x2) &
   (y >= sc.y1) &
   (y <= sc.y2);
END in_rect;

PROCEDURE (sc: slider_ctrl_impl_ptr) on_mouse_button_down*(x, y: bas.double): BOOLEAN;
VAR
  xp, yp: bas.double;
BEGIN
  sc.inverse_transform_xy(x, y);

  xp := sc.xs1 + (sc.xs2 - sc.xs1) * sc.value;
  yp := (sc.ys1 + sc.ys2) / 2.0;

  IF math.calc_distance(x, y, xp, yp) <= sc.y2 - sc.y1 THEN
    sc.pdx:=xp - x;
    sc.mouse_move := TRUE;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END on_mouse_button_down;

<*+WOFF301*>

PROCEDURE (sc: slider_ctrl_impl_ptr) on_mouse_button_up  *(x, y: bas.double): BOOLEAN;
BEGIN
  IF x = y THEN END;
  sc.mouse_move := FALSE;
  IF sc.normalize_value(TRUE) THEN END;
  RETURN TRUE;
END on_mouse_button_up;

<*-WOFF301*>

PROCEDURE (sc: slider_ctrl_impl_ptr) on_mouse_move*(x, y: bas.double; button_flag: BOOLEAN): BOOLEAN;
VAR
  xp: bas.double;
BEGIN
  sc.inverse_transform_xy(x, y);

  IF ~button_flag THEN
    IF sc.on_mouse_button_up(x, y) THEN END;
    RETURN FALSE;
  END;

  IF sc.mouse_move THEN
    xp := x + sc.pdx;
    sc.preview_value := (xp - sc.xs1) / (sc.xs2 - sc.xs1);

    IF sc.preview_value < 0.0 THEN
      sc.preview_value := 0.0;
    END;

    IF sc.preview_value > 1.0 THEN
     sc.preview_value:=1.0;
    END;

    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END on_mouse_move;

PROCEDURE (sc: slider_ctrl_impl_ptr) on_arrow_keys*(left, right, down, up: BOOLEAN): BOOLEAN;
VAR
  d: bas.double;

BEGIN
  d := 0.005;

  IF sc.num_steps # 0 THEN
    d := 1.0 / sc.num_steps;
  END;

  IF right OR up THEN
    sc.preview_value := sc.preview_value + d;

    IF sc.preview_value > 1.0 THEN
      sc.preview_value := 1.0;
    END;

  IF sc.normalize_value(TRUE) THEN END;

    RETURN TRUE;
  END;

  IF left OR down THEN
    sc.preview_value := sc.preview_value - d;

    IF sc.preview_value < 0.0 THEN
      sc.preview_value:=0.0;
    END;

  IF sc.normalize_value(TRUE) THEN END;

    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END on_arrow_keys;

PROCEDURE (sc: slider_ctrl_impl_ptr) num_paths*(): bas.int32;
BEGIN
  RETURN 6;
END num_paths;

PROCEDURE (sc: slider_ctrl_impl_ptr) rewind*(path_id: bas.int32u);
VAR
  i: bas.int32;
  d, x: bas.double;
  buf: ARRAY 256 OF bas.char;
BEGIN
  sc.idx := path_id;

  CASE path_id OF
    1: (* Triangle *)
      sc.avertex := 0;

      IF sc.descending THEN
        sc.vx[0] := sc.x1;
        sc.vy[0] := sc.y1;
        sc.vx[1] := sc.x2;
        sc.vy[1] := sc.y1;
        sc.vx[2] := sc.x1;
        sc.vy[2] := sc.y2;
        sc.vx[3] := sc.x1;
        sc.vy[3] := sc.y1;
      ELSE
        sc.vx[0] := sc.x1;
        sc.vy[0] := sc.y1;
        sc.vx[1] := sc.x2;
        sc.vy[1] := sc.y1;
        sc.vx[2] := sc.x2;
        sc.vy[2] := sc.y2;
        sc.vx[3] := sc.x1;
        sc.vy[3] := sc.y1;
      END;

    | 2:
      sc.text.set_text(sc.label);

      IF LEN(sc.label) # 0 THEN
        prt.sprintf(buf, sc.label, sc.get_value());
        sc.text.set_text(buf);
      END;

      sc.text.set_start_point(sc.x1, sc.y1);
      sc.text.set_size((sc.y2 - sc.y1) * 1.2, sc.y2 - sc.y1);

      sc.text_poly.set_width(sc.text_thickness);
      sc.text_poly.set_line_join(ams.round_join);
      sc.text_poly.set_line_cap(ams.round_cap);

      sc.text_poly.rewind(0);

    | 3: (* pointer preview *)
      sc.ellipse.init(
        sc.xs1 + (sc.xs2 - sc.xs1) * sc.preview_value,
        (sc.ys1 + sc.ys2) / 2.0,
        sc.y2 - sc.y1,
        sc.y2 - sc.y1,
        32, FALSE);

    | 4: (* pointer *)

      IF sc.normalize_value(FALSE) THEN END;

      sc.ellipse.init(
        sc.xs1 + (sc.xs2 - sc.xs1) * sc.value,
        (sc.ys1 + sc.ys2) / 2.0,
        sc.y2 - sc.y1,
        sc.y2 - sc.y1,
        32, FALSE);

      sc.ellipse.rewind(0);

    | 5:
      sc.storage.remove_all();

      IF sc.num_steps # 0 THEN

        d := (sc.xs2 - sc.xs1) / sc.num_steps;

       IF d > 0.004 THEN
         d := 0.004;
       END;

       FOR i := 0 TO sc.num_steps DO
         x := sc.xs1 + (sc.xs2 - sc.xs1) * i / sc.num_steps;
         sc.storage.move_to(x, sc.y1);
         sc.storage.line_to(x - d * (sc.x2 - sc.x1), sc.y1 - sc.border_extra);
         sc.storage.line_to(x + d * (sc.x2 - sc.x1), sc.y1 - sc.border_extra);
       END;
     END;
  ELSE
    sc.avertex:=0;

    sc.vx[0]:= sc.x1 - sc.border_extra;
    sc.vy[0]:= sc.y1 - sc.border_extra;
    sc.vx[1]:= sc.x2 + sc.border_extra;
    sc.vy[1]:= sc.y1 - sc.border_extra;
    sc.vx[2]:= sc.x2 + sc.border_extra;
    sc.vy[2]:= sc.y2 + sc.border_extra;
    sc.vx[3]:= sc.x1 - sc.border_extra;
    sc.vy[3]:= sc.y2 + sc.border_extra;
  END;
END rewind;

PROCEDURE (sc: slider_ctrl_impl_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  cmd: SET;
BEGIN
  cmd := bas.path_cmd_line_to;

  CASE sc.idx OF
  0:
    IF sc.avertex = 0 THEN
      cmd := bas.path_cmd_move_to;
    END;

    IF sc.avertex >= 4 THEN
      cmd := bas.path_cmd_stop;
    END;

     x := sc.vx[sc.avertex];
     y := sc.vy[sc.avertex];

     INC(sc.avertex);
  | 1:
    IF sc.avertex = 0 THEN
      cmd := bas.path_cmd_move_to;
    END;

    IF sc.avertex >= 4 THEN
      cmd := bas.path_cmd_stop;
    END;

    x := sc.vx[sc.avertex];
    y := sc.vy[sc.avertex];

    INC(sc.avertex);

  | 2:
    cmd := sc.text_poly.vertex(x, y);

  | 3, 4:
    cmd := sc.ellipse.vertex(x, y);

  | 5:
    cmd:=sc.storage.vertex(x, y);
  ELSE
    cmd := bas.path_cmd_stop;
  END;

  IF ~bas.is_stop(cmd) THEN
    sc.transform_xy(x, y);
  END;

  RETURN cmd;
END vertex;

PROCEDURE (sc: slider_ctrl_ptr) ConstructCtrl*(x1, y1, x2, y2: bas.double; flip_y: BOOLEAN);
BEGIN
  sc.ConstructCtrl^(x1, y1, x2, y2, flip_y);

  NEW(sc.background_color);
  NEW(sc.triangle_color);
  NEW(sc.text_color);
  NEW(sc.pointer_preview_color);
  NEW(sc.pointer_color);
  NEW(sc.text_color);


  sc.background_color.ConstructDbl     (1.0, 0.9, 0.8, 1.0);
  sc.triangle_color.ConstructDbl       (0.7, 0.6, 0.6, 1.0);
  sc.text_color.ConstructDbl           (0.0, 0.0, 0.0, 1.0);
  sc.pointer_preview_color.ConstructDbl(0.6, 0.4, 0.4, 0.4);
  sc.pointer_color.ConstructDbl        (0.8, 0.0, 0.0, 0.6);

  NEW(sc.colors, 6);
  sc.colors[0] := sc.background_color;
  sc.colors[1] := sc.triangle_color;
  sc.colors[2] := sc.text_color;
  sc.colors[3] := sc.pointer_preview_color;
  sc.colors[4] := sc.pointer_color;
  sc.colors[5] := sc.text_color;
END ConstructCtrl;

PROCEDURE (sc: slider_ctrl_ptr) set_background_color*(c: col.aggclr);
BEGIN
  sc.background_color^ := c;
END set_background_color;

PROCEDURE (sc: slider_ctrl_ptr) set_pointer_color*(c: col.aggclr);
BEGIN
  sc.pointer_color^ := c;
END set_pointer_color;


END AggSliderCtrl.
PROCEDURE (sc: slider_ctrl_ptr)