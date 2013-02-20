MODULE AggEllipse;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource,
  mat := MathL;

TYPE
  ellipse_ptr* = POINTER TO ellipse;
  ellipse* = RECORD(avs.vertex_source)
    x,
    y,
    rx,
    ry,

    scale: bas.double;

    num,
    step: bas.int32;
    cw  : BOOLEAN;
  END;

PROCEDURE (el: ellipse_ptr) Construct*();
BEGIN
  el.Construct^();

  el.x     := 0.0;
  el.y     := 0.0;
  el.rx    := 1.0;
  el.ry    := 1.0;

  el.scale := 1.0;

  el.num   := 4;
  el.step  := 0;
  el.cw    := FALSE;
END Construct;

PROCEDURE (el: ellipse_ptr) calc_num_steps();
VAR
  ra,
  da: bas.double;
BEGIN
  ra := (ABS(el.rx ) + ABS(el.ry)) / 2;
  da := mat.arccos(ra / (ra + 0.125 / el.scale)) * 2;

  el.num := ENTIER(2 * bas.pi / da);
END calc_num_steps;


PROCEDURE (el: ellipse_ptr) ConstructEl*(x, y, rx, ry: bas.double; num_steps: bas.int32; cw: BOOLEAN);
BEGIN
  el.Construct();

  el.x     := x;
  el.y     := y;
  el.rx    := rx;
  el.ry    := ry;

  el.scale := 1.0;

  el.num   := num_steps;
  el.step  := 0;
  el.cw    := cw;

  IF el.num = 0 THEN
    el.calc_num_steps();
  END;
END ConstructEl;

PROCEDURE (el: ellipse_ptr) init*(x, y, rx, ry: bas.double; num_steps: bas.int32; cw: BOOLEAN);
BEGIN
  el.x     := x;
  el.y     := y;
  el.rx    := rx;
  el.ry    := ry;

  el.num   := num_steps;
  el.step  := 0;
  el.cw    := cw;

  IF el.num = 0 THEN
    el.calc_num_steps();
  END;
END init;

PROCEDURE (el: ellipse_ptr) set_approximation_scale*(scale: bas.double);
BEGIN
  el.scale := scale;
  el.calc_num_steps();
END set_approximation_scale;

PROCEDURE (el: ellipse_ptr) approximation_scale*(): bas.double;
BEGIN
  RETURN el.scale;
END approximation_scale;

<*+WOFF301*>
PROCEDURE (el: ellipse_ptr) rewind*(path_id: bas.int32u);
BEGIN
  el.step:=0;
END rewind;
<*-WOFF301*>

PROCEDURE (el: ellipse_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  angle: bas.double;
BEGIN
  IF el.step = el.num THEN
    INC(el.step);
    RETURN bas.path_cmd_end_poly + bas.path_flags_close + bas.path_flags_ccw;
  END;

  IF el.step > el.num THEN
    RETURN bas.path_cmd_stop;
  END;

  angle := el.step / el.num * 2.0 * bas.pi;

  IF el.cw THEN
    angle := 2.0 * bas.pi - angle;
  END;

  x := el.x + mat.cos(angle) * el.rx;
  y := el.y + mat.sin(angle) * el.ry;

  INC(el.step);

  IF el.step = 1 THEN
    RETURN bas.path_cmd_move_to
  ELSE
    RETURN bas.path_cmd_line_to;
  END;
END vertex;


END AggEllipse.