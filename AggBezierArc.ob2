MODULE AggBezierArc;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource,
  ata := AggTransAffine,
  mat := MathL;

TYPE
  bezier_arc_ptr* = POINTER TO bezier_arc;
  bezier_arc* = RECORD(avs.vertex_source)
    vertexno,
    num_vertices-: bas.int32;
    vertices*: bas.double_26;
    cmd: SET;
  END;

(**
==========================================================bezier_arc_svg
  Compute an SVG-style bezier arc.

  Computes an elliptical arc from (x1, y1) to (x2, y2). The size and
  orientation of the ellipse are defined by two radii (rx, ry)
  and an x-axis-rotation, which indicates how the ellipse as a whole
  is rotated relative to the current coordinate system. The center
  (cx, cy) of the ellipse is calculated automatically to satisfy the
  constraints imposed by the other parameters.
  large-arc-flag and sweep-flag contribute to the automatic calculations
  and help determine how the arc is drawn.
*)

  bezier_arc_svg_ptr* = POINTER TO bezier_arc_svg;
  bezier_arc_svg* = RECORD(bezier_arc)
    radii_ok*: BOOLEAN;

(*
    constructor Construct(x1 ,y1 ,rx ,ry ,angle : double; large_arc_flag ,sweep_flag : boolean; x2 ,y2 : double ); overload;

    procedure init(x0 ,y0 ,rx ,ry ,angle : double; large_arc_flag ,sweep_flag : boolean; x2 ,y2 : double );

    function  radii_ok : boolean;*)

  END;


CONST
  bezier_arc_angle_epsilon = 0.01;

PROCEDURE arc_to_bezier(cx, cy, rx, ry, start_angle, sweep_angle: bas.double; VAR curve: ARRAY OF bas.double; startidx: bas.int32);
VAR
 i: bas.int32;

 sn, cs,
 x0, y0,
 tx, ty: bas.double;
 px, py: ARRAY 4 OF bas.double;

BEGIN
  x0 := mat.cos(sweep_angle / 2.0);
  y0 := mat.sin(sweep_angle / 2.0);
  tx := (1.0 - x0) * 4.0 / 3.0;
  ty := y0 - tx * x0 / y0;

  px[0] := x0;
  py[0] := -y0;
  px[1] := x0 + tx;
  py[1] := -ty;
  px[2] := x0 + tx;
  py[2] := ty;
  px[3] := x0;
  py[3] := y0;

  sn := mat.sin(start_angle + sweep_angle / 2.0);
  cs := mat.cos(start_angle + sweep_angle / 2.0);

  FOR i := 0 TO 3 DO
    curve[startidx + i * 2]     := cx + rx * (px[i] * cs - py[i] * sn);
    curve[startidx + i * 2 + 1] := cy + ry * (px[i] * sn + py[i] * cs);
  END;
END arc_to_bezier;



PROCEDURE (ba: bezier_arc_ptr) Construct*();
BEGIN
  ba.vertexno := 26;
  ba.num_vertices := 0;
  ba.cmd := bas.path_cmd_line_to;
END Construct;

PROCEDURE (ba: bezier_arc_ptr) init*(x, y, rx, ry, start_angle, sweep_angle: bas.double);
VAR
  i: bas.int32;
  f: bas.double;

  total_sweep,
  local_sweep,
  prev_sweep: bas.double;
  done: BOOLEAN;

BEGIN
  i := ENTIER(start_angle / (2.0 * bas.pi));
  f := start_angle - (i * 2.0 * bas.pi);

  start_angle := f;

  IF sweep_angle >= 2.0 * bas.pi THEN
    sweep_angle := 2.0 * bas.pi;
  END;

  IF sweep_angle <= -2.0 * bas.pi THEN
    sweep_angle := -2.0 * bas.pi;
  END;

  IF ABS(sweep_angle) < 1.0E-10 THEN
    ba.num_vertices := 4;

    ba.cmd := bas.path_cmd_line_to;

    ba.vertices[0] := x + rx * mat.cos(start_angle);
    ba.vertices[1] := y + ry * mat.sin(start_angle);
    ba.vertices[2] := x + rx * mat.cos(start_angle + sweep_angle);
    ba.vertices[3] := y + ry * mat.sin(start_angle + sweep_angle);

    RETURN;
  END;

  total_sweep := 0.0;

  ba.num_vertices:=2;

  ba.cmd := bas.path_cmd_curve4;

  done := FALSE;

  REPEAT
    IF sweep_angle < 0.0 THEN
      prev_sweep  := total_sweep;
      local_sweep := -bas.pi * 0.5;
      total_sweep := total_sweep - (bas.pi * 0.5);

      IF total_sweep <= sweep_angle + bezier_arc_angle_epsilon THEN
        local_sweep := sweep_angle - prev_sweep;
        done := TRUE;
      END;
    ELSE
      prev_sweep  := total_sweep;
      local_sweep := bas.pi * 0.5;
      total_sweep := total_sweep + (bas.pi * 0.5);

      IF total_sweep >= sweep_angle - bezier_arc_angle_epsilon THEN
        local_sweep:=sweep_angle - prev_sweep;
        done:=TRUE;
      END;
    END;

    arc_to_bezier(
      x, y, rx, ry,
      start_angle, local_sweep,
      ba.vertices, ba.num_vertices - 2);

    ba.num_vertices := ba.num_vertices + 6;
    start_angle    := start_angle + local_sweep;
  UNTIL done OR (ba.num_vertices >= 26);
END init;

PROCEDURE (ba: bezier_arc_ptr) ConstructBArc*(x, y, rx, ry, start_angle, sweep_angle: bas.double);
BEGIN
  ba.init(x, y, rx, ry, start_angle, sweep_angle);
END ConstructBArc;

<*+WOFF301*>
PROCEDURE (ba: bezier_arc_ptr) rewind*(path_id: bas.int32u);
BEGIN
  ba.vertexno := 0;
END rewind;
<*-WOFF301*>

PROCEDURE (ba: bezier_arc_ptr) vertex*(VAR x, y: bas.double): SET;
BEGIN
  IF ba.vertexno >= ba.num_vertices THEN
    RETURN bas.path_cmd_stop;
  END;

  x := ba.vertices[ba.vertexno];
  y := ba.vertices[ba.vertexno + 1];

  INC(ba.vertexno, 2);

  IF ba.vertexno = 2 THEN
    RETURN bas.path_cmd_move_to;
  ELSE
    RETURN ba.cmd;
  END;
END vertex;

PROCEDURE (bav: bezier_arc_svg_ptr) Construct*();
BEGIN
  bav.Construct^();
  bav.radii_ok := FALSE;
END Construct;

PROCEDURE (bav: bezier_arc_svg_ptr) initBASVG(x0, y0, rx, ry, angle: bas.double; large_arc_flag, sweep_flag: BOOLEAN; x2, y2: bas.double);
VAR
  i: bas.int32;

  v,
  p,  n,

  sq,
  x1, y1,
  cx, cy,
  ux, uy,
  vx, vy,

  dx2, dy2,
  prx, pry,
  px1, py1,
  cx1, cy1,
  sx2, sy2,

  sign, coef,

  radii_check,
  start_angle,
  sweep_angle,

  cos_a, sin_a: bas.double;

  mtx: ata.trans_affine_rotation_ptr;
  trn: ata.trans_affine_translation_ptr;
BEGIN
  bav.radii_ok := TRUE;

  IF rx < 0.0 THEN
    rx := -rx;
  END;

  IF ry < 0.0 THEN
    ry := -rx;
  END;

(* Calculate the middle point between *)
(* the current and the final points *)
  dx2 := (x0 - x2) / 2.0;
  dy2 := (y0 - y2) / 2.0;

(* Convert angle from degrees to radians *)
  cos_a := mat.cos(angle);
  sin_a := mat.sin(angle);

(* Calculate (x1, y1) *)
  x1 :=  cos_a * dx2 + sin_a * dy2;
  y1 := -sin_a * dx2 + cos_a * dy2;

(* Ensure radii are large enough *)
  prx := rx * rx;
  pry := ry * ry;
  px1 := x1 * x1;
  py1 := y1 * y1;

(* Check that radii are large enough *)
  radii_check := px1 / prx + py1 / pry;

  IF radii_check > 1.0 THEN
    rx  := mat.sqrt(radii_check) * rx;
    ry  := mat.sqrt(radii_check) * ry;
    prx := rx * rx;
    pry := ry * ry;

    IF radii_check > 10.0 THEN
      bav.radii_ok := FALSE;
    END;
  END;

(* Calculate (cx1, cy1) *)
  IF large_arc_flag = sweep_flag THEN
    sign := -1.0
  ELSE
    sign := 1.0;
  END;

  sq := (prx * pry - prx * py1 - pry * px1) / (prx * py1 + pry * px1);

  IF sq < 0 THEN
    coef := sign * mat.sqrt(0)
  ELSE
    coef := sign * mat.sqrt(sq);
  END;

  cx1 := coef *   ((rx * y1) / ry);
  cy1 := coef * (-((ry * x1) / rx));

(* Calculate (cx, cy) from (cx1, cy1) *)
  sx2 := (x0 + x2) / 2.0;
  sy2 := (y0 + y2) / 2.0;
  cx  := sx2 + (cos_a * cx1 - sin_a * cy1);
  cy  := sy2 + (sin_a * cx1 + cos_a * cy1);

(* Calculate the start_angle (angle1) and the sweep_angle (dangle) *)
  ux :=  (x1 - cx1) / rx;
  uy :=  (y1 - cy1) / ry;
  vx := (-x1 - cx1) / rx;
  vy := (-y1 - cy1) / ry;

(* Calculate the angle start *)
  n:= mat.sqrt(ux * ux + uy * uy);
  p:= ux; (* (1 * ux) + (0 * uy) *)

  IF uy < 0 THEN
    sign := -1.0
  ELSE
    sign := 1.0;
  END;

  v := p / n;

  IF v < -1.0 THEN
    v := -1.0;
  END;

  IF v > 1.0 THEN
    v:=1.0;
  END;

  start_angle := sign * mat.arccos(v);

(* Calculate the sweep angle *)
  n := mat.sqrt((ux * ux + uy * uy) * (vx * vx + vy * vy));
  p := ux * vx + uy * vy;

  IF ux * vy - uy * vx < 0 THEN
    sign := -1.0
  ELSE
    sign := 1.0;
  END;

  v := p / n;

  IF v < -1.0 THEN
    v := -1.0;
  END;

  IF v > 1.0 THEN
    v := 1.0;
  END;

  sweep_angle := sign * mat.arccos(v);

  IF (~sweep_flag) & (sweep_angle > 0) THEN
    sweep_angle := sweep_angle - bas.pi * 2.0
  ELSE
    IF sweep_flag & (sweep_angle < 0) THEN
      sweep_angle := sweep_angle + bas.pi * 2.0;
    END;
  END;

(* We can now build and transform the resulting arc *)
  bav.init(0.0, 0.0, rx, ry, start_angle, sweep_angle);

  NEW(mtx);
  mtx.ConstructR(angle);
  NEW(trn);
  trn.ConstructT(cx, cy);

  mtx.multiply(trn);

  i:=2;

  WHILE i < bav.num_vertices - 2 DO
    mtx.transform(mtx, bav.vertices[i], bav.vertices[i+1]);
    INC(i ,2);
  END;

(* We must make sure that the starting and ending points *)
(* exactly coincide with the initial (x0,y0) and (x2,y2) *)
  bav.vertices[0] := x0;
  bav.vertices[1] := y0;

  IF bav.num_vertices > 2 THEN
    bav.vertices[bav.num_vertices - 2] := x2;
    bav.vertices[bav.num_vertices - 1] := y2;
  END;
END initBASVG;

PROCEDURE (bav: bezier_arc_svg_ptr) ConstructBASVG*(x1, y1, rx, ry, angle: bas.double; large_arc_flag, sweep_flag: BOOLEAN; x2, y2: bas.double);
BEGIN
 bav.Construct();
 bav.initBASVG(x1, y1, rx, ry, angle, large_arc_flag, sweep_flag, x2, y2);
END ConstructBASVG;


END AggBezierArc.
