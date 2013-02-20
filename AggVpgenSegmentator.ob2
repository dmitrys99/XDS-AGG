MODULE AggVpgenSegmentator;

IMPORT 
  bas := AggBasics,
  avs := AggVertexSource,
  math:= MathL;

TYPE
  vpgen_segmentator_ptr* = POINTER TO vpgen_segmentator; 
  vpgen_segmentator* = RECORD(avs.vertex_source)
    an_approximation_scale,

    x1,
    y1,
    dx,
    dy,
    dl,

    ddl: bas.double;
    cmd: SET;
  END;

PROCEDURE (vp: vpgen_segmentator_ptr) Construct*();
BEGIN
  vp.an_approximation_scale := 1.0;

  vp.x1  := 0;
  vp.y1  := 0;
  vp.dx  := 0;
  vp.dy  := 0;
  vp.dl  := 0;
  vp.ddl := 0;
  vp.cmd := {};
END Construct;

PROCEDURE (vp: vpgen_segmentator_ptr) set_approximation_scale*(scale: bas.double);
BEGIN
  vp.an_approximation_scale := scale;
END set_approximation_scale;

PROCEDURE (vp: vpgen_segmentator_ptr) approximation_scale*(): bas.double;
BEGIN
  RETURN vp.an_approximation_scale;
END approximation_scale;

PROCEDURE (vp: vpgen_segmentator_ptr) reset*();
BEGIN
  vp.cmd := bas.path_cmd_stop;
END reset;

PROCEDURE (vp: vpgen_segmentator_ptr) move_to*(x, y: bas.double);
BEGIN
  vp.x1  :=   x;
  vp.y1  :=   y;
  vp.dx  := 0.0;
  vp.dy  := 0.0;
  vp.dl  := 2.0;
  vp.ddl := 2.0;
  vp.cmd := bas.path_cmd_move_to;
END move_to;

PROCEDURE (vp: vpgen_segmentator_ptr) line_to*(x, y: bas.double);
VAR
  len: bas.double;
BEGIN
 vp.x1 := vp.x1 + vp.dx;
 vp.y1 := vp.y1 + vp.dy;
 vp.dx := x - vp.x1;
 vp.dy := y - vp.y1;

 len := math.sqrt(vp.dx * vp.dx + vp.dy * vp.dy ) * vp.an_approximation_scale;

 IF len < 1.0E-30 THEN
  len := 1.0E-30;
 END;

 vp.ddl := 1.0 / len;

 IF vp.cmd = bas.path_cmd_move_to THEN
  vp.dl := 0.0
 ELSE
  vp.dl := vp.ddl;
 END;

 IF vp.cmd = bas.path_cmd_stop THEN
   vp.cmd := bas.path_cmd_line_to;
 END;
END line_to;

PROCEDURE (vp: vpgen_segmentator_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  cmd: SET;
BEGIN
  IF vp.cmd = bas.path_cmd_stop THEN
    RETURN bas.path_cmd_stop
  ELSE
    cmd    := vp.cmd;
    vp.cmd := bas.path_cmd_line_to;

    IF vp.dl >= 1.0 - vp.ddl THEN
      vp.dl  := 1.0;
      vp.cmd := bas.path_cmd_stop;

      x := vp.x1 + vp.dx;
      y := vp.y1 + vp.dy;

      RETURN cmd;
    ELSE
      x := vp.x1 + vp.dx * vp.dl;
      y := vp.y1 + vp.dy * vp.dl;

      vp.dl := vp.dl + vp.ddl;

      RETURN cmd;
    END;
  END;
END vertex;

END AggVpgenSegmentator.