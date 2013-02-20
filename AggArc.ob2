MODULE AggArc;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource,
  mat := MathL;

TYPE
  arc_ptr* = POINTER TO arc;
  arc* = RECORD(avs.vertex_source);
    x,
    y,
    rx,
    ry,
    angle,
    start,
    end,
    scale,
    da: bas.double;

    ccw,
    initialized: BOOLEAN;

    path_cmd: SET;
  END;

PROCEDURE (a: arc_ptr) Construct*();
BEGIN
  a.x     := 0;
  a.y     := 0;
  a.rx    := 0;
  a.ry    := 0;
  a.angle := 0;
  a.start := 0;
  a.end   := 0;
  a.da    := 0;

  a.ccw      := FALSE;
  a.path_cmd := {};
  a.scale := 1;

  a.initialized := FALSE;
END Construct;

PROCEDURE (a: arc_ptr) normalize*(a1, a2: bas.double; ccw: BOOLEAN);
VAR
  ra: bas.double;
BEGIN
  ra   :=(ABS(a.rx) + ABS(a.ry)) / 2;
  a.da :=mat.arccos(ra / (ra + 0.125 / a.scale)) * 2;

  IF ccw THEN
    WHILE a2 < a1 DO
      a2:=a2 + (bas.pi * 2.0)
    END
  ELSE
    WHILE a1 < a2 DO
      a1 := a1 + (bas.pi * 2.0);
    END;
    a.da := -a.da;
  END;

  a.ccw   := ccw;
  a.start := a1;
  a.end   := a2;

  a.initialized := TRUE;
END normalize;

PROCEDURE (a: arc_ptr) ConstructArc*(x, y, rx, ry, a1, a2: bas.double; ccw: BOOLEAN);
BEGIN
  a.Construct();

  a.x  := x;
  a.y  := y;
  a.rx := rx;
  a.ry := ry;

  a.scale := 1.0;

  a.normalize(a1, a2, ccw);
END ConstructArc;

PROCEDURE (a: arc_ptr) init*(x, y, rx, ry, a1, a2: bas.double; ccw: BOOLEAN);
BEGIN
  a.x  := x;
  a.y  := y;
  a.rx := rx;
  a.ry := ry;

  a.normalize(a1, a2, ccw);
END init;

PROCEDURE (a: arc_ptr) set_approximation_scale*(scale: bas.double);
BEGIN
  a.scale := scale;

  IF a.initialized THEN
    a.normalize(a.start, a.end, a.ccw);
  END;
END set_approximation_scale;

PROCEDURE (a: arc_ptr) approximation_scale*(): bas.double;
BEGIN
  RETURN a.scale;
END approximation_scale;

<*+WOFF301*>
PROCEDURE (a: arc_ptr) rewind*(path_id: bas.int32u);
BEGIN
  a.path_cmd := bas.path_cmd_move_to;
  a.angle    := a.start;
END rewind;
<*-WOFF301*>

PROCEDURE (a: arc_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  pf: SET;
BEGIN
  IF bas.is_stop(a.path_cmd) THEN
    RETURN bas.path_cmd_stop
  ELSE
    IF (a.angle < a.end - a.da / 4 ) # a.ccw THEN
      x := a.x + mat.cos(a.end) * a.rx;
      y := a.y + mat.sin(a.end) * a.ry;

      a.path_cmd := bas.path_cmd_stop;

      RETURN bas.path_cmd_line_to;
    ELSE
      x := a.x + mat.cos(a.angle) * a.rx;
      y := a.y + mat.sin(a.angle) * a.ry;

      a.angle := a.angle + a.da;

      pf         := a.path_cmd;
      a.path_cmd := bas.path_cmd_line_to;

      RETURN pf;
    END;
  END;
END vertex;


END AggArc.