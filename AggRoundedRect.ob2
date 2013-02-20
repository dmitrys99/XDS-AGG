MODULE AggRoundedRect;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource,
  arc := AggArc;

TYPE

  rounded_rect_ptr* = POINTER TO rounded_rect;
  rounded_rect* = RECORD(avs.vertex_source)
    x1,
    y1,
    x2,
    y2,
    rx1,
    ry1,
    rx2,
    ry2,
    rx3,
    ry3,
    rx4,
    ry4: bas.double;

    status: bas.int32;
    arc: arc.arc_ptr;
  END;

PROCEDURE (rr: rounded_rect_ptr) Construct*();
BEGIN
  rr.x1  := 0;
  rr.y1  := 0;
  rr.x2  := 0;
  rr.y2  := 0;
  rr.rx1 := 0;
  rr.ry1 := 0;
  rr.rx2 := 0;
  rr.ry2 := 0;
  rr.rx3 := 0;
  rr.ry3 := 0;
  rr.rx4 := 0;
  rr.ry4 := 0;

  rr.status:=0;

  NEW(rr.arc);
  rr.arc.Construct();
END Construct;

PROCEDURE (rr: rounded_rect_ptr) ConstructRR*(x1, y1, x2, y2, r: bas.double);
BEGIN
  rr.Construct();

  rr.x1  := x1;
  rr.y1  := y1;
  rr.x2  := x2;
  rr.y2  := y2;
  rr.rx1 := r;
  rr.ry1 := r;
  rr.rx2 := r;
  rr.ry2 := r;
  rr.rx3 := r;
  rr.ry3 := r;
  rr.rx4 := r;
  rr.ry4 := r;

  IF x1 > x2 THEN
    rr.x1 := x2;
    rr.x2 := x1;
  END;

  IF y1 > y2 THEN
    rr.y1 := y2;
    rr.y2 := y1;
  END;
END ConstructRR;

PROCEDURE (rr: rounded_rect_ptr) rect*(x1, y1, x2, y2: bas.double);
BEGIN
  rr.x1 := x1;
  rr.y1 := y1;
  rr.x2 := x2;
  rr.y2 := y2;

  IF x1 > x2 THEN
    rr.x1 := x2;
    rr.x2 := x1;
  END;

  IF y1 > y2 THEN
    rr.y1 := y2;
    rr.y2 := y1;
  END;
END rect;

PROCEDURE (rr: rounded_rect_ptr) radius*(r: bas.double);
BEGIN
  rr.rx1 := r;
  rr.ry1 := r;
  rr.rx2 := r;
  rr.ry2 := r;
  rr.rx3 := r;
  rr.ry3 := r;
  rr.rx4 := r;
  rr.ry4 := r;
END radius;

PROCEDURE (rr: rounded_rect_ptr) radius1*(rx, ry: bas.double);
BEGIN
  rr.rx1 := rx;
  rr.rx2 := rx;
  rr.rx3 := rx;
  rr.rx4 := rx;
  rr.ry1 := ry;
  rr.ry2 := ry;
  rr.ry3 := ry;
  rr.ry4 := ry;
END radius1;

PROCEDURE (rr: rounded_rect_ptr) radius2*(rx_bottom, ry_bottom, rx_top, ry_top: bas.double);
BEGIN
  rr.rx1 := rx_bottom;
  rr.rx2 := rx_bottom;
  rr.rx3 := rx_top;
  rr.rx4 := rx_top;
  rr.ry1 := ry_bottom;
  rr.ry2 := ry_bottom;
  rr.ry3 := ry_top;
  rr.ry4 := ry_top;
END radius2;


PROCEDURE (rr: rounded_rect_ptr) radius3*(rx1, ry1, rx2, ry2, rx3, ry3, rx4, ry4: bas.double);
BEGIN
  rr.rx1 := rx1;
  rr.ry1 := ry1;
  rr.rx2 := rx2;
  rr.ry2 := ry2;
  rr.rx3 := rx3;
  rr.ry3 := ry3;
  rr.rx4 := rx4;
  rr.ry4 := ry4;
END radius3;

PROCEDURE (rr: rounded_rect_ptr) normalize_radius*();
VAR
  dx, dy, k, t: bas.double;
BEGIN
  dx := ABS(rr.y2 - rr.y1);
  dy := ABS(rr.x2 - rr.x1);

  k := 1.0;

  IF ~bas.is_equal_eps(rr.rx1 + rr.rx2, 0.0, bas.double_epsilon) THEN
    t := dx / (rr.rx1 + rr.rx2);

    IF t < 1.0 THEN
      k := t;
    END;
  END;

  IF ~bas.is_equal_eps(rr.rx3 + rr.rx4, 0.0, bas.double_epsilon) THEN
    t := dx / (rr.rx3 + rr.rx4);

    IF t < k THEN
      k := t;
    END;
  END;

  IF ~bas.is_equal_eps(rr.ry1 + rr.ry2, 0.0, bas.double_epsilon) THEN
    t := dy / (rr.ry1 + rr.ry2);

    IF t < k THEN
      k := t;
    END;
  END;

  IF ~bas.is_equal_eps(rr.ry3 + rr.ry4, 0.0, bas.double_epsilon) THEN
    t := dy / (rr.ry3 + rr.ry4);

    IF t < k THEN
      k := t;
    END;
  END;

  IF k < 1.0 THEN
    rr.rx1 := rr.rx1 * k;
    rr.ry1 := rr.ry1 * k;
    rr.rx2 := rr.rx2 * k;
    rr.ry2 := rr.ry2 * k;
    rr.rx3 := rr.rx3 * k;
    rr.ry3 := rr.ry3 * k;
    rr.rx4 := rr.rx4 * k;
    rr.ry4 := rr.ry4 * k;
  END;

END normalize_radius;

PROCEDURE (rr: rounded_rect_ptr) set_approximation_scale*(scale: bas.double);
BEGIN
  rr.arc.set_approximation_scale(scale);
END set_approximation_scale;

PROCEDURE (rr: rounded_rect_ptr) approximation_scale*(): bas.double;
BEGIN
  RETURN rr.arc.approximation_scale();
END approximation_scale;

<*+WOFF301*>
PROCEDURE (rr: rounded_rect_ptr) rewind*(path_id: bas.int32u);
BEGIN
  rr.status := 0;
END rewind;
<*-WOFF301*>

PROCEDURE (rr: rounded_rect_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  cmd: SET;
BEGIN
  cmd := bas.path_cmd_stop;

  IF rr.status = 0 THEN
    rr.arc.init(rr.x1 + rr.rx1, rr.y1 + rr.ry1, rr.rx1, rr.ry1, bas.pi, bas.pi + bas.pi * 0.5, TRUE);
    rr.arc.rewind(0);
    INC(rr.status);
  END;

  IF rr.status = 1 THEN
      cmd := rr.arc.vertex(x, y);
      IF (bas.is_stop(cmd)) THEN
        INC(rr.status);
      ELSE
        RETURN cmd;
      END;
  END;

  IF rr.status = 2 THEN
     rr.arc.init(rr.x2 - rr.rx2, rr.y1 + rr.ry2, rr.rx2, rr.ry2, bas.pi + bas.pi*0.5, 0.0, TRUE);
     rr.arc.rewind(0);
     INC(rr.status);
  END;

  IF rr.status = 3 THEN
    cmd := rr.arc.vertex(x, y);
    IF(bas.is_stop(cmd)) THEN
      INC(rr.status);
    ELSE
      RETURN bas.path_cmd_line_to;
    END;
  END;

  IF rr.status = 4 THEN
    rr.arc.init(rr.x2 - rr.rx3, rr.y2 - rr.ry3, rr.rx3, rr.ry3, 0.0, bas.pi*0.5, TRUE);
    rr.arc.rewind(0);
    INC(rr.status);
  END;

  IF rr.status = 5 THEN
    cmd := rr.arc.vertex(x, y);
    IF(bas.is_stop(cmd)) THEN
      INC(rr.status);
    ELSE
      RETURN bas.path_cmd_line_to;
    END;
  END;

  IF rr.status = 6 THEN
    rr.arc.init(rr.x1 + rr.rx4, rr.y2 - rr.ry4, rr.rx4, rr.ry4, bas.pi*0.5, bas.pi, TRUE);
    rr.arc.rewind(0);
    INC(rr.status);
  END;

  IF rr.status = 7 THEN
    cmd := rr.arc.vertex(x, y);
    IF (bas.is_stop(cmd)) THEN
      INC(rr.status);
    ELSE
      RETURN bas.path_cmd_line_to;
    END;
  END;

  IF rr.status = 8 THEN
    cmd := bas.path_cmd_end_poly + bas.path_flags_close + bas.path_flags_ccw;
    INC(rr.status);
  END;

  RETURN cmd;
END vertex;

END AggRoundedRect.