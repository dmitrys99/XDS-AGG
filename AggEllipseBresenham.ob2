MODULE AggEllipseBresenham;

IMPORT
  bas := AggBasics;

TYPE
  ellipse_bresenham_interpolator* = RECORD
    rx2,
    ry2,
    two_rx2,
    two_ry2,
    dx-,
    dy-,
    inc_x,
    inc_y,
    cur_f: bas.int32;
  END;

PROCEDURE (VAR ei: ellipse_bresenham_interpolator) Construct*(rx, ry: bas.int32);
BEGIN
 ei.rx2 := rx * rx;
 ei.ry2 := ry * ry;

 ei.two_rx2 := ASH(ei.rx2, 1);
 ei.two_ry2 := ASH(ei.ry2, 1);

 ei.dx := 0;
 ei.dy := 0;

 ei.inc_x := 0;
 ei.inc_y := -ry * ei.two_rx2;
 ei.cur_f := 0;
END Construct;

PROCEDURE (VAR ei: ellipse_bresenham_interpolator) inc_operator*();
VAR
  mx, my, mxy, min_m, fx, fy, fxy: bas.int32;
  flag: BOOLEAN;
BEGIN
  mx := ei.cur_f + ei.inc_x + ei.ry2;
  fx := mx;

  IF mx < 0 THEN
    mx := -mx;
  END;

  my := ei.cur_f + ei.inc_y + ei.rx2;
  fy := my;

  IF my < 0 THEN
    my := -my;
  END;

  mxy := ei.cur_f + ei.inc_x + ei.ry2 + ei.inc_y + ei.rx2;
  fxy := mxy;

  IF mxy < 0 THEN
    mxy:=-mxy;
  END;

  min_m := mx;
  flag := TRUE;

  IF min_m > my THEN
    min_m := my;
    flag  := FALSE;
  END;

  ei.dx:=0;
  ei.dy:=0;

  IF min_m > mxy THEN
    INC(ei.inc_x, ei.two_ry2);
    INC(ei.inc_y, ei.two_rx2);

    ei.cur_f := fxy;

    ei.dx := 1;
    ei.dy := 1;

    RETURN;
  END;

  IF flag THEN
    INC(ei.inc_x, ei.two_ry2);

    ei.cur_f := fx;
    ei.dx    := 1;

    RETURN;
  END;

 INC(ei.inc_y, ei.two_rx2);

 ei.cur_f := fy;
 ei.dy    := 1;
END inc_operator;


END AggEllipseBresenham.