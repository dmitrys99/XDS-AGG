MODULE AggRendererPrimitives;

IMPORT
  bas := AggBasics,
  col := AggColor,
  pfb := AggPixfmtBase,
  dda := AggDDALine,
  ren := AggRendererBase,
  aeb := AggEllipseBresenham;

TYPE
  renderer_primitives_ptr* = POINTER TO renderer_primitives;
  renderer_primitives* = RECORD (ren.renderer_base)
    fill_color*,
    line_color*: col.aggclr;
    curr_x, curr_y: bas.int32;
  END;

PROCEDURE (rp: renderer_primitives_ptr) coord*(c: bas.double): bas.int32;
BEGIN
  RETURN ENTIER(c * dda.subpixel_size);
END coord;

PROCEDURE (rp: renderer_primitives_ptr) Construct*(pf: pfb.pixel_formats_ptr);
BEGIN
  ASSERT(pf # NIL);

  rp.Construct^(pf);

  rp.fill_color.ConstructI();
  rp.line_color.ConstructI();

  rp.curr_x := 0;
  rp.curr_y := 0;
END Construct;

PROCEDURE (rp: renderer_primitives_ptr) rectangle*(x1, y1, x2, y2: bas.int32);
BEGIN
  rp.blend_hline(x1,     y1,     x2 - 1, rp.line_color, bas.cover_full);
  rp.blend_vline(x2,     y1,     y2 - 1, rp.line_color, bas.cover_full);
  rp.blend_hline(x1 + 1, y2,     x2,     rp.line_color, bas.cover_full);
  rp.blend_vline(x1,     y1 + 1, y2,     rp.line_color, bas.cover_full);
END rectangle;

PROCEDURE (rp: renderer_primitives_ptr) solid_rectangle*(x1, y1, x2, y2: bas.int32);
BEGIN
  rp.blend_bar(x1, y1, x2, y2, rp.fill_color, bas.cover_full);
END solid_rectangle;

PROCEDURE (rp: renderer_primitives_ptr) outlined_rectangle*(x1, y1, x2, y2: bas.int32);
BEGIN
  rp.rectangle(x1, y1, x2, y2);
  rp.solid_rectangle(x1 + 1, y1 + 1, x2 - 1, y2 - 1);
END outlined_rectangle;

PROCEDURE (rp: renderer_primitives_ptr) ellipse*(x, y, rx, ry: bas.int32);
VAR
  ei: aeb.ellipse_bresenham_interpolator;
  dx, dy: bas.int32;
BEGIN
  ei.Construct(rx, ry);

  dx := 0;
  dy := -ry;
  REPEAT
    INC(dx, ei.dx);
    INC(dy, ei.dy);

    rp.blend_pixel(x + dx, y + dy, rp.line_color, bas.cover_full);
    rp.blend_pixel(x + dx, y - dy, rp.line_color, bas.cover_full);
    rp.blend_pixel(x - dx, y - dy, rp.line_color, bas.cover_full);
    rp.blend_pixel(x - dx, y + dy, rp.line_color, bas.cover_full);

    ei.inc_operator;

  UNTIL dy >= 0;
END ellipse;

(* solid_ellipse *)

PROCEDURE (rp: renderer_primitives_ptr) solid_ellipse*(x, y, rx, ry: bas.int32);
VAR
 ei: aeb.ellipse_bresenham_interpolator;

 dx, dy, dx0, dy0: bas.int32;
BEGIN
  ei.Construct(rx, ry);

  dx  := 0;
  dy  := -ry;
  dy0 := dy;
  dx0 := 0;

  REPEAT
    INC(dx, ei.dx);
    INC(dy, ei.dy);

    IF dy # dy0 THEN
      rp.blend_hline(x - dx0, y + dy0, x + dx0, rp.fill_color, bas.cover_full);
      rp.blend_hline(x - dx0, y - dy0, x + dx0, rp.fill_color, bas.cover_full);
    END;

    dx0 := dx;
    dy0 := dy;

    ei.inc_operator();

  UNTIL dy >= 0;

  rp.blend_hline(x - dx0, y + dy0, x + dx0, rp.fill_color, bas.cover_full);
END solid_ellipse;

PROCEDURE (rp: renderer_primitives_ptr) outlined_ellipse*(x, y, rx, ry: bas.int32);
VAR
  ei: aeb.ellipse_bresenham_interpolator;
  dx, dy: bas.int32;
BEGIN
  ei.Construct(rx, ry);

  dx := 0;
  dy := -ry;

  REPEAT
    INC(dx, ei.dx);
    INC(dy, ei.dy);

    rp.blend_pixel(x + dx, y + dy, rp.line_color, bas.cover_full);
    rp.blend_pixel(x + dx, y - dy, rp.line_color, bas.cover_full);
    rp.blend_pixel(x - dx, y - dy, rp.line_color, bas.cover_full);
    rp.blend_pixel(x - dx, y + dy, rp.line_color, bas.cover_full);

    IF (ei.dy # 0) & (dx # 0) THEN
      rp.blend_hline(x - dx + 1, y + dy, x + dx - 1, rp.fill_color, bas.cover_full);
      (* »збегаем двойного рисовани€ центральной линии *)
      IF dy # 0 THEN
        rp.blend_hline(x - dx + 1, y - dy, x + dx - 1, rp.fill_color, bas.cover_full);
      END;
    END;

    ei.inc_operator();

  UNTIL dy >= 0;
END outlined_ellipse;

PROCEDURE (rp: renderer_primitives_ptr) line*(x1, y1, x2, y2: bas.int32; last: BOOLEAN);
VAR
 li : dda.line_bresenham_interpolator;
 len: bas.int32;
BEGIN
  li.Construct(x1, y1, x2, y2);

  len := li.len;

  IF len = 0 THEN
    IF last THEN
      rp.blend_pixel(li.line_lr(x1), li.line_lr(y1), rp.line_color, bas.cover_full);
    END;

    RETURN;
  END;

  IF last THEN
    INC(len);
  END;

  IF li.ver THEN
    REPEAT
     rp.blend_pixel(li.x2(), li.y1(), rp.line_color, bas.cover_full);

     li.vstep();

     DEC(len);
    UNTIL len = 0
  ELSE
    REPEAT
      rp.blend_pixel(li.x1(), li.y2(), rp.line_color, bas.cover_full);

      li.hstep();

      DEC(len);
    UNTIL len = 0;
  END;
END line;

PROCEDURE (rp: renderer_primitives_ptr) move_to*(x, y: bas.int32);
BEGIN
  rp.curr_x := x;
  rp.curr_y := y;
END move_to;

PROCEDURE (rp: renderer_primitives_ptr) line_to1*(x, y: bas.int32; last: BOOLEAN);
BEGIN
  rp.line(rp.curr_x, rp.curr_y, x, y, last);
  rp.curr_x := x;
  rp.curr_y := y;
END line_to1;
PROCEDURE (rp: renderer_primitives_ptr) line_to*(x, y: bas.int32);
BEGIN
  rp.line_to1(x, y, FALSE);
END line_to;

END AggRendererPrimitives.