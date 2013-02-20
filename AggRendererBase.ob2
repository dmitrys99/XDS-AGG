MODULE AggRendererBase;

IMPORT
  bas := AggBasics,
  pfb := AggPixfmtBase,
  col := AggColor,
  rb  := AggRenderingBuffer;


TYPE
  renderer_base_ptr* = POINTER TO renderer_base;
  renderer_base* = RECORD
    pf      - : pfb.pixel_formats_ptr;
    clipbox - : bas.rect;
  END;

PROCEDURE (rb: renderer_base_ptr) Construct*(pf: pfb.pixel_formats_ptr);
VAR
 w, h: bas.int32;
BEGIN
  ASSERT(pf # NIL);
  rb.pf := pf;

  IF rb.pf.pixel_width() > 0 THEN
    w := rb.pf.pixel_width() - 1
  ELSE
    w := 0;
  END;

  IF rb.pf.pixel_height() > 0 THEN
    h := rb.pf.pixel_height() - 1
  ELSE
    h := 0;
  END;

  rb.clipbox.Construct(0, 0, w, h);
END Construct;

(* pixel_width *)

PROCEDURE (rb: renderer_base_ptr) pixel_width*(): bas.int32;
BEGIN
  RETURN rb.pf.pixel_width();
END pixel_width;

(* pixel_height *)

PROCEDURE (rb: renderer_base_ptr) pixel_height*(): bas.int32;
BEGIN
  RETURN rb.pf.pixel_height();
END pixel_height;

(* set_clip_box *)

PROCEDURE (rb: renderer_base_ptr) set_clip_box*(x1, y1, x2, y2: bas.int32): BOOLEAN;
VAR
  cb, rc: bas.rect;
BEGIN
  cb.Construct(x1, y1, x2, y2);
  cb.Normalize();

  rc.Construct(0, 0, rb.pixel_width() - 1, rb.pixel_height() - 1);
  IF cb.Clip(rc) THEN
    rb.clipbox := cb;
    RETURN TRUE;
  ELSE
    rb.clipbox.x1 := 1;
    rb.clipbox.y1 := 1;
    rb.clipbox.x2 := 0;
    rb.clipbox.y2 := 0;
    RETURN FALSE;
  END;
END set_clip_box;

(* set_clip_box_naked *)

PROCEDURE (rb: renderer_base_ptr) set_clip_box_naked*(x1, y1, x2, y2: bas.int32);
BEGIN
  rb.clipbox.x1 := x1;
  rb.clipbox.y1 := y1;
  rb.clipbox.x2 := x2;
  rb.clipbox.y2 := y2;
END set_clip_box_naked;

(* inbox *)

PROCEDURE (rb: renderer_base_ptr) inbox*(x, y: bas.int32): BOOLEAN;
BEGIN
 RETURN
  (x >= rb.clipbox.x1) &
  (y >= rb.clipbox.y1) &
  (x <= rb.clipbox.x2) &
  (y <= rb.clipbox.y2) ;
END inbox;

(* (x|y)(max|min) *)

PROCEDURE (rb: renderer_base_ptr) xmin*(): bas.int32;
BEGIN
  RETURN rb.clipbox.x1;
END xmin;

PROCEDURE (rb: renderer_base_ptr) xmax*(): bas.int32;
BEGIN
  RETURN rb.clipbox.x2;
END xmax;

PROCEDURE (rb: renderer_base_ptr) ymin*(): bas.int32;
BEGIN
  RETURN rb.clipbox.y1;
END ymin;

PROCEDURE (rb: renderer_base_ptr) ymax*(): bas.int32;
BEGIN
  RETURN rb.clipbox.y2;
END ymax;

PROCEDURE (rb: renderer_base_ptr) bounding_xmin*(): bas.int32;
BEGIN
  RETURN rb.clipbox.x1;
END bounding_xmin;

(* bounding_(x|y)(max|min) *)

PROCEDURE (rb: renderer_base_ptr) bounding_xmax*(): bas.int32;
BEGIN
  RETURN rb.clipbox.x2;
END bounding_xmax;

PROCEDURE (rb: renderer_base_ptr) bounding_ymin*(): bas.int32;
BEGIN
  RETURN rb.clipbox.y1;
END bounding_ymin;

PROCEDURE (rb: renderer_base_ptr) bounding_ymax*(): bas.int32;
BEGIN
  RETURN rb.clipbox.y2;
END bounding_ymax;

PROCEDURE (rb: renderer_base_ptr) bounding_box*(VAR b: bas.rect);
BEGIN
  b := rb.clipbox;
END bounding_box;

(* clear *)

PROCEDURE (rb: renderer_base_ptr) clear*(c: col.aggclr);
VAR
  y: bas.int32;
BEGIN
  IF (rb.pixel_width() > 0) & (rb.pixel_height() > 0) THEN
    FOR y := 0 TO rb.pixel_height() - 1 DO
      rb.pf.copy_hline(0, y, rb.pixel_width(), c);
    END;
  END;
END clear;

(* copy_pixel *)

PROCEDURE (rb: renderer_base_ptr) copy_pixel*(x, y: bas.int32; c: col.aggclr);
BEGIN
  IF rb.inbox(x,y) THEN
    rb.pf.copy_pixel(x, y, c);
  END;
END copy_pixel;

(* blend_pixel *)

PROCEDURE (rb: renderer_base_ptr) blend_pixel*(x, y: bas.int32; c: col.aggclr; cover: bas.int8u);
BEGIN
  IF rb.inbox(x,y) THEN
    rb.pf.blend_pixel(x, y, c, cover);
  END;
END blend_pixel;

(* pixel *)

PROCEDURE (rb: renderer_base_ptr) pixel*(x, y: bas.int32; VAR c: col.aggclr);
BEGIN
  IF rb.inbox(x,y) THEN
    rb.pf.pixel(x,y,c);
  ELSE
    c.ConstructI();
  END;
END pixel;

(* copy_hline *)

PROCEDURE (rb: renderer_base_ptr) copy_hline*(x1, y, x2: bas.int32; c: col.aggclr);
VAR
  t: bas.int32;
BEGIN
  IF x1 > x2 THEN
    t  := x2;
    x2 := x1;
    x1 := t ;
  END;
  IF (y > rb.ymax()) OR (y < rb.ymin()) OR (x1 > rb.xmax()) OR (x1 < rb.xmin()) OR (x2 > rb.xmax()) OR (x2 < rb.xmin()) THEN
    RETURN;
  END;

  rb.pf.copy_hline(x1, y, x2 - x1 + 1, c);
END copy_hline;

(* copy_vline *)

PROCEDURE (rb: renderer_base_ptr) copy_vline*(x, y1, y2: bas.int32; c: col.aggclr);
VAR
  t: bas.int32;
BEGIN
  IF y1 > y2 THEN
    t  := y2;
    y2 := y1;
    y1 := t ;
  END;
  IF (y1 > rb.ymax()) OR (y1 < rb.ymin()) OR (x > rb.xmax()) OR (x < rb.xmin()) OR (y2 > rb.ymax()) OR (y2 < rb.ymin()) THEN
    RETURN;
  END;

  rb.pf.copy_vline(x, y1, y2 - y1 + 1, c);
END copy_vline;

(* blend_hline *)

PROCEDURE (rb: renderer_base_ptr) blend_hline*(x1, y, x2: bas.int32; c: col.aggclr; cover: bas.int8u);
VAR
  t: bas.int32;
BEGIN
  IF x1 > x2 THEN
    t  := x2;
    x2 := x1;
    x1 := t ;
  END;


  IF y  > rb.ymax() THEN
    RETURN
  END;

  IF y  < rb.ymin() THEN
    RETURN
  END;

  IF x1 > rb.xmax() THEN
    RETURN
  END;

  IF x2 < rb.xmin() THEN
    RETURN
  END;

  IF x1 < rb.xmin() THEN
    x1 := rb.xmin();
  END;

 IF x2 > rb.xmax() THEN
   x2 := rb.xmax();
 END;

  rb.pf.blend_hline(x1, y, x2 - x1 + 1, c, cover);
END blend_hline;

(* blend_vline *)

PROCEDURE (rb: renderer_base_ptr) blend_vline*(x, y1, y2: bas.int32; c: col.aggclr; cover: bas.int8u);
VAR
  t: bas.int32;
BEGIN

  IF y1 > y2 THEN
    t  := y2;
    y2 := y1;
    y1 := t;
  END;

  IF x  > rb.xmax() THEN
    RETURN
  END;

  IF x  < rb.xmin() THEN
    RETURN
  END;

  IF y1 > rb.ymax() THEN
    RETURN
  END;

  IF y2 < rb.ymin() THEN
    RETURN
  END;

  IF y1 < rb.ymin() THEN
    y1 := rb.ymin();
  END;

  IF y2 > rb.ymax() THEN
    y2 := rb.ymax();
  END;

  rb.pf.blend_vline(x, y1, y2 - y1 + 1, c, cover);
END blend_vline;

(* copy_bar *)

PROCEDURE (rb: renderer_base_ptr) copy_bar(x1, y1, x2, y2: bas.int32; c: col.aggclr);
VAR
 y  : bas.int32;
 rc : bas.rect;
BEGIN
  rc.Construct(x1, y1, x2, y2);
  rc.Normalize();

  IF rc.Clip(rb.clipbox) THEN
    y := rc.y1;
    WHILE y <= rc.y2 DO
      rb.pf.copy_hline(rc.x1, y, rc.x2 - rc.x1 + 1, c);
      INC(y);
    END;
  END;
END copy_bar;

(* blend_bar *)

PROCEDURE (rb: renderer_base_ptr) blend_bar*(x1, y1, x2, y2: bas.int32; c: col.aggclr; cover: bas.int8u);
VAR
 y  : bas.int32;
 rc : bas.rect;
BEGIN
  rc.Construct(x1, y1, x2, y2);
  rc.Normalize();

  IF rc.Clip(rb.clipbox) THEN
    y := rc.y1;
    WHILE y <= rc.y2 DO
      rb.pf.blend_hline(rc.x1, y, rc.x2 - rc.x1 + 1, c, cover);
      INC(y);
    END;
  END;
END blend_bar;

(* blend_solid_hspan *)

(* Т.к. при обработке scanline может получиться так, что массив covers начнется не с нуля, то "начало" массива сдвигается на covers_offset *)
PROCEDURE (rb: renderer_base_ptr) blend_solid_hspan*(x, y, len: bas.int32; c: col.aggclr; VAR covers: col.covers_array_ptr; VAR covers_offset: bas.int32);
VAR
  cov_offset: bas.int32;
BEGIN
  cov_offset := 0;

  IF y > rb.ymax() THEN
    RETURN
  END;

  IF y < rb.ymin() THEN
    RETURN
  END;

  IF x < rb.xmin() THEN
    cov_offset := rb.xmin() - x;
    DEC(len, rb.xmin() - x);
    IF len <= 0 THEN
      RETURN;
    END;
    x := rb.xmin();
  END;
  IF (x + len) > rb.xmax() THEN
    len := rb.xmax() - x + 1;
    IF len <= 0 THEN
      RETURN;
    END;
  END;

  rb.pf.blend_solid_hspan(x, y, len, c, covers, cov_offset + covers_offset);
END blend_solid_hspan;

(* blend_solid_vspan *)

PROCEDURE (rb: renderer_base_ptr) blend_solid_vspan*(x, y, len: bas.int32; c: col.aggclr; VAR covers: col.covers_array_ptr; VAR covers_offset: bas.int32);
VAR
  cov_offset: bas.int32;
BEGIN
  cov_offset := 0;

  IF (x > rb.xmax()) OR (x < rb.xmin()) THEN
    RETURN;
  END;

  IF y < rb.ymin() THEN
    cov_offset := rb.ymin() - y;
    DEC(len, rb.ymin() - y);
    IF len <= 0 THEN
      RETURN;
    END;
    y := rb.ymin();
  END;

  IF (y + len) > rb.ymax() THEN
    len := rb.ymax() - y + 1;
    IF len <= 0 THEN
      RETURN;
    END;
  END;

  rb.pf.blend_solid_vspan(x, y, len, c, covers, cov_offset + covers_offset);
END blend_solid_vspan;

(* blend_color_hspan *)

PROCEDURE (rb: renderer_base_ptr) blend_color_hspan*(x, y, len: bas.int32;
  VAR colors: col.colors_array_ptr; VAR covers: col.covers_array_ptr; cover: bas.int8u);
VAR
  offset: bas.int32;
BEGIN
  offset := 0;
  IF (y > rb.ymax()) OR (y < rb.ymin()) THEN
    RETURN;
  END;
  IF x < rb.xmin() THEN
    offset := rb.xmin() - x;
    DEC(len, rb.xmin() - x);
    IF len <= 0 THEN
      RETURN;
    END;
    x := rb.xmin();
  END;
  IF (x + len) > rb.xmax() THEN
    len := rb.xmax() - x + 1;
    IF len <= 0 THEN
      RETURN;
    END;
  END;

  rb.pf.blend_color_hspan(x, y, len, colors, offset, covers, offset, cover);
END blend_color_hspan;

(* blend_color_vspan *)

PROCEDURE (rb: renderer_base_ptr) blend_color_vspan*(x, y, len: bas.int32;
  VAR colors: col.colors_array_ptr; VAR covers: col.covers_array_ptr; cover: bas.int8u);
VAR
  offset: bas.int32;
BEGIN
  offset := 0;

  IF (x > rb.xmax()) OR (x < rb.xmin()) THEN
    RETURN;
  END;

  IF y < rb.ymin() THEN
    offset := rb.ymin() - y;
    DEC(len, rb.ymin() - y);
    IF len <= 0 THEN
      RETURN;
    END;
    y := rb.ymin();
  END;

  IF (y + len) > rb.ymax() THEN
    len := rb.ymax() - y + 1;
    IF len <= 0 THEN
      RETURN;
    END;
  END;

  rb.pf.blend_color_vspan(x, y, len, colors, offset, covers, offset, cover);
END blend_color_vspan;

(* blend_color_hspan_no_clip *)

PROCEDURE (rb: renderer_base_ptr) blend_color_hspan_no_clip*(x, y, len: bas.int32; VAR colors: col.colors_array_ptr; VAR covers: col.covers_array_ptr; cover: bas.int8u);
BEGIN
  rb.pf.blend_color_hspan(x, y, len, colors, 0, covers, 0, cover);
END blend_color_hspan_no_clip;

PROCEDURE (rb: renderer_base_ptr) blend_color_hspan_no_clip_offset*(x, y, len: bas.int32;
  VAR colors: col.colors_array_ptr; colors_offset: bas.int32;
  VAR covers: col.covers_array_ptr; covers_offset: bas.int32;
  cover: bas.int8u);
BEGIN
  rb.pf.blend_color_hspan(x, y, len, colors, colors_offset, covers, covers_offset, cover);
END blend_color_hspan_no_clip_offset;

(* blend_color_vspan_no_clip *)

PROCEDURE (rb: renderer_base_ptr) blend_color_vspan_no_clip*(x, y, len: bas.int32;
  VAR colors: col.colors_array_ptr; VAR covers: col.covers_array_ptr; cover: bas.int8u);
BEGIN
  rb.pf.blend_color_vspan(x, y, len, colors, 0, covers, 0, cover);
END blend_color_vspan_no_clip;

(* clip_renderer_base *)

PROCEDURE (rb: renderer_base_ptr) clip_rect_area*(dst, src: bas.rect; wsrc, hsrc: bas.int32; VAR res: bas.rect);
VAR
  rc, cb: bas.rect;
BEGIN
  rc.Construct(0, 0, 0, 0);
  cb := rb.clipbox;

  INC(cb.x2);
  INC(cb.y2);

  IF src.x1 < 0 THEN
    dst.x1 := dst.x1 - src.x1;
    src.x1 := 0;
  END;

  IF src.y1 < 0 THEN
    dst.y1 := dst.y1 - src.y1;
    src.y1 := 0;
  END;

  IF src.x2 > wsrc THEN
    src.x2 := wsrc;
  END;

  IF src.y2 > hsrc THEN
    src.y2 := hsrc;
  END;

  IF dst.x1 < cb.x1 THEN
    src.x1 := src.x1 + (cb.x1 - dst.x1);
    dst.x1 := cb.x1;
  END;

  IF dst.y1 < cb.y1 THEN
    src.y1 := src.y1 + (cb.y1 - dst.y1);
    dst.y1 := cb.y1;
  END;

  IF dst.x2 > cb.x2 THEN
    dst.x2 := cb.x2;
  END;

  IF dst.y2 > cb.y2 THEN
    dst.y2 := cb.y2;
  END;

  rc.x2 := dst.x2 - dst.x1;
  rc.y2 := dst.y2 - dst.y1;

  IF rc.x2 > (src.x2 - src.x1) THEN
    rc.x2 := src.x2 - src.x1;
  END;

  IF rc.y2 > src.y2 - src.y1 THEN
    rc.y2 := src.y2 - src.y1;
  END;

  res := rc;
END clip_rect_area;

PROCEDURE (rb: renderer_base_ptr) copy_from*(src: rb.rendering_buffer_ptr; rect_src: bas.rect; dx: bas.int32; dy: bas.int32);
VAR
  rsrc, rdst, rc: bas.rect;
  incy: bas.int32;
  pw, ph: bas.int32;
BEGIN
  ASSERT(src.byte_width MOD src.bppabs() = 0);

  pw := src.byte_width DIV src.bppabs();
  ph := src.byte_height DIV src.bppabs();

  rsrc.Construct(0, 0, pw, ph);

  IF rect_src.is_valid() THEN
    rsrc.x1 := rect_src.x1;
    rsrc.y1 := rect_src.y1;
    rsrc.x2 := rect_src.x2 + 1;
    rsrc.y2 := rect_src.y2 + 1;
  END;

  rdst.Construct(rsrc.x1 + dx, rsrc.y1 + dy, rsrc.x2 + dx, rsrc.y2 + dy);

  rb.clip_rect_area(rdst, rsrc, pw, ph, rc);

  IF rc.x2 > 0 THEN
    incy := 1;

    IF rdst.y1 > rsrc.y1 THEN
      rsrc.y1 := rsrc.y1 + (rc.y2 - 1);
      rdst.y1 := rdst.y1 + (rc.y2 - 1);
      incy := -1;
    END;

    WHILE rc.y2 > 0 DO
      rb.pf.copy_from(src, rdst.x1, rdst.y1, rsrc.x1, rsrc.y1, rc.x2);

      INC(rdst.y1, incy);
      INC(rsrc.y1, incy);

      DEC(rc.y2);
    END;
  END;
END copy_from;

(** blend_from  ТРЕБУЕТСЯ ТЕСТИРОВАНИЕ.
Возможно, код должен повторять copy_from.*)

PROCEDURE (rb: renderer_base_ptr) blend_from*(src: pfb.pixel_formats_ptr; rect_src: bas.rect; dx, dy: bas.int32; cover: bas.int8u);
VAR
  rsrc, rdst, rc: bas.rect;
  incy, x1src, x1dst, len: bas.int32;
  pw, ph: bas.int32;
  rw: bas.int32;

  (*rsx1: bas.int32;*)
  rl: bas.int32;
BEGIN
  pw := src.pixel_width();
  ph := src.pixel_height();

  rsrc.Construct(0, 0, pw, ph);

  IF rect_src.is_valid() THEN
    rsrc.x1 := rect_src.x1;
    rsrc.y1 := rect_src.y1;
    rsrc.x2 := rect_src.x2 + 1;
    rsrc.y2 := rect_src.y2 + 1;
  END;

  rdst.Construct(rsrc.x1 + dx, rsrc.y1 + dy, rsrc.x2 + dx, rsrc.y2 + dy);

  rb.clip_rect_area(rdst, rsrc, pw, ph, rc);

  IF rc.x2 > 0 THEN
    incy := 1;

    IF rdst.y1 > rsrc.y1 THEN
      rsrc.y1 := rsrc.y1 + (rc.y2 - 1);
      rdst.y1 := rdst.y1 + (rc.y2 - 1);
      incy := -1;
    END;

    WHILE rc.y2 > 0 DO
      rw := src.rbuf.row(rsrc.y1);
      (*rsx1 := rsrc.x1;*)
      rl   := src.pixel_width() - 1;

      IF rw < src.pixel_height() THEN
        x1src := rsrc.x1;
        x1dst := rdst.x1;
        len   := rc.x2;

        (* const code?
        IF rsx1 > x1src THEN
          INC(x1dst, rsx1 - x1src);
          DEC(len, rsx1 - x1src);
          x1src := rsx1;
        END; *)

        IF len > 0 THEN
          IF x1src + len - 1 > rl THEN
            DEC(len, x1src + len - rl - 1 );
          END;

          IF len > 0 THEN
            rb.pf.blend_from(src, x1dst, rdst.y1, x1src, rsrc.y1, len, cover);
          END;

        END;

      END;


      INC(rdst.y1, incy);
      INC(rsrc.y1, incy);

      DEC(rc.y2);
    END;
  END;
END blend_from;

PROCEDURE (rb: renderer_base_ptr) first_clip_box*();
BEGIN
END first_clip_box;

PROCEDURE (rb: renderer_base_ptr) next_clip_box*(): BOOLEAN;
BEGIN
  RETURN FALSE;
END next_clip_box;



END AggRendererBase.