MODULE AggPixfmtRgb24Pre;

IMPORT
  bas := AggBasics,
  AggPixfmt24,
  AggPixfmtBase,
  col := AggColor,
  rb  := AggRenderingBuffer;

TYPE
  pixel_format_rgb24_pre_ptr* = POINTER TO pixel_format_rgb24_pre;
  pixel_format_rgb24_pre* = RECORD(AggPixfmt24.pixel_format24)
  END;

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) ConstructI*();
BEGIN
  pf.ConstructI^();
  pf.byte_order := AggPixfmtBase.rgb_order;
END ConstructI;

(* blend_pix_rgb *)

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) blend_pix_rgb24_pre(x, y: bas.int32; cr, cg, cb, alpha, cover: bas.int8u);
VAR
  r,g,b: bas.int32;
BEGIN
  alpha := col.base_mask - alpha;
  cover := bas.agg_getbyte(ASH(cover + 1, col.base_shift - 8));

  r := pf.value(x, y, pf.byte_order.R);
  g := pf.value(x, y, pf.byte_order.G);
  b := pf.value(x, y, pf.byte_order.B);

  pf.setvalue(x, y, pf.byte_order.R, bas.agg_getbyte(ASH(r * alpha + cr * cover, -col.base_shift)));
  pf.setvalue(x, y, pf.byte_order.G, bas.agg_getbyte(ASH(g * alpha + cg * cover, -col.base_shift)));
  pf.setvalue(x, y, pf.byte_order.B, bas.agg_getbyte(ASH(b * alpha + cb * cover, -col.base_shift)));
END blend_pix_rgb24_pre;

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) blend_pix_rgb24_pre1(x, y: bas.int32; cr, cg, cb, alpha: bas.int8u);
VAR
  r,g,b: bas.int32;
  cri,cgi,cbi, a: bas.int32;
BEGIN
  alpha := col.base_mask - alpha;

  r := pf.value(x, y, pf.byte_order.R);
  g := pf.value(x, y, pf.byte_order.G);
  b := pf.value(x, y, pf.byte_order.B);

  cri := cr;
  cgi := cg;
  cbi := cb;

  a := alpha;

  pf.setvalue(x, y, pf.byte_order.R, bas.agg_getbyte(ASH(r * a, -col.base_shift) + cri));
  pf.setvalue(x, y, pf.byte_order.G, bas.agg_getbyte(ASH(g * a, -col.base_shift) + cgi));
  pf.setvalue(x, y, pf.byte_order.B, bas.agg_getbyte(ASH(b * a, -col.base_shift) + cbi));
END blend_pix_rgb24_pre1;


(* copy_or_blend_pix_rgb *)

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) copy_or_blend_pix_rgb_pre(x, y: bas.int32; c: col.aggclr; cover: bas.int8u);
VAR
  alpha: bas.int8u;
BEGIN
  IF c.a # 0 THEN
    alpha := bas.agg_getbyte(ASH((LONG(c.a) * (LONG(cover) + 1)), -col.base_shift));
    IF alpha = col.base_mask THEN
      pf.setvalue(x, y, pf.byte_order.R, c.r);
      pf.setvalue(x, y, pf.byte_order.G, c.g);
      pf.setvalue(x, y, pf.byte_order.B, c.b);
    ELSE
      pf.blend_pix_rgb24_pre1(x, y, c.r, c.g, c.b, alpha);
    END;
  END;
END copy_or_blend_pix_rgb_pre;

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) copy_or_blend_pix_rgb1(x, y: bas.int32; c: col.aggclr);
BEGIN
  IF c.a # 0 THEN
    IF c.a = col.base_mask THEN
      pf.setvalue(x, y, pf.byte_order.R, c.r);
      pf.setvalue(x, y, pf.byte_order.G, c.g);
      pf.setvalue(x, y, pf.byte_order.B, c.b);
    ELSE
      pf.blend_pix_rgb24_pre1(x, y, c.r, c.g, c.b, c.a);
    END;
  END;
END copy_or_blend_pix_rgb1;


(* blend_pixel *)

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) blend_pixel*(x, y: bas.int32; c: col.aggclr; cover: bas.int8u);
BEGIN
  pf.copy_or_blend_pix_rgb_pre(x, y, c, cover);
END blend_pixel;

(* blend_hline *)

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) blend_hline*(x, y, len: bas.int32; c: col.aggclr; cover: bas.int8u);
VAR
  alpha: bas.int8u;
  ax: bas.int32;
BEGIN
  ax := x;
  IF c.a # 0 THEN
    alpha := bas.agg_getbyte(ASH((LONG(c.a) * (LONG(cover) + 1)), -col.base_shift));
    IF alpha = col.base_mask THEN
      REPEAT
        pf.setvalue(ax, y, pf.byte_order.R, c.r);
        pf.setvalue(ax, y, pf.byte_order.G, c.g);
        pf.setvalue(ax, y, pf.byte_order.B, c.b);
        INC(ax);
        DEC(len);
      UNTIL len = 0;
    ELSE
      REPEAT
        pf.blend_pix_rgb24_pre1(ax, y, c.r, c.g, c.b, alpha);
        INC(ax);
        DEC(len);
      UNTIL len = 0;
    END;
  END;
END blend_hline;

(* blend_vline *)

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) blend_vline*(x, y, len: bas.int32; c: col.aggclr; cover: bas.int8u);
VAR
  alpha: bas.int8u;
  ay: bas.int32;
BEGIN
  ay := y;
  IF c.a # 0 THEN
    alpha := bas.agg_getbyte(ASH((LONG(c.a) * (LONG(cover) + 1 )), -col.base_shift));
    IF alpha = col.base_mask THEN
      REPEAT
        pf.setvalue(x, ay, pf.byte_order.R, c.r);
        pf.setvalue(x, ay, pf.byte_order.G, c.g);
        pf.setvalue(x, ay, pf.byte_order.B, c.b);
        INC(ay);
        DEC(len);
      UNTIL len = 0;
    ELSE
      REPEAT
        pf.blend_pix_rgb24_pre1(x, ay, c.r, c.g, c.b, alpha);
        INC(ay);
        DEC(len);
      UNTIL len = 0;
    END;
  END;
END blend_vline;

(* blend_solid_hspan *)
(* Здесь необходимо сделать важное замечание.
   Смешение цвета производится с помощью массива covers.
   Взятие содержимого массива covers начинается с нуля.
   т.е. точке с координатой x соответствует covers[0],
   x+1 -> covers[1],
   ...
   x+n -> covers[n].

   Для целей отсечения введен дополнительный параметр(ы)
   (cov|col)_offset, задающий смещение относительно начала массива
   covers|colors. Т.е. смешение по прежнему производится как описано
   выше, но добавляеется смещение.
    *)
PROCEDURE (pf: pixel_format_rgb24_pre_ptr) blend_solid_hspan*(x, y, len: bas.int32; c: col.aggclr; VAR covers: col.covers_array_ptr; cov_offset: bas.int32);
VAR
  alpha: bas.int8u;
  i,ax, alen: bas.int32;
BEGIN
  ASSERT(covers # NIL);
  IF c.a # 0 THEN
    alen := bas.agg_min32(len, LEN(covers^));
    i := 0;
    ax := x;
    REPEAT
      alpha := bas.agg_getbyte(

        ASH((LONG(c.a) * (LONG(covers^[i + cov_offset]) + 1)), -col.base_shift)

      );

      IF alpha = col.base_mask THEN
        pf.setvalue(ax, y, pf.byte_order.R, c.r);
        pf.setvalue(ax, y, pf.byte_order.G, c.g);
        pf.setvalue(ax, y, pf.byte_order.B, c.b);
      ELSE
        pf.blend_pix_rgb24_pre(ax, y, c.r, c.g, c.b, alpha, covers^[i + cov_offset]);
      END;

      INC(ax);
      INC(i);
    UNTIL i = alen;
  END;
END blend_solid_hspan;

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) blend_solid_vspan*(x, y, len: bas.int32; c: col.aggclr;
  VAR covers: col.covers_array_ptr; cov_offset: bas.int32);
VAR
  alpha: bas.int8u;
  i, ay, alen: bas.int32;
BEGIN
  ASSERT(covers # NIL);
  IF c.a # 0 THEN
    alen := bas.agg_min32(len, LEN(covers^));
    i := 0;
    ay := y;
    REPEAT
      alpha := bas.agg_getbyte(ASH((c.a * (covers^[i + cov_offset] + 1)), -col.base_shift));
      IF alpha = col.base_mask THEN
        pf.setvalue(x, ay, pf.byte_order.R, c.r);
        pf.setvalue(x, ay, pf.byte_order.G, c.g);
        pf.setvalue(x, ay, pf.byte_order.B, c.b);
      ELSE
        pf.blend_pix_rgb24_pre(x, ay, c.r, c.g, c.b, alpha, covers^[i + cov_offset]);
      END;

      INC(ay);
      INC(i);
    UNTIL i = alen;
  END;
END blend_solid_vspan;

(* blend_color_hspan *)

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) blend_color_hspan*(x, y, len: bas.int32;
  VAR colors: col.colors_array_ptr; col_offset: bas.int32;
  VAR covers: col.covers_array_ptr; cov_offset: bas.int32;
       cover: bas.int8u);

VAR
  ax, i, alen: bas.int32;
BEGIN
  IF covers # NIL THEN
    alen := bas.agg_min32(len,
      bas.agg_min32(LEN(covers^), LEN(colors^)));
    i  := 0;
    ax := x;
    REPEAT
      pf.copy_or_blend_pix_rgb_pre(ax, y, colors^[i + col_offset], covers^[i + cov_offset]);
      INC(i);
      INC(ax);
    UNTIL i = alen;
  ELSE
    alen := bas.agg_min32(len, LEN(colors^));
    IF cover = bas.cover_full THEN
      i := 0;
      ax := x;
      REPEAT
        pf.copy_or_blend_pix_rgb1(ax, y, colors^[i + col_offset]);
        INC(i);
        INC(ax);
      UNTIL i = alen;
    ELSE
      i := 0;
      ax := x;
      REPEAT
        pf.copy_or_blend_pix_rgb_pre(ax, y, colors^[i + col_offset], cover);
        INC(i);
        INC(ax);
      UNTIL i = alen;
    END;
  END;
END blend_color_hspan;

(* blend_color_vspan *)

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) blend_color_vspan*(x, y, len: bas.int32;
  VAR colors: col.colors_array_ptr; col_offset: bas.int32;
  VAR covers: col.covers_array_ptr; cov_offset: bas.int32;
       cover: bas.int8u);
VAR
  ay, i, alen: bas.int32;
BEGIN
  IF covers # NIL THEN
    alen := bas.agg_min32(len,
      bas.agg_min32(LEN(covers^), LEN(colors^)));
    i  := 0;
    ay := y;
    REPEAT
      pf.copy_or_blend_pix_rgb_pre(x, ay, colors^[i + col_offset], covers^[i + cov_offset]);
      INC(i);
      INC(ay);
    UNTIL i = alen;
  ELSE
    alen := bas.agg_min32(len, LEN(colors^));
    IF cover = bas.cover_full THEN
      i := 0;
      ay := y;
      REPEAT
        pf.copy_or_blend_pix_rgb1(x, ay, colors^[i + col_offset]);
        INC(i);
        INC(ay);
      UNTIL i = alen;
    ELSE
      i := 0;
      ay := y;
      REPEAT
        pf.copy_or_blend_pix_rgb_pre(x, ay, colors^[i + col_offset], cover);
        INC(i);
        INC(ay);
      UNTIL i = alen;
    END;
  END;
END blend_color_vspan;

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) copy_from*(from: rb.rendering_buffer_ptr; xdst, ydst, xsrc, ysrc, len: bas.int32);
BEGIN
  bas.agg_move(
    bas.agg_adr(pf.rbuf.buf^[pf.rbuf.row(ysrc), xsrc * pf.rbuf.bppabs()]),
    bas.agg_adr(from.buf^[pf.rbuf.row(ydst), xdst * pf.rbuf.bppabs()]), len * from.bppabs());
END copy_from;

PROCEDURE (pf: pixel_format_rgb24_pre_ptr) blend_from*(from: AggPixfmtBase.pixel_formats_ptr; xdst, ydst, xsrc, ysrc, len: bas.int32; cover: bas.int8u);
VAR
  color: col.aggclr;
  alpha: bas.int8u;
  xs, ys, xd, yd: bas.int32;
BEGIN
  ASSERT(from.rbuf.bppabs() = 4);
  ASSERT(pf.pixel_height() = from.pixel_height());
  ASSERT(pf.pixel_width() = from.pixel_width());
  xs := xsrc;
  ys := ysrc;
  xd := xdst;
  yd := ydst;

  IF cover = bas.cover_mask THEN
    REPEAT
      alpha := from.value(xs, ys, from.byte_order.A);

      IF alpha # 0 THEN
        IF alpha = col.base_mask THEN
          pf.setvalue(xd, yd, pf.byte_order.R, from.value(xs, ys, from.byte_order.R));
          pf.setvalue(xd, yd, pf.byte_order.G, from.value(xs, ys, from.byte_order.G));
          pf.setvalue(xd, yd, pf.byte_order.B, from.value(xs, ys, from.byte_order.B));
        ELSE
          pf.blend_pix_rgb24_pre1(xd, yd,
              from.value(xs, ys, from.byte_order.R),
              from.value(xs, ys, from.byte_order.G),
              from.value(xs, ys, from.byte_order.B),
            alpha);
        END;
      END;

      INC(xs); IF xs = from.pixel_width() THEN INC(ys); xs := 0; END;
      INC(xd); IF xd = pf.pixel_width()   THEN INC(yd); xd := 0; END;


      DEC(len);
    UNTIL len = 0;
  ELSE
    REPEAT
      color.Constructrgba(
        from.value(xs, ys, from.byte_order.R),
        from.value(xs, ys, from.byte_order.G),
        from.value(xs, ys, from.byte_order.B),
        from.value(xs, ys, from.byte_order.A)
      );

      pf.copy_or_blend_pix_rgb_pre(xd, yd, color, cover);

      INC(xs); IF xs = from.pixel_width() THEN INC(ys); xs := 0; END;
      INC(xd); IF xd = pf.pixel_width()   THEN INC(yd); xd := 0; END;

      DEC(len);
    UNTIL len = 0;
  END
END blend_from;


END AggPixfmtRgb24Pre.
