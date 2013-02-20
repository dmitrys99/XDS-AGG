MODULE AggPixfmtRgb24Gamma;

IMPORT
  bas := AggBasics, AggPixfmt24, AggPixfmtBase, col := AggColor, rb := AggRenderingBuffer;

TYPE
  pixel_format_rgb24_gamma_ptr* = POINTER TO pixel_format_rgb24_gamma;
  pixel_format_rgb24_gamma* = RECORD(AggPixfmt24.pixel_format24)
  END;

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) ConstructI*();
BEGIN
  pf.ConstructI^();
  pf.byte_order := AggPixfmtBase.rgb_order;
END ConstructI;

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) ConstructGamma*(rbuf: rb.rendering_buffer_ptr; gamma: AggPixfmtBase.Gamma_ptr);
BEGIN
  pf.Construct(rbuf);
  pf.gamma := gamma;
END ConstructGamma;

(* blend_pix_rgb *)

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) blend_pix_rgb24(x, y: bas.int32; cr, cg, cb, alpha: bas.int8u);
VAR
  r,g,b: bas.int32;
  a: bas.int32;
BEGIN
  ASSERT(pf.gamma # NIL);
  (* ѕоскольку в трех последних операторах происходит €вное целочисленное переполнение (уход в минус),
     то данные предварительно расшир€ютс€. *)
  r := pf.gamma.dir(pf.value(x, y, pf.byte_order.R));
  g := pf.gamma.dir(pf.value(x, y, pf.byte_order.G));
  b := pf.gamma.dir(pf.value(x, y, pf.byte_order.B));

  a := alpha;

  pf.setvalue(x, y, pf.byte_order.R, SHORT(SHORT(pf.gamma.inv(bas.agg_getbyte(ASH((SHORT(pf.gamma.dir(cr)) - r) * a, -col.base_shift) + r)))));
  pf.setvalue(x, y, pf.byte_order.G, SHORT(SHORT(pf.gamma.inv(bas.agg_getbyte(ASH((SHORT(pf.gamma.dir(cg)) - g) * a, -col.base_shift) + g)))));
  pf.setvalue(x, y, pf.byte_order.B, SHORT(SHORT(pf.gamma.inv(bas.agg_getbyte(ASH((SHORT(pf.gamma.dir(cb)) - b) * a, -col.base_shift) + b)))));
END blend_pix_rgb24;

(* copy_or_blend_pix_rgb *)

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) copy_or_blend_pix_rgb(x, y: bas.int32; c: col.aggclr; cover: bas.int8u);
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
      pf.blend_pix_rgb24(x, y, c.r, c.g, c.b, alpha);
    END;
  END;
END copy_or_blend_pix_rgb;

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) copy_or_blend_pix_rgb1(x, y: bas.int32; c: col.aggclr);
BEGIN
  IF c.a # 0 THEN
    IF c.a = col.base_mask THEN
      pf.setvalue(x, y, pf.byte_order.R, c.r);
      pf.setvalue(x, y, pf.byte_order.G, c.g);
      pf.setvalue(x, y, pf.byte_order.B, c.b);
    ELSE
      pf.blend_pix_rgb24(x, y, c.r, c.g, c.b, c.a);
    END;
  END;
END copy_or_blend_pix_rgb1;


(* blend_pixel *)

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) blend_pixel*(x, y: bas.int32; c: col.aggclr; cover: bas.int8u);
BEGIN
  pf.copy_or_blend_pix_rgb(x, y, c, cover);
END blend_pixel;

(* blend_hline *)

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) blend_hline*(x, y, len: bas.int32; c: col.aggclr; cover: bas.int8u);
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
        pf.blend_pix_rgb24(ax, y, c.r, c.g, c.b, alpha);
        INC(ax);
        DEC(len);
      UNTIL len = 0;
    END;
  END;
END blend_hline;

(* blend_vline *)

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) blend_vline*(x, y, len: bas.int32; c: col.aggclr; cover: bas.int8u);
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
        pf.blend_pix_rgb24(x, ay, c.r, c.g, c.b, alpha);
        INC(ay);
        DEC(len);
      UNTIL len = 0;
    END;
  END;
END blend_vline;

(* blend_solid_hspan *)
(* «десь необходимо сделать важное замечание.
   —мешение цвета производитс€ с помощью массива covers.
   ¬з€тие содержимого массива covers начинаетс€ с нул€.
   т.е. точке с координатой x соответствует covers[0],
   x+1 -> covers[1],
   ...
   x+n -> covers[n].

   ƒл€ целей отсечени€ введен дополнительный параметр(ы)
   (cov|col)_offset, задающий смещение относительно начала массива
   covers|colors. “.е. смешение по прежнему производитс€ как описано
   выше, но добавл€еетс€ смещение.
    *)
PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) blend_solid_hspan*(x, y, len: bas.int32; c: col.aggclr; VAR covers: col.covers_array_ptr; cov_offset: bas.int32);
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
        pf.blend_pix_rgb24(ax, y, c.r, c.g, c.b, alpha);
      END;

      INC(ax);
      INC(i);
    UNTIL i = alen;
  END;
END blend_solid_hspan;

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) blend_solid_vspan*(x, y, len: bas.int32; c: col.aggclr;
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
        pf.blend_pix_rgb24(x, ay, c.r, c.g, c.b, alpha);
      END;

      INC(ay);
      INC(i);
    UNTIL i = alen;
  END;
END blend_solid_vspan;

(* blend_color_hspan *)

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) blend_color_hspan*(x, y, len: bas.int32;
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
      pf.copy_or_blend_pix_rgb(ax, y, colors^[i + col_offset], covers^[i + cov_offset]);
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
        pf.copy_or_blend_pix_rgb(ax, y, colors^[i + col_offset], cover);
        INC(i);
        INC(ax);
      UNTIL i = alen;
    END;
  END;
END blend_color_hspan;

(* blend_color_vspan *)

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) blend_color_vspan*(x, y, len: bas.int32;
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
      pf.copy_or_blend_pix_rgb(x, ay, colors^[i + col_offset], covers^[i + cov_offset]);
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
        pf.copy_or_blend_pix_rgb(x, ay, colors^[i + col_offset], cover);
        INC(i);
        INC(ay);
      UNTIL i = alen;
    END;
  END;
END blend_color_vspan;

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) copy_from*(from: rb.rendering_buffer_ptr; xdst, ydst, xsrc, ysrc, len: bas.int32);
BEGIN
  bas.agg_move(
    bas.agg_adr(pf.rbuf.buf^[pf.rbuf.row(ysrc), xsrc * pf.rbuf.bppabs()]),
    bas.agg_adr(from.buf^[pf.rbuf.row(ydst), xdst * pf.rbuf.bppabs()]), len * from.bppabs());
END copy_from;

PROCEDURE (pf: pixel_format_rgb24_gamma_ptr) blend_from*(from: AggPixfmtBase.pixel_formats_ptr; xdst, ydst, xsrc, ysrc, len: bas.int32; cover: bas.int8u);
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
          pf.blend_pix_rgb24(xd, yd,
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

      pf.copy_or_blend_pix_rgb(xd, yd, color, cover);

      INC(xs); IF xs = from.pixel_width() THEN INC(ys); xs := 0; END;
      INC(xd); IF xd = pf.pixel_width()   THEN INC(yd); xd := 0; END;

      DEC(len);
    UNTIL len = 0;
  END
END blend_from;


END AggPixfmtRgb24Gamma.
