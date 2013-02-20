MODULE AggPixfmtGray;

IMPORT
  bas := AggBasics,
  pfb := AggPixfmtGrayBase,
  col := AggColor;

TYPE
  pixel_format_gray8_ptr* = POINTER TO pixel_format_gray8;
  pixel_format_gray8    * = RECORD (pfb.pixel_format_gray_base)
  END;

  pixel_format_gray24r_ptr* = POINTER TO pixel_format_gray24r;
  pixel_format_gray24r    * = RECORD (pixel_format_gray8)
  END;
  pixel_format_gray24g_ptr* = POINTER TO pixel_format_gray24g;
  pixel_format_gray24g    * = RECORD (pixel_format_gray8)
  END;
  pixel_format_gray24b_ptr* = POINTER TO pixel_format_gray24b;
  pixel_format_gray24b    * = RECORD (pixel_format_gray8)
  END;

PROCEDURE (pfg: pixel_format_gray8_ptr) ConstructI*();
BEGIN
  pfg.ConstructI^();
  pfg.step   := 1;
  pfg.offset := 0;
END ConstructI;

PROCEDURE (pfg: pixel_format_gray24r_ptr) ConstructI*();
BEGIN
  pfg.ConstructI^();
  pfg.step   := 3;
  pfg.offset := 2;
END ConstructI;

PROCEDURE (pfg: pixel_format_gray24g_ptr) ConstructI*();
BEGIN
  pfg.ConstructI^();
  pfg.step   := 3;
  pfg.offset := 1;
END ConstructI;

PROCEDURE (pfg: pixel_format_gray24b_ptr) ConstructI*();
BEGIN
  pfg.ConstructI^();
  pfg.step   := 3;
  pfg.offset := 0;
END ConstructI;


PROCEDURE (pfg: pixel_format_gray8_ptr) blend_pix_gray(x, y: bas.int32; cv, alpha: bas.int8u);
VAR
  v: bas.int32;
BEGIN
  v := pfg.rbuf.buf^[y, x];
  pfg.rbuf.buf^[y, x] := bas.agg_getbyte(ASH(((cv - v) * alpha + ASH(v, col.base_shift)), -col.base_shift));
END blend_pix_gray;


PROCEDURE (pfg: pixel_format_gray8_ptr) copy_or_blend_pix_gray(x, y: bas.int32; c: col.aggclr; cover: bas.int8u);
VAR
  alpha: bas.int8u;
BEGIN
  IF c.a # 0 THEN
    alpha := bas.agg_getbyte(ASH((LONG(c.a) * (LONG(cover) + 1)), -col.base_shift));
    IF alpha = col.base_mask THEN
      pfg.rbuf.buf^[y, x] := c.v;
    ELSE
      pfg.blend_pix_gray(x, y, c.v, alpha);
    END;
  END;
END copy_or_blend_pix_gray;

PROCEDURE (pfg: pixel_format_gray8_ptr) copy_or_blend_pix_gray1(x, y: bas.int32; c: col.aggclr);
BEGIN
  IF c.a # 0 THEN
    IF c.a = col.base_mask THEN
      pfg.rbuf.buf^[y, x] := c.v;
    ELSE
      pfg.blend_pix_gray(x, y, c.v, c.a);
    END;
  END;
END copy_or_blend_pix_gray1;


PROCEDURE (pfg: pixel_format_gray8_ptr) blend_pixel*(x, y: bas.int32; c: col.aggclr; cover: bas.int8u);
BEGIN
  pfg.copy_or_blend_pix_gray(x * pfg.step + pfg.offset, pfg.rbuf.row(y), c, cover);
END blend_pixel;

PROCEDURE (pfg: pixel_format_gray8_ptr) blend_hline*(x, y, len: bas.int32; c: col.aggclr; cover: bas.int8u);
VAR
  alpha: bas.int8u;
  ax: bas.int32;
BEGIN
  IF c.a # 0 THEN
    ax := x * pfg.step + pfg.offset;
    alpha := bas.agg_getbyte(ASH((LONG(c.a) * (LONG(cover) + 1)), -col.base_shift));
    IF alpha = col.base_mask THEN
      REPEAT
	pfg.rbuf.buf^[pfg.rbuf.row(y), ax] := c.v;
        INC(ax, pfg.step);
        DEC(len);
      UNTIL len = 0;
    ELSE
      REPEAT
        pfg.blend_pix_gray(ax, pfg.rbuf.row(y), c.v, alpha);
        INC(ax, pfg.step);
        DEC(len);
      UNTIL len = 0;
    END;
  END;
END blend_hline;

PROCEDURE (pfg: pixel_format_gray8_ptr) blend_vline*(x, y, len: bas.int32; c: col.aggclr; cover: bas.int8u);
VAR
  alpha: bas.int8u;
  ay: bas.int32;
BEGIN
  ay := y;
  IF c.a # 0 THEN
    alpha := bas.agg_getbyte(ASH((LONG(c.a) * (LONG(cover) + 1 )), -col.base_shift));
    IF alpha = col.base_mask THEN
      REPEAT
	pfg.rbuf.buf^[pfg.rbuf.row(ay), x] := c.v;
        INC(ay);
        DEC(len);
      UNTIL len = 0;
    ELSE
      REPEAT
        pfg.blend_pix_gray(x, ay, c.v, alpha);
        INC(ay);
        DEC(len);
      UNTIL len = 0;
    END;
  END;
END blend_vline;


PROCEDURE (pfg: pixel_format_gray8_ptr) blend_solid_hspan*(x, y, len: bas.int32; c: col.aggclr; VAR covers: col.covers_array_ptr; cov_offset: bas.int32);
VAR
  alpha: bas.int8u;
  i,ax, alen: bas.int32;
BEGIN
  ASSERT(covers # NIL);
  IF c.a # 0 THEN
    alen := bas.agg_min32(len, LEN(covers^));
    i := 0;
    ax := x * pfg.step + pfg.offset;
    REPEAT
      alpha := bas.agg_getbyte(

        ASH((LONG(c.a) * (LONG(covers^[i + cov_offset]) + 1)), -col.base_shift)

      );

      IF alpha = col.base_mask THEN
	pfg.rbuf.buf^[pfg.rbuf.row(y), ax] := c.v;
      ELSE
        pfg.blend_pix_gray(ax, pfg.rbuf.row(y), c.v, alpha);
      END;

      INC(ax, pfg.step);
      INC(i);
    UNTIL i = alen;
  END;
END blend_solid_hspan;


PROCEDURE (pfg: pixel_format_gray8_ptr) blend_solid_vspan*(x, y, len: bas.int32; c: col.aggclr;
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
      alpha := bas.agg_getbyte(ASH((LONG(c.a) * (LONG(covers^[i + cov_offset]) + 1)), -col.base_shift));
      IF alpha = col.base_mask THEN
	pfg.rbuf.buf^[pfg.rbuf.row(ay), x] := c.v;
      ELSE
        pfg.blend_pix_gray(x, ay, c.v, alpha);
      END;

      INC(ay);
      INC(i);
    UNTIL i = alen;
  END;
END blend_solid_vspan;


PROCEDURE (pfg: pixel_format_gray8_ptr) blend_color_hspan*(x, y, len: bas.int32;
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
    ax := x * pfg.step + pfg.offset;
    REPEAT
      pfg.copy_or_blend_pix_gray(ax, y, colors^[i + col_offset], covers^[i + cov_offset]);
      INC(i);
      INC(ax, pfg.step);
    UNTIL i = alen;
  ELSE
    alen := bas.agg_min32(len, LEN(colors^));
    IF cover = bas.cover_full THEN
      i := 0;
      ax := x;
      REPEAT
        pfg.copy_or_blend_pix_gray1(ax, y, colors^[i + col_offset]);
        INC(i);
        INC(ax, pfg.step);
      UNTIL i = alen;
    ELSE
      i := 0;
      ax := x;
      REPEAT
        pfg.copy_or_blend_pix_gray(ax, y, colors^[i + col_offset], cover);
        INC(i);
        INC(ax, pfg.step);
      UNTIL i = alen;
    END;
  END;
END blend_color_hspan;

(* blend_color_vspan *)

PROCEDURE (pfg: pixel_format_gray8_ptr) blend_color_vspan*(x, y, len: bas.int32;
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
      pfg.copy_or_blend_pix_gray(x, ay, colors^[i + col_offset], covers^[i + cov_offset]);
      INC(i);
      INC(ay);
    UNTIL i = alen;
  ELSE
    alen := bas.agg_min32(len, LEN(colors^));
    IF cover = bas.cover_full THEN
      i := 0;
      ay := y;
      REPEAT
        pfg.copy_or_blend_pix_gray1(x, ay, colors^[i + col_offset]);
        INC(i);
        INC(ay);
      UNTIL i = alen;
    ELSE
      i := 0;
      ay := y;
      REPEAT
        pfg.copy_or_blend_pix_gray(x, ay, colors^[i + col_offset], cover);
        INC(i);
        INC(ay);
      UNTIL i = alen;
    END;
  END;
END blend_color_vspan;


END AggPixfmtGray.

PROCEDURE (pfg: pixel_format_gray8_ptr)
