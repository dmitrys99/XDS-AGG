MODULE AggPixfmtGrayPre;

IMPORT
  bas := AggBasics,
  pfb := AggPixfmtGrayBase,
  col := AggColor;

TYPE
  pixel_format_gray8_pre_ptr* = POINTER TO pixel_format_gray8_pre;
  pixel_format_gray8_pre    * = RECORD (pfb.pixel_format_gray_base)
  END;

  pixel_format_gray24r_pre_ptr* = POINTER TO pixel_format_gray24r_pre;
  pixel_format_gray24r_pre    * = RECORD (pixel_format_gray8_pre)
  END;
  pixel_format_gray24g_pre_ptr* = POINTER TO pixel_format_gray24g_pre;
  pixel_format_gray24g_pre    * = RECORD (pixel_format_gray8_pre)
  END;
  pixel_format_gray24b_pre_ptr* = POINTER TO pixel_format_gray24b_pre;
  pixel_format_gray24b_pre    * = RECORD (pixel_format_gray8_pre)
  END;

PROCEDURE (pfg: pixel_format_gray8_pre_ptr) ConstructI*();
BEGIN
  pfg.ConstructI^();
  pfg.step   := 1;
  pfg.offset := 0;
END ConstructI;

PROCEDURE (pfg: pixel_format_gray24r_pre_ptr) ConstructI*();
BEGIN
  pfg.ConstructI^();
  pfg.step   := 3;
  pfg.offset := 2;
END ConstructI;

PROCEDURE (pfg: pixel_format_gray24g_pre_ptr) ConstructI*();
BEGIN
  pfg.ConstructI^();
  pfg.step   := 3;
  pfg.offset := 1;
END ConstructI;

PROCEDURE (pfg: pixel_format_gray24b_pre_ptr) ConstructI*();
BEGIN
  pfg.ConstructI^();
  pfg.step   := 3;
  pfg.offset := 0;
END ConstructI;


PROCEDURE (pfg: pixel_format_gray8_pre_ptr) blend_pix_gray_pre(x, y: bas.int32; cv, alpha, cover: bas.int8u);
VAR
  v: bas.int8u;
BEGIN
  alpha := col.base_mask - alpha;
  cover := bas.agg_getbyte(ASH(cover + 1, col.base_shift - 8));

  v := pfg.rbuf.buf^[y, x];

  pfg.rbuf.buf^[y, x] := bas.agg_getbyte(ASH((v * alpha + cv * cover), -col.base_shift));
END blend_pix_gray_pre;

PROCEDURE (pfg: pixel_format_gray8_pre_ptr) blend_pix_gray_pre1(x, y: bas.int32; cv, alpha: bas.int8u);
VAR
  v: bas.int8u;
BEGIN
  v := pfg.rbuf.buf^[y, x];

  pfg.rbuf.buf^[y, x] := bas.agg_getbyte((ASH((v * (col.base_mask - alpha)), -col.base_shift) + cv));
END blend_pix_gray_pre1;


PROCEDURE (pfg: pixel_format_gray8_pre_ptr) copy_or_blend_pix_gray_pre(x, y: bas.int32; c: col.aggclr; cover: bas.int8u);
VAR
  alpha: bas.int8u;
BEGIN
  IF c.a # 0 THEN
    alpha := bas.agg_getbyte(ASH((LONG(c.a) * (LONG(cover) + 1)), -col.base_shift));
    IF alpha = col.base_mask THEN
      pfg.rbuf.buf^[y, x] := c.v;
    ELSE
      pfg.blend_pix_gray_pre(x, y, c.v, alpha, cover);
    END;
  END;
END copy_or_blend_pix_gray_pre;

PROCEDURE (pfg: pixel_format_gray8_pre_ptr) copy_or_blend_pix_gray_pre1(x, y: bas.int32; c: col.aggclr);
BEGIN
  IF c.a # 0 THEN
    IF c.a = col.base_mask THEN
      pfg.rbuf.buf^[pfg.rbuf.row(y), x] := c.v;
    ELSE
      pfg.blend_pix_gray_pre1(x, y, c.v, c.a);
    END;
  END;
END copy_or_blend_pix_gray_pre1;


PROCEDURE (pfg: pixel_format_gray8_pre_ptr) blend_pixel*(x, y: bas.int32; c: col.aggclr; cover: bas.int8u);
BEGIN
  pfg.copy_or_blend_pix_gray_pre(x * pfg.step + pfg.offset, pfg.rbuf.row(y), c, cover);
END blend_pixel;

PROCEDURE (pfg: pixel_format_gray8_pre_ptr) pixel*(x, y: bas.int32; VAR c: col.aggclr);
VAR
  v: bas.int8u;
BEGIN
  v := pfg.rbuf.buf^[pfg.rbuf.row(y), x * pfg.step + pfg.offset];
  c.ConstructV(v, col.base_mask);
END pixel;

PROCEDURE (pfg: pixel_format_gray8_pre_ptr) blend_hline*(x, y, len: bas.int32; c: col.aggclr; cover: bas.int8u);
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
        pfg.blend_pix_gray_pre(ax, y, c.v, alpha, cover);
        INC(ax, pfg.step);
        DEC(len);
      UNTIL len = 0;
    END;
  END;
END blend_hline;

PROCEDURE (pfg: pixel_format_gray8_pre_ptr) blend_vline*(x, y, len: bas.int32; c: col.aggclr; cover: bas.int8u);
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
        pfg.blend_pix_gray_pre(x, ay, c.v, alpha, cover);
        INC(ay);
        DEC(len);
      UNTIL len = 0;
    END;
  END;
END blend_vline;


PROCEDURE (pfg: pixel_format_gray8_pre_ptr) blend_solid_hspan*(x, y, len: bas.int32; c: col.aggclr; VAR covers: col.covers_array_ptr; cov_offset: bas.int32);
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
        pfg.blend_pix_gray_pre(ax, y, c.v, alpha, covers^[i + cov_offset]);
      END;

      INC(ax, pfg.step);
      INC(i);
    UNTIL i = alen;
  END;
END blend_solid_hspan;


PROCEDURE (pfg: pixel_format_gray8_pre_ptr) blend_solid_vspan*(x, y, len: bas.int32; c: col.aggclr;
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
        pfg.blend_pix_gray_pre(x, ay, c.v, alpha, covers^[i + cov_offset]);
      END;

      INC(ay);
      INC(i);
    UNTIL i = alen;
  END;
END blend_solid_vspan;


PROCEDURE (pfg: pixel_format_gray8_pre_ptr) blend_color_hspan*(x, y, len: bas.int32;
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
      pfg.copy_or_blend_pix_gray_pre(ax, y, colors^[i + col_offset], covers^[i + cov_offset]);
      INC(i);
      INC(ax, pfg.step);
    UNTIL i = alen;
  ELSE
    alen := bas.agg_min32(len, LEN(colors^));
    IF cover = bas.cover_full THEN
      i := 0;
      ax := x;
      REPEAT
        pfg.copy_or_blend_pix_gray_pre1(ax, y, colors^[i + col_offset]);
        INC(i);
        INC(ax, pfg.step);
      UNTIL i = alen;
    ELSE
      i := 0;
      ax := x;
      REPEAT
        pfg.copy_or_blend_pix_gray_pre(ax, y, colors^[i + col_offset], cover);
        INC(i);
        INC(ax, pfg.step);
      UNTIL i = alen;
    END;
  END;
END blend_color_hspan;

(* blend_color_vspan *)

PROCEDURE (pfg: pixel_format_gray8_pre_ptr) blend_color_vspan*(x, y, len: bas.int32;
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
      pfg.copy_or_blend_pix_gray_pre(x, ay, colors^[i + col_offset], covers^[i + cov_offset]);
      INC(i);
      INC(ay);
    UNTIL i = alen;
  ELSE
    alen := bas.agg_min32(len, LEN(colors^));
    IF cover = bas.cover_full THEN
      i := 0;
      ay := y;
      REPEAT
        pfg.copy_or_blend_pix_gray_pre1(x, ay, colors^[i + col_offset]);
        INC(i);
        INC(ay);
      UNTIL i = alen;
    ELSE
      i := 0;
      ay := y;
      REPEAT
        pfg.copy_or_blend_pix_gray_pre(x, ay, colors^[i + col_offset], cover);
        INC(i);
        INC(ay);
      UNTIL i = alen;
    END;
  END;
END blend_color_vspan;


END AggPixfmtGrayPre.

PROCEDURE (pfg: pixel_format_gray8_pre_ptr)
