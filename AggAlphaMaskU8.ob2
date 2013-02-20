MODULE AggAlphaMaskU8;

IMPORT
  bas := AggBasics,
  arb := AggRenderingBuffer;

TYPE
  func_mask_calculate* = PROCEDURE(rbuf: arb.rendering_buffer_ptr; x, y: bas.int32): bas.int8u;

  alpha_mask_ptr* = POINTER TO alpha_mask;
  alpha_mask* = RECORD
    step, offset: bas.int32;
    rbuf          : arb.rendering_buffer_ptr;
    mask_function*: func_mask_calculate;
  END;

  alpha_mask_u8_ptr* = POINTER TO alpha_mask_u8;
  alpha_mask_u8* = RECORD(alpha_mask)
  END;

  amask_no_clip_u8_ptr* = POINTER TO amask_no_clip_u8;
  amask_no_clip_u8 = RECORD(alpha_mask)
  END;

  (* GRAY *)

  alpha_mask_gray8_ptr* = POINTER TO alpha_mask_gray8;
  alpha_mask_gray8* = RECORD(alpha_mask_u8)
  END;

  (*RGB*)

  alpha_mask_rgb24r_ptr* = POINTER TO alpha_mask_rgb24r;
  alpha_mask_rgb24r* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_rgb24g_ptr* = POINTER TO alpha_mask_rgb24g;
  alpha_mask_rgb24g* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_rgb24b_ptr* = POINTER TO alpha_mask_rgb24b;
  alpha_mask_rgb24b* = RECORD(alpha_mask_u8)
  END;

  (*BGR*)

  alpha_mask_bgr24r_ptr* = POINTER TO alpha_mask_bgr24r;
  alpha_mask_bgr24r* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_bgr24g_ptr* = POINTER TO alpha_mask_bgr24g;
  alpha_mask_bgr24g* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_bgr24b_ptr* = POINTER TO alpha_mask_bgr24b;
  alpha_mask_bgr24b* = RECORD(alpha_mask_u8)
  END;

  (* RGBA *)

  alpha_mask_rgba32r_ptr* = POINTER TO alpha_mask_rgba32r;
  alpha_mask_rgba32r* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_rgba32g_ptr* = POINTER TO alpha_mask_rgba32g;
  alpha_mask_rgba32g* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_rgba32b_ptr* = POINTER TO alpha_mask_rgba32b;
  alpha_mask_rgba32b* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_rgba32a_ptr* = POINTER TO alpha_mask_rgba32a;
  alpha_mask_rgba32a* = RECORD(alpha_mask_u8)
  END;

  (*ARGB*)

  alpha_mask_argb32r_ptr* = POINTER TO alpha_mask_argb32r;
  alpha_mask_argb32r* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_argb32g_ptr* = POINTER TO alpha_mask_argb32g;
  alpha_mask_argb32g* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_argb32b_ptr* = POINTER TO alpha_mask_argb32b;
  alpha_mask_argb32b* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_argb32a_ptr* = POINTER TO alpha_mask_argb32a;
  alpha_mask_argb32a* = RECORD(alpha_mask_u8)
  END;

  (* BRGA *)

  alpha_mask_bgra32r_ptr* = POINTER TO alpha_mask_bgra32r;
  alpha_mask_bgra32r* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_bgra32g_ptr* = POINTER TO alpha_mask_bgra32g;
  alpha_mask_bgra32g* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_bgra32b_ptr* = POINTER TO alpha_mask_bgra32b;
  alpha_mask_bgra32b* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_bgra32a_ptr* = POINTER TO alpha_mask_bgra32a;
  alpha_mask_bgra32a* = RECORD(alpha_mask_u8)
  END;

  (* ABRG *)

  alpha_mask_abgr32r_ptr* = POINTER TO alpha_mask_abgr32r;
  alpha_mask_abgr32r* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_abgr32g_ptr* = POINTER TO alpha_mask_abgr32g;
  alpha_mask_abgr32g* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_abgr32b_ptr* = POINTER TO alpha_mask_abgr32b;
  alpha_mask_abgr32b* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_abgr32a_ptr* = POINTER TO alpha_mask_abgr32a;
  alpha_mask_abgr32a* = RECORD(alpha_mask_u8)
  END;

  (* GRAY *)

  alpha_mask_rgb24gray_ptr* = POINTER TO alpha_mask_rgb24gray;
  alpha_mask_rgb24gray* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_bgr24gray_ptr* = POINTER TO alpha_mask_bgr24gray;
  alpha_mask_bgr24gray* = RECORD(alpha_mask_u8)
  END;


  alpha_mask_rgba32gray_ptr* = POINTER TO alpha_mask_rgba32gray;
  alpha_mask_rgba32gray* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_argb32gray_ptr* = POINTER TO alpha_mask_argb32gray;
  alpha_mask_argb32gray* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_bgra32gray_ptr* = POINTER TO alpha_mask_bgra32gray;
  alpha_mask_bgra32gray* = RECORD(alpha_mask_u8)
  END;

  alpha_mask_abgr32gray_ptr* = POINTER TO alpha_mask_abgr32gray;
  alpha_mask_abgr32gray* = RECORD(alpha_mask_u8)
  END;

  (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*)
  (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*)
  (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*)

  (* GRAY *)

  amask_no_clip_gray8_ptr* = POINTER TO amask_no_clip_gray8;
  amask_no_clip_gray8* = RECORD(amask_no_clip_u8)
  END;

  (*RGB*)

  amask_no_clip_rgb24r_ptr* = POINTER TO amask_no_clip_rgb24r;
  amask_no_clip_rgb24r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_rgb24g_ptr* = POINTER TO amask_no_clip_rgb24g;
  amask_no_clip_rgb24g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_rgb24b_ptr* = POINTER TO amask_no_clip_rgb24b;
  amask_no_clip_rgb24b* = RECORD(amask_no_clip_u8)
  END;

  (*BGR*)

  amask_no_clip_bgr24r_ptr* = POINTER TO amask_no_clip_bgr24r;
  amask_no_clip_bgr24r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgr24g_ptr* = POINTER TO amask_no_clip_bgr24g;
  amask_no_clip_bgr24g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgr24b_ptr* = POINTER TO amask_no_clip_bgr24b;
  amask_no_clip_bgr24b* = RECORD(amask_no_clip_u8)
  END;

  (* RGBA *)

  amask_no_clip_rgba32r_ptr* = POINTER TO amask_no_clip_rgba32r;
  amask_no_clip_rgba32r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_rgba32g_ptr* = POINTER TO amask_no_clip_rgba32g;
  amask_no_clip_rgba32g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_rgba32b_ptr* = POINTER TO amask_no_clip_rgba32b;
  amask_no_clip_rgba32b* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_rgba32a_ptr* = POINTER TO amask_no_clip_rgba32a;
  amask_no_clip_rgba32a* = RECORD(amask_no_clip_u8)
  END;

  (*ARGB*)

  amask_no_clip_argb32r_ptr* = POINTER TO amask_no_clip_argb32r;
  amask_no_clip_argb32r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_argb32g_ptr* = POINTER TO amask_no_clip_argb32g;
  amask_no_clip_argb32g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_argb32b_ptr* = POINTER TO amask_no_clip_argb32b;
  amask_no_clip_argb32b* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_argb32a_ptr* = POINTER TO amask_no_clip_argb32a;
  amask_no_clip_argb32a* = RECORD(amask_no_clip_u8)
  END;

  (* BRGA *)

  amask_no_clip_bgra32r_ptr* = POINTER TO amask_no_clip_bgra32r;
  amask_no_clip_bgra32r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgra32g_ptr* = POINTER TO amask_no_clip_bgra32g;
  amask_no_clip_bgra32g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgra32b_ptr* = POINTER TO amask_no_clip_bgra32b;
  amask_no_clip_bgra32b* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgra32a_ptr* = POINTER TO amask_no_clip_bgra32a;
  amask_no_clip_bgra32a* = RECORD(amask_no_clip_u8)
  END;

  (* ABRG *)

  amask_no_clip_abgr32r_ptr* = POINTER TO amask_no_clip_abgr32r;
  amask_no_clip_abgr32r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_abgr32g_ptr* = POINTER TO amask_no_clip_abgr32g;
  amask_no_clip_abgr32g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_abgr32b_ptr* = POINTER TO amask_no_clip_abgr32b;
  amask_no_clip_abgr32b* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_abgr32a_ptr* = POINTER TO amask_no_clip_abgr32a;
  amask_no_clip_abgr32a* = RECORD(amask_no_clip_u8)
  END;

  (* GRAY *)

  amask_no_clip_rgb24gray_ptr* = POINTER TO amask_no_clip_rgb24gray;
  amask_no_clip_rgb24gray* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgr24gray_ptr* = POINTER TO amask_no_clip_bgr24gray;
  amask_no_clip_bgr24gray* = RECORD(amask_no_clip_u8)
  END;


  amask_no_clip_rgba32gray_ptr* = POINTER TO amask_no_clip_rgba32gray;
  amask_no_clip_rgba32gray* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_argb32gray_ptr* = POINTER TO amask_no_clip_argb32gray;
  amask_no_clip_argb32gray* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgra32gray_ptr* = POINTER TO amask_no_clip_bgra32gray;
  amask_no_clip_bgra32gray* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_abgr32gray_ptr* = POINTER TO amask_no_clip_abgr32gray;
  amask_no_clip_abgr32gray* = RECORD(amask_no_clip_u8)
  END;



CONST
  cover_shift = 8;
  cover_none  = 0;
  cover_full  = 255;

PROCEDURE one_component_mask_u8(rbuf: arb.rendering_buffer_ptr; x, y: bas.int32): bas.int8u;
BEGIN
  RETURN rbuf.buf[y, x];
END one_component_mask_u8;

PROCEDURE rgb_to_gray_mask_u8_012(rbuf: arb.rendering_buffer_ptr; x, y: bas.int32): bas.int8u;
BEGIN
  RETURN bas.agg_getbyte(
           ASH(
             LONG(rbuf.buf[y, x + 0]) *  77 +
             LONG(rbuf.buf[y, x + 1]) * 150 +
             LONG(rbuf.buf[y, x + 2]) *  29,
           -cover_shift)
         );
END rgb_to_gray_mask_u8_012;

PROCEDURE rgb_to_gray_mask_u8_210(rbuf: arb.rendering_buffer_ptr; x, y: bas.int32): bas.int8u;
BEGIN
  RETURN bas.agg_getbyte(
           ASH(
             LONG(rbuf.buf[y, x + 2]) *  77 +
             LONG(rbuf.buf[y, x + 1]) * 150 +
             LONG(rbuf.buf[y, x + 0]) *  29,
           -cover_shift)
         );
END rgb_to_gray_mask_u8_210;


PROCEDURE (am: alpha_mask_ptr) attach*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  am.rbuf := rbuf;
END attach;

PROCEDURE (am: alpha_mask_ptr) ConstructI*();
BEGIN
  am.rbuf   := NIL;
  am.step   := 0;
  am.offset := 0;
  am.mask_function := NIL;
END ConstructI;


PROCEDURE (am: alpha_mask_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  am.ConstructI;
  am.attach(rbuf);
END Construct;

PROCEDURE (am: alpha_mask_ptr) ConstructF*(f: func_mask_calculate; step: bas.int32; offset: bas.int32);
BEGIN
  am.rbuf   := NIL;
  am.step   := step;
  am.offset := offset;
  am.mask_function := f;
END ConstructF;

PROCEDURE (am: alpha_mask_ptr) ConstructRF*(r: arb.rendering_buffer_ptr; f: func_mask_calculate; step: bas.int32; offset: bas.int32);
BEGIN
  am.rbuf   := r;
  am.step   := step;
  am.offset := offset;
  am.mask_function := f;
END ConstructRF;

<*+WOFF301*>
PROCEDURE (am: alpha_mask_ptr) pixel*(x, y: bas.int32): bas.int8u;
BEGIN
  RETURN 0;
END pixel;

PROCEDURE (au: alpha_mask_ptr) combine_pixel*(x, y: bas.int32; val: bas.int8u): bas.int8u;
BEGIN
  RETURN 0;
END combine_pixel;

PROCEDURE (am: alpha_mask_ptr) fill_hspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
BEGIN

END fill_hspan;

PROCEDURE (au: alpha_mask_ptr) combine_hspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
BEGIN
END combine_hspan;

PROCEDURE (am: alpha_mask_ptr) fill_vspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
BEGIN
END fill_vspan;

PROCEDURE (au: alpha_mask_ptr) combine_vspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
BEGIN
END combine_vspan;
<*-WOFF301*>

PROCEDURE (au: alpha_mask_u8_ptr) pixel*(x, y: bas.int32): bas.int8u;
VAR
  ax: bas.int32;
BEGIN
  ax := x * au.step + au.offset;
  IF (au.rbuf # NIL) & (au.mask_function # NIL) &
     (ax >= 0) &
     (y  >= 0) &
     (ax < au.rbuf.byte_width) &
     (y < au.rbuf.byte_height) THEN
    RETURN au.mask_function(au.rbuf, ax, au.rbuf.row(y));
  ELSE
    RETURN 0;
  END;
END pixel;

PROCEDURE (au: alpha_mask_u8_ptr) combine_pixel*(x, y: bas.int32; val: bas.int8u): bas.int8u;
VAR
  ax: bas.int32;
BEGIN
  ax := x * au.step + au.offset;
  IF (au.rbuf # NIL) & (au.mask_function # NIL) &
     (ax >= 0) &
     (y  >= 0) &
     (ax < au.rbuf.byte_width) &
     (y < au.rbuf.byte_height) THEN
    RETURN bas.agg_getbyte(ASH(cover_full + LONG(val) * LONG(au.mask_function(au.rbuf, ax, au.rbuf.row(y))), -cover_shift));
  ELSE
    RETURN 0;
  END;
END combine_pixel;

PROCEDURE (au: alpha_mask_u8_ptr) fill_hspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
VAR
  xmax, ymax, count, rest: bas.int32;
  covers: bas.int8u_array_ptr;
  mask: bas.int8u;
  covers_idx: bas.int32;
  ax: bas.int32;
BEGIN
  xmax := au.rbuf.pixel_width - 1;
  ymax := au.rbuf.pixel_height - 1;

  count  := LEN(dest^);
  covers := dest;
  covers_idx := 0;

  IF (y < 0) OR (y > ymax) THEN
    bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
    RETURN;
  END;

  IF x < 0 THEN
    INC(count, x);

    IF count <= 0 THEN
      bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
      RETURN;
    END;

    bas.agg_fill(bas.agg_adr(covers^[0]), -x, 0);
    covers_idx := -x;
    x := 0;
  END;

  IF x + count > xmax THEN
    rest := x + count - xmax - 1;
    DEC(count, rest);

    IF count <= 0 THEN
      bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
      RETURN;
    END;
    bas.agg_fill(bas.agg_adr(covers^[count]), rest, 0);
  END;

  ax := x * au.step + au.offset;

  REPEAT
    mask := au.mask_function(au.rbuf, ax, au.rbuf.row(y));
    covers^[covers_idx] := mask;
    INC(covers_idx);
    INC(ax, au.step);
    DEC(count);
  UNTIL count = 0;
END fill_hspan;

PROCEDURE (au: alpha_mask_u8_ptr) combine_hspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
VAR
  xmax, ymax, count, rest: bas.int32;
  mask: bas.int8u;
  covers: bas.int8u_array_ptr;
  covers_idx: bas.int32;
  ax: bas.int32;
BEGIN
  xmax := au.rbuf.pixel_width - 1;
  ymax := au.rbuf.pixel_height - 1;

  count  := LEN(dest^);
  covers := dest;
  covers_idx := 0;

  IF (y < 0) OR (y > ymax) THEN
    bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
    RETURN;
  END;

  IF x < 0 THEN
    INC(count, x);

    IF count <= 0 THEN
      bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
      RETURN;
    END;

    bas.agg_fill(bas.agg_adr(covers^[0]), -x, 0);
    covers_idx := -x;
    x := 0;
  END;

  IF x + count > xmax THEN
    rest := x + count - xmax - 1;
    DEC(count, rest);

    IF count <= 0 THEN
      bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
      RETURN;
    END;

    bas.agg_fill(bas.agg_adr(covers^[count]), rest, 0);
  END;

  ax := x * au.step + au.offset;

  REPEAT
    mask := au.mask_function(au.rbuf, ax, au.rbuf.row(y));
    covers^[covers_idx] := bas.agg_getbyte(ASH(cover_full + LONG(covers^[covers_idx]) * LONG(mask), -cover_shift));
    INC(covers_idx);
    INC(ax, au.step);
    DEC(count);
  UNTIL count = 0;
END combine_hspan;

PROCEDURE (au: alpha_mask_u8_ptr) fill_vspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
VAR
  xmax, ymax, count, rest: bas.int32;
  covers: bas.int8u_array_ptr;
  mask: bas.int8u;
  covers_idx: bas.int32;
  ax: bas.int32;
BEGIN
  xmax := au.rbuf.pixel_width - 1;
  ymax := au.rbuf.pixel_height - 1;

  count  := LEN(dest^);
  covers := dest;
  covers_idx := 0;

  IF (x < 0) OR (x > xmax) THEN
    bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
    RETURN;
  END;

  IF y < 0 THEN
    INC(count, y);

    IF count <= 0 THEN
      bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
      RETURN;
    END;

    bas.agg_fill(bas.agg_adr(covers^[0]), -y, 0);
    covers_idx := -y;
    y := 0;
  END;

  IF y + count > ymax THEN
    rest := y + count - ymax - 1;
    DEC(count, rest);

    IF count <= 0 THEN
      bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
      RETURN;
    END;
    bas.agg_fill(bas.agg_adr(covers^[count]), rest, 0);
  END;

  ax := x * au.step + au.offset;

  REPEAT
    mask := au.mask_function(au.rbuf, ax, au.rbuf.row(y));
    covers^[covers_idx] := mask;
    INC(covers_idx);
    INC(y);
    DEC(count);
  UNTIL count = 0;
END fill_vspan;

PROCEDURE (au: alpha_mask_u8_ptr) combine_vspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
VAR
  xmax, ymax, count, rest: bas.int32;
  covers: bas.int8u_array_ptr;
  mask: bas.int8u;
  covers_idx: bas.int32;
  ax: bas.int32;
BEGIN
  xmax := au.rbuf.pixel_width - 1;
  ymax := au.rbuf.pixel_height - 1;

  count  := LEN(dest^);
  covers := dest;
  covers_idx := 0;

  IF (x < 0) OR (x > xmax) THEN
    bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
    RETURN;
  END;

  IF y < 0 THEN
    INC(count, y);

    IF count <= 0 THEN
      bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
      RETURN;
    END;

    bas.agg_fill(bas.agg_adr(covers^[0]), -y, 0);
    covers_idx := -y;
    y := 0;
  END;

  IF y + count > ymax THEN
    rest := y + count - ymax - 1;
    DEC(count, rest);

    IF count <= 0 THEN
      bas.agg_fill(bas.agg_adr(dest^[0]), LEN(dest^), 0);
      RETURN;
    END;
    bas.agg_fill(bas.agg_adr(covers^[count]), rest, 0);
  END;

  ax := x * au.step + au.offset;

  REPEAT
    mask := au.mask_function(au.rbuf, ax, au.rbuf.row(y));
    covers^[covers_idx] := bas.agg_getbyte(ASH(cover_full + LONG(covers^[covers_idx]) * LONG(mask), -cover_shift));
    INC(covers_idx);
    INC(y);
    DEC(count);
  UNTIL count = 0;
END combine_vspan;

(* NOCLIP *)

PROCEDURE (an: amask_no_clip_u8_ptr) pixel*(x, y: bas.int32): bas.int8u;
BEGIN
  RETURN an.mask_function(an.rbuf, x * an.step + an.offset, an.rbuf.row(y));
END pixel;

PROCEDURE (an: amask_no_clip_u8_ptr) combine_pixel*(x, y: bas.int32; val: bas.int8u): bas.int8u;
BEGIN
  RETURN bas.agg_getbyte(ASH(cover_full + LONG(val) * LONG(an.mask_function(an.rbuf, x * an.step + an.offset, an.rbuf.row(y))), -cover_shift));
END combine_pixel;

PROCEDURE (an: amask_no_clip_u8_ptr) fill_hspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
VAR
  mask: bas.int8u;
  ax: bas.int32;
  dest_idx: bas.int32;
  count: bas.int32;
BEGIN
  dest_idx := 0;
  ax := x * an.step + an.offset;
  count := LEN(dest^);

  REPEAT
    mask := an.mask_function(an.rbuf, ax, an.rbuf.row(y));
    dest^[dest_idx] := mask;
    INC(dest_idx);
    INC(ax, an.step);
    DEC(count);
  UNTIL count = 0;
END fill_hspan;

PROCEDURE (an: amask_no_clip_u8_ptr) combine_hspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
VAR
  mask: bas.int8u;
  ax: bas.int32;
  dest_idx: bas.int32;
  count: bas.int32;
BEGIN
  dest_idx := 0;
  ax := x * an.step + an.offset;
  count := LEN(dest^);

  REPEAT
    mask := an.mask_function(an.rbuf, ax, an.rbuf.row(y));
    dest^[dest_idx] := bas.agg_getbyte(ASH(cover_full + LONG(dest^[dest_idx]) * LONG(mask), -cover_shift));
    INC(dest_idx);
    INC(ax, an.step);
    DEC(count);
  UNTIL count = 0;
END combine_hspan;

PROCEDURE (an: amask_no_clip_u8_ptr) fill_vspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
VAR
  mask: bas.int8u;
  ax: bas.int32;
  dest_idx: bas.int32;
  count: bas.int32;
BEGIN
  dest_idx := 0;
  ax := x * an.step + an.offset;
  count := LEN(dest^);

  REPEAT
    mask := an.mask_function(an.rbuf, ax, an.rbuf.row(y));
    dest^[dest_idx] := mask;
    INC(dest_idx);
    INC(y);
    DEC(count);
  UNTIL count = 0;
END fill_vspan;

PROCEDURE (an: amask_no_clip_u8_ptr) combine_vspan*(x, y: bas.int32; dest: bas.int8u_array_ptr);
VAR
  mask: bas.int8u;
  ax: bas.int32;
  dest_idx: bas.int32;
  count: bas.int32;
BEGIN
  dest_idx := 0;
  ax := x * an.step + an.offset;
  count := LEN(dest^);

  REPEAT
    mask := an.mask_function(an.rbuf, ax, an.rbuf.row(y));
    dest^[dest_idx] := bas.agg_getbyte(ASH(cover_full + LONG(dest^[dest_idx]) * LONG(mask), -cover_shift));
    INC(dest_idx);
    INC(y);
    DEC(count);
  UNTIL count = 0;
END combine_vspan;

(* GRAY *)

PROCEDURE (au: alpha_mask_gray8_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 1, 0);
END Construct;

(*RGB*)

PROCEDURE (au: alpha_mask_rgb24r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 0);
END Construct;

PROCEDURE (au: alpha_mask_rgb24g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 1);
END Construct;

PROCEDURE (au: alpha_mask_rgb24b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 2);
END Construct;


(* BGR *)

PROCEDURE (au: alpha_mask_bgr24r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 2);
END Construct;

PROCEDURE (au: alpha_mask_bgr24g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 1);
END Construct;

PROCEDURE (au: alpha_mask_bgr24b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 0);
END Construct;


(* RGBA *)

PROCEDURE (au: alpha_mask_rgba32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

PROCEDURE (au: alpha_mask_rgba32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: alpha_mask_rgba32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: alpha_mask_rgba32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

(* ARGB *)

PROCEDURE (au: alpha_mask_argb32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: alpha_mask_argb32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: alpha_mask_argb32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

PROCEDURE (au: alpha_mask_argb32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

(* BGRA *)

PROCEDURE (au: alpha_mask_bgra32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: alpha_mask_bgra32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: alpha_mask_bgra32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

PROCEDURE (au: alpha_mask_bgra32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

(* ABGR *)

PROCEDURE (au: alpha_mask_abgr32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

PROCEDURE (au: alpha_mask_abgr32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: alpha_mask_abgr32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: alpha_mask_abgr32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

(* GRAY *)

PROCEDURE (au: alpha_mask_rgb24gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_012, 3, 0);
END Construct;

PROCEDURE (au: alpha_mask_bgr24gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_210, 3, 0);
END Construct;

PROCEDURE (au: alpha_mask_rgba32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_012, 4, 0);
END Construct;

PROCEDURE (au: alpha_mask_argb32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_012, 4, 1);
END Construct;

PROCEDURE (au: alpha_mask_bgra32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_210, 4, 0);
END Construct;

PROCEDURE (au: alpha_mask_abgr32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_210, 4, 1);
END Construct;

  (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*)
  (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*)
  (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*) (*NOCLIP*)


(* GRAY *)

PROCEDURE (au: amask_no_clip_gray8_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 1, 0);
END Construct;

(*RGB*)

PROCEDURE (au: amask_no_clip_rgb24r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 0);
END Construct;

PROCEDURE (au: amask_no_clip_rgb24g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 1);
END Construct;

PROCEDURE (au: amask_no_clip_rgb24b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 2);
END Construct;


(* BGR *)

PROCEDURE (au: amask_no_clip_bgr24r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 2);
END Construct;

PROCEDURE (au: amask_no_clip_bgr24g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 1);
END Construct;

PROCEDURE (au: amask_no_clip_bgr24b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 0);
END Construct;


(* RGBA *)

PROCEDURE (au: amask_no_clip_rgba32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

PROCEDURE (au: amask_no_clip_rgba32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: amask_no_clip_rgba32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: amask_no_clip_rgba32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

(* ARGB *)

PROCEDURE (au: amask_no_clip_argb32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: amask_no_clip_argb32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: amask_no_clip_argb32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

PROCEDURE (au: amask_no_clip_argb32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

(* BGRA *)

PROCEDURE (au: amask_no_clip_bgra32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: amask_no_clip_bgra32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: amask_no_clip_bgra32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

PROCEDURE (au: amask_no_clip_bgra32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

(* ABGR *)

PROCEDURE (au: amask_no_clip_abgr32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

PROCEDURE (au: amask_no_clip_abgr32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: amask_no_clip_abgr32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: amask_no_clip_abgr32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

(* GRAY *)

PROCEDURE (au: amask_no_clip_rgb24gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_012, 3, 0);
END Construct;

PROCEDURE (au: amask_no_clip_bgr24gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_210, 3, 0);
END Construct;

PROCEDURE (au: amask_no_clip_rgba32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_012, 4, 0);
END Construct;

PROCEDURE (au: amask_no_clip_argb32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_012, 4, 1);
END Construct;

PROCEDURE (au: amask_no_clip_bgra32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_210, 4, 0);
END Construct;

PROCEDURE (au: amask_no_clip_abgr32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_210, 4, 1);
END Construct;

END AggAlphaMaskU8.

PROCEDURE (an: amask_no_clip_u8_ptr)