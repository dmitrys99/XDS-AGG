MODULE AggPixfmtBase;

IMPORT
  rb  := AggRenderingBuffer,
  bas := AggBasics,
  col := AggColor;

CONST
  comp_op_clear       * =  0;
  comp_op_src         * =  1;
  comp_op_dst         * =  2;
  comp_op_src_over    * =  3;
  comp_op_dst_over    * =  4;
  comp_op_src_in      * =  5;
  comp_op_dst_in      * =  6;
  comp_op_src_out     * =  7;
  comp_op_dst_out     * =  8;
  comp_op_src_atop    * =  9;
  comp_op_dst_atop    * = 10;
  comp_op_xor         * = 11;
  comp_op_plus        * = 12;
  comp_op_minus       * = 13;
  comp_op_multiply    * = 14;
  comp_op_screen      * = 15;
  comp_op_overlay     * = 16;
  comp_op_darken      * = 17;
  comp_op_lighten     * = 18;
  comp_op_color_dodge * = 19;
  comp_op_color_burn  * = 20;
  comp_op_hard_light  * = 21;
  comp_op_soft_light  * = 22;
  comp_op_difference  * = 23;
  comp_op_exclusion   * = 24;
  comp_op_contrast    * = 25;

TYPE
  (* Тип определяет порядок следования байт цвета/альфа-канала при обработке.
     Отдельные компоненты являются смещениями от начала пикселя в массиве байт. *)
  order_type* = RECORD
    R*,G*,B*,A*: bas.int8u;
  END;

  Gamma_ptr* = POINTER TO Gamma;
  Gamma* = RECORD

  END;

  pixel_formats_ptr* = POINTER TO pixel_formats;
  pixel_formats  * = RECORD
    rbuf         - : rb.rendering_buffer_ptr; (* Пиксельный буфер *)
    byte_order   * : order_type;          (* Порядок следования байт цвета в строке. *)
    comp_op      - : bas.int32;
    gamma        * : Gamma_ptr;
  END;

VAR
 (* Разные порядки следования байт в пикселе. *)
 rgb_order  - : order_type;
 bgr_order  - : order_type;
 rgba_order - : order_type;
 bgra_order - : order_type;
 argb_order - : order_type;
 abgr_order - : order_type;
 none_order - : order_type;

PROCEDURE (g: Gamma_ptr) dir*(v : bas.int32): bas.int32u;
BEGIN
  RETURN v;
END dir;

PROCEDURE (g: Gamma_ptr) inv*(v : bas.int32): bas.int32u;
BEGIN
  RETURN v;
END inv;

(* ConstructI *)

PROCEDURE (pf: pixel_formats_ptr) ConstructI*();
BEGIN
  NEW(pf.rbuf);
  pf.rbuf.Construct();
  pf.byte_order   := none_order;
(*  pf.pixel_width  := 0;
  pf.pixel_height := 0;*)
  pf.comp_op      := comp_op_src_over;
  pf.gamma        := NIL;
END ConstructI;

(* Construct *)
<*WOFF301+*>
PROCEDURE (pf: pixel_formats_ptr) value*(x, y, offset: bas.int32): bas.int8u;
BEGIN
  RETURN 0;
END value;
<*WOFF301-*>

PROCEDURE (pf: pixel_formats_ptr) pixel_width*(): bas.int32;
BEGIN
  RETURN pf.rbuf.pixel_width;
END pixel_width;

PROCEDURE (pf: pixel_formats_ptr) pixel_height*(): bas.int32;
BEGIN
  RETURN pf.rbuf.pixel_height;
END pixel_height;

PROCEDURE (pf: pixel_formats_ptr) bpp*(): bas.int32;
BEGIN
  RETURN pf.rbuf.bpp;
END bpp;

PROCEDURE (pf: pixel_formats_ptr) bits_per_pix*(): bas.int32;
BEGIN
  RETURN pf.rbuf.bits_per_pix;
END bits_per_pix;


(* setvalue *)

PROCEDURE (pf: pixel_formats_ptr) setvalue*(x, y, offset: bas.int32; value: bas.int8u);
BEGIN

END setvalue;

PROCEDURE (pf: pixel_formats_ptr) Construct*(rbuf: rb.rendering_buffer_ptr);
BEGIN
  pf.ConstructI();
  pf.rbuf.Attach(rbuf);
(*  pf.pixel_width := pf.rbuf.byte_width DIV pf.rbuf.bppabs();
  pf.pixel_height := pf.rbuf.byte_height;*)
END Construct;

PROCEDURE (pf: pixel_formats_ptr) copy_pixel*(x, y: bas.int32; c: col.aggclr);
BEGIN
END copy_pixel;

PROCEDURE (pf: pixel_formats_ptr) blend_pixel*(x, y: bas.int32; c: col.aggclr; cover: bas.int8u);
BEGIN
END blend_pixel;

PROCEDURE (pf: pixel_formats_ptr) pixel*(x, y: bas.int32; VAR c: col.aggclr);
BEGIN
END pixel;

(* Процедуры copy_?line тиражирют цвет c вдоль линии (координаты x или y) на длину len пикселей *)
PROCEDURE (pf: pixel_formats_ptr) copy_hline*(x, y, len: bas.int32; c: col.aggclr);
BEGIN
END copy_hline;

PROCEDURE (pf: pixel_formats_ptr) copy_vline*(x, y, len: bas.int32; c: col.aggclr);
BEGIN
END copy_vline;

PROCEDURE (pf: pixel_formats_ptr) blend_hline*(x, y, len: bas.int32; c: col.aggclr; cover: bas.int8u);
BEGIN
END blend_hline;

PROCEDURE (pf: pixel_formats_ptr) blend_vline*(x, y, len: bas.int32; c: col.aggclr; cover: bas.int8u);
BEGIN
END blend_vline;

PROCEDURE (pf: pixel_formats_ptr) blend_solid_hspan*(x, y, len: bas.int32; c: col.aggclr;
  VAR covers: col.covers_array_ptr; cov_offset: bas.int32);
BEGIN
END blend_solid_hspan;

PROCEDURE (pf: pixel_formats_ptr) blend_solid_vspan*(x, y, len: bas.int32; c: col.aggclr;
  VAR covers: col.covers_array_ptr; cov_offset: bas.int32);
BEGIN
END blend_solid_vspan;

PROCEDURE (pf: pixel_formats_ptr) blend_color_hspan*(x, y, len: bas.int32;
  VAR colors: col.colors_array_ptr; col_offset: bas.int32;
  VAR covers: col.covers_array_ptr; cov_offset: bas.int32;
       cover: bas.int8u);
BEGIN
END blend_color_hspan;

PROCEDURE (pf: pixel_formats_ptr) blend_color_vspan*(x, y, len: bas.int32;
  VAR colors: col.colors_array_ptr; col_offset: bas.int32;
  VAR covers: col.covers_array_ptr; cov_offset: bas.int32;
       cover: bas.int8u);
BEGIN
END blend_color_vspan;

PROCEDURE (pf: pixel_formats_ptr) copy_from*(from: rb.rendering_buffer_ptr; xdst, ydst, xsrc, ysrc, len: bas.int32);
BEGIN
END copy_from;

PROCEDURE (pf: pixel_formats_ptr) blend_from*(from: pixel_formats_ptr; xdst, ydst, xsrc, ysrc, len: bas.int32; cover: bas.int8u);
BEGIN
END blend_from;

PROCEDURE (pf: pixel_formats_ptr) SetCompOp*(comp_op: bas.int32);
BEGIN
  pf.comp_op := comp_op;
END SetCompOp;

(* В процедурах DoApplyGamma* производится применение gamma-функции к пикселю *)
PROCEDURE (pf: pixel_formats_ptr) DoApplyGammaDir*(x, y: bas.int32);
BEGIN
END DoApplyGammaDir;

PROCEDURE (pf: pixel_formats_ptr) DoApplyGammaInv*(x, y: bas.int32);
BEGIN
END DoApplyGammaInv;

(* В процедурах apply_gamma_* производится попиксельный перебор массива. *)
PROCEDURE (pf: pixel_formats_ptr) apply_gamma_dir*(VAR g: Gamma_ptr; order: order_type);
VAR
  x,y: bas.int32;
BEGIN
  pf.gamma := g;
  pf.byte_order := order;

  y := 0;
  WHILE y < pf.pixel_height() DO
    x := pf.pixel_width();
    REPEAT
      pf.DoApplyGammaDir(x,y);
      DEC(x);
    UNTIL x = 0;
    INC(y);
  END;
END apply_gamma_dir;

PROCEDURE (pf: pixel_formats_ptr) apply_gamma_inv*(VAR g: Gamma_ptr; order: order_type);
VAR
  x,y: bas.int32;
BEGIN
  pf.gamma := g;
  pf.byte_order := order;

  y := 0;
  WHILE y < pf.pixel_height() DO
    x := pf.pixel_width();
    REPEAT
      pf.DoApplyGammaInv(x,y);
      DEC(x);
    UNTIL x = 0;
    INC(y);
  END;
END apply_gamma_inv;

(* blend_from : func_blend_from; *)

PROCEDURE InitOrder();
BEGIN
  rgb_order.R  := 0;
  rgb_order.G  := 1;
  rgb_order.B  := 2;
  rgb_order.A  := 3;

  bgr_order.R  := 2;
  bgr_order.G  := 1;
  bgr_order.B  := 0;
  bgr_order.A  := 3;

  rgba_order.R := 0;
  rgba_order.G := 1;
  rgba_order.B := 2;
  rgba_order.A := 3;

  bgra_order.R := 2;
  bgra_order.G := 1;
  bgra_order.B := 0;
  bgra_order.A := 3;

  argb_order.R := 1;
  argb_order.G := 2;
  argb_order.B := 3;
  argb_order.A := 0;

  abgr_order.R := 3;
  abgr_order.G := 2;
  abgr_order.B := 1;
  abgr_order.A := 0;

  none_order.R := 0;
  none_order.G := 0;
  none_order.B := 0;
  none_order.A := 0;
END InitOrder;

BEGIN
  InitOrder();
END AggPixfmtBase.
