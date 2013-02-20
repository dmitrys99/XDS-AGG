MODULE AggPixfmt24;

IMPORT
  pfbase := AggPixfmtBase,
  bas := AggBasics,
  rb := AggRenderingBuffer,
  AggColor;

TYPE
  pixel_format24_ptr* = POINTER TO pixel_format24;
  pixel_format24* = RECORD(pfbase.pixel_formats)
  END;

PROCEDURE (pf: pixel_format24_ptr) ConstructI*();
BEGIN
  pf.ConstructI^();
END ConstructI;

(* copy_from *)

(* Копируем пиксели из другого буфера. Предполагается, что буферы одинакового размера. Координаты и длины - в пикселях *)
PROCEDURE (pf: pixel_format24_ptr) copy_from*(from: rb.rendering_buffer_ptr; xdst, ydst, xsrc, ysrc, len: bas.int32);
BEGIN
  bas.agg_move(
    bas.agg_adr(from.buf^[ysrc, xsrc + xsrc + xsrc]),
    bas.agg_adr(pf.rbuf.buf^[ydst, xdst + xdst + xdst]),
    len * 3);
END copy_from;

(* value *)

PROCEDURE (pf: pixel_format24_ptr) value*(x, y, offset: bas.int32): bas.int8u;
BEGIN
  RETURN pf.rbuf.buf^[pf.rbuf.row(y), x + x + x + offset];
END value;

(* setvalue *)

PROCEDURE (pf: pixel_format24_ptr) setvalue*(x, y, offset: bas.int32; value: bas.int8u);
BEGIN
  pf.rbuf.buf^[pf.rbuf.row(y), x + x + x + offset] := value;
END setvalue;

PROCEDURE (pf: pixel_format24_ptr) DoApplyGammaDir*(x,y: bas.int32);
BEGIN
  pf.setvalue(x, y, pf.byte_order.R, SHORT(SHORT(pf.gamma.dir(pf.value(x, y, pf.byte_order.R)))));
  pf.setvalue(x, y, pf.byte_order.G, SHORT(SHORT(pf.gamma.dir(pf.value(x, y, pf.byte_order.G)))));
  pf.setvalue(x, y, pf.byte_order.B, SHORT(SHORT(pf.gamma.dir(pf.value(x, y, pf.byte_order.B)))));
END DoApplyGammaDir;

PROCEDURE (pf: pixel_format24_ptr) DoApplyGammaInv*(x,y: bas.int32);
BEGIN
  pf.setvalue(x, y, pf.byte_order.R, SHORT(SHORT(pf.gamma.inv(pf.value(x, y, pf.byte_order.R)))));
  pf.setvalue(x, y, pf.byte_order.G, SHORT(SHORT(pf.gamma.inv(pf.value(x, y, pf.byte_order.G)))));
  pf.setvalue(x, y, pf.byte_order.B, SHORT(SHORT(pf.gamma.inv(pf.value(x, y, pf.byte_order.B)))));
END DoApplyGammaInv;

(* copy_pixel *)

PROCEDURE (pf: pixel_format24_ptr) copy_pixel*(x, y: bas.int32; c: AggColor.aggclr);
BEGIN
  pf.setvalue(x,y, pf.byte_order.R, c.r);
  pf.setvalue(x,y, pf.byte_order.G, c.g);
  pf.setvalue(x,y, pf.byte_order.B, c.b);
END copy_pixel;

(* pixel *)

PROCEDURE (pf: pixel_format24_ptr) pixel*(x, y: bas.int32; VAR c: AggColor.aggclr);
BEGIN
  c.r := pf.value(x, y, pf.byte_order.R);
  c.g := pf.value(x, y, pf.byte_order.G);
  c.b := pf.value(x, y, pf.byte_order.B);
END pixel;

(* copy_hline
  От пикселя x,y нарисовать len пикселей цветом c.
*)

PROCEDURE (pf: pixel_format24_ptr) copy_hline*(x, y, len: bas.int32; c: AggColor.aggclr);
VAR
  ax: bas.int32;
BEGIN
  ax := x;
  REPEAT
    pf.copy_pixel(ax, y, c);
    INC(ax);
    DEC(len);
  UNTIL len = 0;
END copy_hline;

(* copy_vline *)

PROCEDURE (pf: pixel_format24_ptr) copy_vline*(x, y, len: bas.int32; c: AggColor.aggclr);
VAR
  ay: bas.int32;
BEGIN
  ay := y;
  REPEAT
    pf.copy_pixel(x, ay, c);
    INC(ay);
    DEC(len);
  UNTIL len = 0;
END copy_vline;

END AggPixfmt24.