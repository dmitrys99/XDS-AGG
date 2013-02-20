MODULE AggPixfmtGrayBase;

IMPORT
  bas := AggBasics,
  pfb := AggPixfmtBase,
  col := AggColor,
  arb := AggRenderingBuffer;

TYPE
  pixel_format_gray_base_ptr* = POINTER TO pixel_format_gray_base;
  pixel_format_gray_base    * = RECORD (pfb.pixel_formats)
    apply*: pfb.Gamma_ptr;
    step*, offset*: bas.int32;
  END;

PROCEDURE (pfg: pixel_format_gray_base_ptr) ConstructI*();
BEGIN
  pfg.ConstructI^();
  pfg.step   := 1;
  pfg.offset := 0;
  pfg.apply  := NIL;
END ConstructI;

PROCEDURE (pfg: pixel_format_gray_base_ptr) copy_pixel*(x, y: bas.int32; c: col.aggclr);
BEGIN
  pfg.rbuf.buf^[pfg.rbuf.row(y), x * pfg.step + pfg.offset] := c.v;
END copy_pixel;


PROCEDURE (pfg: pixel_format_gray_base_ptr) pixel*(x, y: bas.int32; VAR c: col.aggclr);
VAR
  v: bas.int8u;
BEGIN
  v := pfg.rbuf.buf^[pfg.rbuf.row(y), x * pfg.step + pfg.offset];
  c.ConstructV(v, col.base_mask);
END pixel;

PROCEDURE (pfg: pixel_format_gray_base_ptr) copy_hline*(x, y, len: bas.int32; c: col.aggclr);
VAR
  ax: bas.int32;
BEGIN
  ax := x;
  REPEAT
    pfg.copy_pixel(ax, y, c);
    INC(ax);
    DEC(len);
  UNTIL len = 0;
END copy_hline;

PROCEDURE (pfg: pixel_format_gray_base_ptr) copy_vline*(x, y, len: bas.int32; c: col.aggclr);
VAR
  ay: bas.int32;
BEGIN
  ay := y;
  REPEAT
    pfg.copy_pixel(x, ay, c);
    INC(ay);
    DEC(len);
  UNTIL len = 0;
END copy_vline;

PROCEDURE (pfg: pixel_format_gray_base_ptr) copy_from*(from: arb.rendering_buffer_ptr; xdst, ydst, xsrc, ysrc, len: bas.int32);
BEGIN
  bas.agg_move(
    bas.agg_adr(pfg.rbuf.buf^[pfg.rbuf.row(ysrc), xsrc * pfg.rbuf.bppabs()]),
    bas.agg_adr(from.buf^[pfg.rbuf.row(ydst), xdst * pfg.rbuf.bppabs()]), len * from.bppabs());
END copy_from;

PROCEDURE (pfg: pixel_format_gray_base_ptr) DoApplyGammaDir*(x, y: bas.int32);
BEGIN
  pfg.rbuf.buf^[pfg.rbuf.row(y), x * pfg.step + pfg.offset] :=
    SHORT(SHORT(pfg.apply.dir(pfg.rbuf.buf^[pfg.rbuf.row(y), x * pfg.step + pfg.offset])));
END DoApplyGammaDir;

PROCEDURE (pfg: pixel_format_gray_base_ptr) DoApplyGammaInv*(x, y: bas.int32);
BEGIN
  pfg.rbuf.buf^[pfg.rbuf.row(y), x * pfg.step + pfg.offset] :=
    SHORT(SHORT(pfg.apply.inv(pfg.rbuf.buf^[pfg.rbuf.row(y), x * pfg.step + pfg.offset])));
END DoApplyGammaInv;

END AggPixfmtGrayBase.

PROCEDURE (pfg: pixel_format_gray_base_ptr)
