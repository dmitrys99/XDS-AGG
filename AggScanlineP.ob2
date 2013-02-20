MODULE AggScanlineP;

IMPORT
  bas := AggBasics,
  col := AggColor,
  scl := AggScanline,
  asb := AggScanlineBin;

TYPE
  span_p8* = RECORD(scl.span)
  END;

  scanline_p8_ptr* = POINTER TO scanline_p8;
  scanline_p8* = RECORD(asb.scanline_bin)
    covers_ptr : bas.int32;
  END;

PROCEDURE (VAR sp: scanline_p8) Construct*();
BEGIN
  sp.Construct^();
  sp.covers_ptr := -1;
END Construct;

PROCEDURE (VAR sp: scanline_p8) Destruct*();
BEGIN
  sp.spans := NIL;
  sp.covers := NIL;
END Destruct;

PROCEDURE (VAR sp: scanline_p8) reset*(min_x, max_x: bas.int32);
VAR
  maxlen: bas.int32;
BEGIN
  maxlen := max_x - min_x + 3;

  IF maxlen > sp.maxlen THEN
    NEW(sp.spans, maxlen);
    NEW(sp.covers, maxlen);
    sp.maxlen := maxlen;
  END;

  sp.last_x     := 7FFFFFF0H;
  sp.cur_span   := 0;
  sp.covers_ptr := 0;

  sp.spans[sp.cur_span].len := 0;
END reset;

PROCEDURE (VAR sp: scanline_p8) reset_spans*();
BEGIN
  sp.last_x   := 7FFFFFF0H;

  sp.cur_span := 0;
  sp.covers_ptr := 0;

  sp.spans[sp.cur_span].len := 0;
END reset_spans;

PROCEDURE (VAR sp: scanline_p8) finalize*(y: bas.int32);
BEGIN
  sp.y := y;
END finalize;

PROCEDURE (VAR sp: scanline_p8) add_cell*(x: bas.int32; cover: bas.int8u);
BEGIN
  sp.covers^[sp.covers_ptr] := cover;

  IF (x = sp.last_x + 1) & (sp.spans[sp.cur_span].len > 0 ) THEN
    INC(sp.spans[sp.cur_span].len)
  ELSE
    INC(sp.cur_span);
    sp.spans[sp.cur_span].covers_offset := sp.covers_ptr;
    sp.spans[sp.cur_span].x   := x;
    sp.spans[sp.cur_span].len := 1;
  END;

 sp.last_x := x;

 INC(sp.covers_ptr);
END add_cell;


PROCEDURE (VAR sp: scanline_p8) add_cells*(x, len, covers_offset: bas.int32; covers: col.covers_array_ptr);
VAR
  i: bas.int32;
BEGIN
  ASSERT(covers # NIL);
  ASSERT(sp.covers # NIL);

  FOR i := covers_offset TO covers_offset + len - 1 DO
    sp.covers^[sp.covers_ptr + i - covers_offset] := covers[i];
  END;

  IF (x = sp.last_x + 1) & (sp.spans[sp.cur_span].len > 0) THEN
    INC(sp.spans[sp.cur_span].len);
  ELSE
    INC(sp.cur_span);

    sp.spans[sp.cur_span].covers_offset := sp.covers_ptr;
    sp.spans[sp.cur_span].x             := x;
    sp.spans[sp.cur_span].len           := len;
  END;

  INC(sp.covers_ptr, len);

  sp.last_x := x + len - 1;
END add_cells;

PROCEDURE (VAR sp: scanline_p8) add_span*(x, len: bas.int32; cover: bas.int8u);
BEGIN
  IF (x = sp.last_x + 1) &
     (sp.spans^[sp.cur_span].len < 0) &
     (cover = sp.covers^[sp.spans^[sp.cur_span].covers_offset]) THEN
    DEC(sp.spans^[sp.cur_span].len, len);
  ELSE
    sp.covers^[sp.covers_ptr] := cover;

    INC(sp.cur_span);

    sp.spans^[sp.cur_span].covers_offset := sp.covers_ptr;
    sp.spans^[sp.cur_span].x             := x;
    sp.spans^[sp.cur_span].len           := len;
    sp.spans^[sp.cur_span].len           := -sp.spans[sp.cur_span].len;

    INC(sp.covers_ptr);
  END;

  sp.last_x := x + len - 1;
END add_span;

(*
PROCEDURE (VAR sp: scanline_p8) num_spans*(): bas.int32;
BEGIN
  IF sp.cur_span = sp.num_spans^() THEN
    RETURN sp.num_spans^()
  ELSE
    RETURN sp.cur_span;
  END;
END num_spans;
*)
END AggScanlineP.
