MODULE AggScanlineU;

IMPORT
  bas := AggBasics,
  col := AggColor,
  scl := AggScanline,
  asb := AggScanlineBin;

TYPE
  span_u8* = RECORD(scl.span)
  END;

  scanline_u8_ptr* = POINTER TO scanline_u8;
  scanline_u8* = RECORD(asb.scanline_bin)
    covers_ptr : bas.int32;
    min_x      : bas.int32;
  END;

PROCEDURE (VAR su: scanline_u8) Construct*();
BEGIN
  su.Construct^();
  su.covers_ptr := -1;
END Construct;

PROCEDURE (VAR su: scanline_u8) Destruct*();
BEGIN
  su.spans := NIL;
  su.covers := NIL;
END Destruct;

PROCEDURE (VAR su: scanline_u8) reset*(min_x, max_x: bas.int32);
VAR
  maxlen: bas.int32;
BEGIN
  maxlen := max_x - min_x + 2;

  IF maxlen > su.maxlen THEN
    NEW(su.spans, maxlen);
    NEW(su.covers, maxlen);
    su.maxlen := maxlen;
  END;

 su.last_x   := 7FFFFFF0H;
 su.min_x    := min_x;
 su.cur_span := 0;
END reset;

PROCEDURE (VAR su: scanline_u8) reset_spans*();
BEGIN
  su.last_x   := 7FFFFFF0H;
  su.cur_span := 0;
END reset_spans;

PROCEDURE (VAR su: scanline_u8) add_cell*(x: bas.int32; cover: bas.int8u);
BEGIN
  DEC(x, su.min_x);

  su.covers^[x] := cover;

  IF x = su.last_x + 1 THEN
    INC(su.spans[su.cur_span].len)
  ELSE
    INC(su.cur_span);

    su.spans[su.cur_span].x   := x + su.min_x;
    su.spans[su.cur_span].len := 1;

    su.spans[su.cur_span].covers_offset := x;
  END;

  su.last_x := x;
END add_cell;

PROCEDURE (VAR su: scanline_u8) add_cells*(x, len, covers_offset: bas.int32; covers: col.covers_array_ptr);
VAR
  i: bas.int32;
BEGIN
  DEC(x, su.min_x);

  FOR i := covers_offset TO covers_offset + len - 1 DO
    su.covers^[su.covers_ptr + i - covers_offset] := covers[i];
  END;

  IF x = su.last_x + 1 THEN
    INC(su.spans[su.cur_span].len);
  ELSE
    INC(su.cur_span);

    su.spans[su.cur_span].x   := x + su.min_x;
    su.spans[su.cur_span].len := len;

    su.spans[su.cur_span].covers_offset := x;
  END;

  su.last_x := x + len - 1;
END add_cells;

PROCEDURE (VAR su: scanline_u8) add_span*(x, len: bas.int32; cover: bas.int8u);
VAR
  i: bas.int32;
BEGIN
  DEC(x, su.min_x);

  FOR i := 0 TO len - 1 DO
    su.covers^[su.covers_ptr + x + i] := cover;
  END;

  IF x = su.last_x + 1 THEN
    INC(su.spans[su.cur_span].len);
  ELSE
    INC(su.cur_span);

    su.spans[su.cur_span].x   := x + su.min_x;
    su.spans[su.cur_span].len := len;

    su.spans[su.cur_span].covers_offset := x;
  END;

  su.last_x := x + len - 1;
END add_span;

END AggScanlineU.