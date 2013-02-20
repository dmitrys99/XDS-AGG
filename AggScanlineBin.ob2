MODULE AggScanlineBin;

IMPORT
  bas := AggBasics,
  scl := AggScanline;

TYPE
  span_bin* = RECORD(scl.span)
  END;

  scanline_bin_ptr* = POINTER TO scanline_bin;
  scanline_bin* = RECORD(scl.scanline)
  END;

PROCEDURE (VAR sb: scanline_bin) Construct*();
BEGIN
  sb.Construct^();
END Construct;

PROCEDURE (VAR sb: scanline_bin) Destruct*();
BEGIN
  sb.spans := NIL;
END Destruct;

PROCEDURE (VAR sb: scanline_bin) reset*(min_x, max_x: bas.int32);
VAR
  maxlen: bas.int32;
BEGIN
  maxlen := max_x - min_x + 3;

  IF maxlen > sb.maxlen THEN
    NEW(sb.spans, maxlen);
    sb.maxlen := maxlen;
  END;

  sb.last_x   := 7FFFFFF0H;
  sb.cur_span := 0;
END reset;

PROCEDURE (VAR sb: scanline_bin) reset_spans*();
BEGIN
  sb.last_x   := 7FFFFFF0H;
  sb.cur_span := 0;
END reset_spans;

PROCEDURE (VAR sb: scanline_bin) finalize*(y: bas.int32);
BEGIN
  sb.y := y;
END finalize;

<*+WOFF301*>
PROCEDURE (VAR sb: scanline_bin) add_cell*(x: bas.int32; cover: bas.int8u);
BEGIN

  IF x = sb.last_x + 1 THEN
    INC(sb.spans[sb.cur_span].len)
  ELSE
    INC(sb.cur_span);

    sb.spans[sb.cur_span].x   := x;
    sb.spans[sb.cur_span].len := 1;
  END;

  sb.last_x := x;
END add_cell;

PROCEDURE (VAR sb: scanline_bin) add_span*(x, len: bas.int32; cover: bas.int8u);
BEGIN

  IF x = sb.last_x + 1 THEN
     sb.spans[sb.cur_span].len := sb.spans[sb.cur_span].len + len;
  ELSE
    INC(sb.cur_span);

    sb.spans[sb.cur_span].x   := x;
    sb.spans[sb.cur_span].len := len;
  END;

  sb.last_x := x + len - 1;
END add_span;

<*-WOFF301*>
(*
PROCEDURE (VAR sb: scanline_bin) num_spans*(): bas.int32;
BEGIN
  IF sb.cur_span = sb.num_spans^() THEN
    RETURN sb.num_spans^()
  ELSE
    RETURN sb.cur_span;
  END;
END num_spans;
*)
END AggScanlineBin.