MODULE AggScanline;

IMPORT
  bas := AggBasics,
  col := AggColor;

TYPE
  span_ptr* = POINTER TO span;
  span* = RECORD
    x*,len* : bas.int32;
    covers_offset*: bas.int32;
  END;

  span_array_ptr* = POINTER TO ARRAY OF span;

(*  span_obj_ptr* = POINTER TO span_obj;
  span_obj*     = RECORD
    covers_offset*: bas.int32;
  END; *)

  scanline_ptr* = POINTER TO scanline;
  scanline* = RECORD
    y*        : bas.int32;
    maxlen*   : bas.int32;
    last_x*   : bas.int32;
    spans*    : span_array_ptr;
    covers*   : col.covers_array_ptr;
    cur_span* : bas.int32;
  END;

(*==========*)
(* span_obj *)
(*==========*)
(*
PROCEDURE (VAR so: span_obj) x*(): bas.int32;
BEGIN
  RETURN 0;
END x;

PROCEDURE (VAR so: span_obj) len*(): bas.int32;
BEGIN
  RETURN 0;
END len;

PROCEDURE (VAR so: span_obj) inc_operator*();
BEGIN

END inc_operator;
*)
(*==========*)
(* scanline *)
(*==========*)
PROCEDURE (VAR sl: scanline) Construct*();
BEGIN
  sl.maxlen     := 0;
  sl.last_x     := 7FFFFFF0H;
  sl.y          := 0;
  sl.covers     := NIL;
  sl.spans      := NIL;
  sl.cur_span   := -1;
END Construct;

PROCEDURE (VAR sl: scanline) reset*(min_x, max_x: bas.int32);
BEGIN

END reset;

PROCEDURE (VAR sl: scanline) reset_spans*();
BEGIN

END reset_spans;

PROCEDURE (VAR sl: scanline) finalize*(y: bas.int32);
BEGIN

END finalize;

PROCEDURE (VAR sl: scanline) add_cell*(x: bas.int32; cover: bas.int8u);
BEGIN

END add_cell;

PROCEDURE (VAR sl: scanline) add_cells*(x, len, covers_offset: bas.int32; covers: col.covers_array_ptr);
BEGIN

END add_cells;

PROCEDURE (VAR sl: scanline) add_span*(x, len: bas.int32; cover: bas.int8u);
BEGIN

END add_span;

PROCEDURE (VAR sl: scanline) num_spans*(): bas.int32;
BEGIN
  RETURN sl.cur_span;
END num_spans;

PROCEDURE (VAR sl: scanline) is_plain_span*(): BOOLEAN;
BEGIN
  RETURN FALSE;
END is_plain_span;

PROCEDURE (VAR sl: scanline) is_embedded*(): BOOLEAN;
BEGIN
  RETURN FALSE;
END is_embedded;

END AggScanline.