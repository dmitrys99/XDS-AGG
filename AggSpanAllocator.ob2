MODULE AggSpanAllocator;

IMPORT
  bas := AggBasics,
  col := AggColor;

TYPE
  span_allocator_ptr* = POINTER TO span_allocator;
  span_allocator* = RECORD
    max_span_len-: bas.int32;
    span-: col.colors_array_ptr;
  END;

PROCEDURE (VAR sa: span_allocator) Construct*();
BEGIN
  sa.max_span_len := 0;
  sa.span         := NIL;
END Construct;

PROCEDURE (VAR sa: span_allocator) Destruct*();
BEGIN
  sa.max_span_len := 0;
  sa.span         := NIL;
END Destruct;

PROCEDURE (VAR sa: span_allocator) allocate*(max_span_len: bas.int32);
BEGIN
  IF max_span_len > sa.max_span_len THEN
    NEW(sa.span, max_span_len);
    sa.max_span_len := max_span_len;
  END;
END allocate;

END AggSpanAllocator.