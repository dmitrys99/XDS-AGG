MODULE AggSpanGenerator;
IMPORT
  bas := AggBasics,
  asa := AggSpanAllocator,
  avs := AggVertexSource,
  col := AggColor;

TYPE
  span_generator_ptr* = POINTER TO span_generator;
  span_generator* = RECORD(avs.vertex_source)
    alloc-: asa.span_allocator;
  END;

PROCEDURE (VAR sg: span_generator) ConstructSA*(VAR sa: asa.span_allocator);
BEGIN
  sg.alloc := sa;
END ConstructSA ;

PROCEDURE (VAR sg: span_generator) prepare*(max_span_len: bas.int32);
BEGIN
  sg.alloc.allocate(max_span_len);
END prepare;


<*+WOFF301*>
PROCEDURE (VAR sg: span_generator) generate*(x, y: bas.int32; len: bas.int32): col.colors_array_ptr;
BEGIN
  RETURN sg.alloc.span;
END generate;
<*-WOFF301*>

END AggSpanGenerator.