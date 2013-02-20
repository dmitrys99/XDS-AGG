MODULE AggColorConv;
IMPORT
  bas := AggBasics,
  arb := AggRenderingBuffer;


TYPE copy_functor* = PROCEDURE (VAR dst, src: ARRAY OF bas.int8u);

PROCEDURE color_conv*(dst, src: arb.rendering_buffer_ptr; cp: copy_functor);
VAR
  y, height, width: bas.int32;
BEGIN
  width  := bas.agg_min32(src.byte_width,  dst.byte_width);
  height := bas.agg_min32(src.byte_height, dst.byte_height);
  IF width > 0 THEN
    FOR y := 0 TO height - 1 DO
      cp(dst.buf[dst.row(y)], src.buf[src.row(y)]);
    END;
  END;
END color_conv;

PROCEDURE color_conv_same*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  bas.agg_move(bas.agg_adr(src[0]), bas.agg_adr(dst[0]), bas.agg_min32(LEN(src), LEN(dst)));
END color_conv_same;

END AggColorConv.