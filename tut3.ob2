<* +MAIN *>
MODULE tut3;

IMPORT
  AggBasics,
  rb  := AggRenderingBuffer,
  pb  := AggPixfmtRgb24,
  utl := tut_utl,
  col := AggColor,
  pfb := AggPixfmtBase;

CONST
  frame_width  = 320;
  frame_height = 200;
  frame_bpp    = 3;

VAR
  renb: rb.rendering_buffer;
  pixf: pb.pixel_format_rgb24;

(* Рисуем с помощью pixel formatter *)

PROCEDURE DrawBlackFrame*(pixf: pb.pixel_format_rgb24);
VAR
  c: col.aggclr;
  i: AggBasics.int32;
BEGIN
  c.ConstructDbl(0,0,0,1);
  i := 0;
  WHILE i < pixf.pixel_width DO
    pixf.copy_pixel(i, 0, c);
    pixf.copy_pixel(i, pixf.pixel_height-1, c);
    INC(i);
  END;

  i := 0;
  WHILE i < pixf.pixel_height DO
    pixf.copy_pixel(0, i, c);
    pixf.copy_pixel(pixf.pixel_width-1, i, c);
    INC(i);
  END;

END DrawBlackFrame;

PROCEDURE DrawGradient*(pixf: pb.pixel_format_rgb24);
VAR
  span: col.colors_array_ptr;
  covers: col.covers_array_ptr;
  i: AggBasics.int32;
BEGIN
  NEW(span, frame_width);
  covers := NIL;
  FOR i := 0 TO frame_width - 1 DO
    span^[i].ConstructWave(380.0 + 400.0 * i / frame_width, 0.8);
  END;
  i := 0;
  WHILE i < pixf.pixel_height DO
    pixf.blend_color_hspan(0, i, frame_width, span, 0, covers, 0, 200);
    INC(i);
  END;
END DrawGradient;

BEGIN
  renb.Create(frame_height, frame_width, frame_bpp);
  utl.Fill(renb, 255);
  pixf.Construct(renb);

  DrawGradient(pixf);
  DrawBlackFrame(pixf);

  utl.WritePPM("tut3.ppm", renb);
  renb.Destroy();
END tut3.