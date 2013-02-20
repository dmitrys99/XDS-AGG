<* +MAIN *>
MODULE tut2;

IMPORT
  AggBasics,
  rb  := AggRenderingBuffer,
  pb  := AggPixfmtRgb24,
  utl := tut_utl,
  col := AggColor;

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
  c.ConstructDbl(0,1.0,0,1.0);
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

BEGIN
  renb.Create(frame_height, frame_width, frame_bpp);
  utl.Fill(renb, 200);
  pixf.Construct(renb);
  DrawBlackFrame(pixf);
  utl.WritePPM("tut2.ppm", renb);
  renb.Destroy();
END tut2.