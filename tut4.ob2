<* +MAIN *>
MODULE tut4;

IMPORT
  arp := AggRendererPrimitives,
  rb  := AggRenderingBuffer,
  pb  := AggPixfmtRgb24,
  utl := tut_utl;

CONST
  frame_width  = 320;
  frame_height = 200;
  frame_bpp    = 3;

VAR
  renb: rb.rendering_buffer;
  pixf: pb.pixel_format_rgb24_ptr;
  rp  : arp.renderer_primitives;
BEGIN
  renb.Create(frame_height, frame_width, frame_bpp);
  utl.Fill(renb, 100);

  NEW(pixf);
  pixf.Construct(renb);

  rp.Construct(pixf);

  rp.line_color.ConstructDbl(0,0,0,1);
  rp.fill_color.ConstructDbl(0,1.0,0.33,1);

  rp.rectangle(5,5,100,100);
  rp.outlined_rectangle(20,120,200,180);
  rp.fill_color.ConstructDbl(0.7, 1.0, 0.1, 0.4);
  rp.solid_rectangle(90,90,150,190);

  rp.ellipse(80, 100, 40, 50);
  rp.fill_color.ConstructDbl(0, 0.9, 0, 0.8);
  rp.outlined_ellipse(180, 100, 30, 50);

  rp.solid_ellipse(220, 100, 30, 50);
  rp.line_color.ConstructDbl(0,0,1,1);
  rp.line(rp.coord(-10),
          rp.coord(-10),
          rp.coord(1000),
          rp.coord(2000), FALSE);
  (*rp.move_to(0, frame_height);
  rp.line_to(frame_width, 0, FALSE);*)
  utl.WritePPM("tut4.ppm", renb);
END tut4.