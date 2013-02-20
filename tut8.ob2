<* +MAIN *>
MODULE tut8;

IMPORT
  bas := AggBasics,
  arb := AggRenderingBuffer,
  apf := AggPixfmtRgb24,
  reb := AggRendererBase,
  ars := AggRendererScanline,
  rsaa:= AggRasterizerScanlineAA,
  rns := AggRenderScanlines,
  scl := AggScanlineP,
  col := AggColor,
  aa  := AggArc,
  utl := tut_utl;

CONST
  frame_width  = 320;
  frame_height = 200;
  frame_bpp    =   3;

VAR
  rbuf: arb.rendering_buffer_ptr;
  pixf: apf.pixel_format_rgb24_ptr;
  rbas: reb.renderer_base_ptr;
  c   : col.aggclr;
  a   : aa.arc_ptr;

  raa : ars.renderer_scanline_aa_solid_ptr;
  ras : rsaa.rasterizer_scanline_aa_ptr;
  sl  : scl.scanline_p8_ptr;

(*  a   : aba.bezier_arc_ptr; *)

BEGIN
  NEW(rbuf);
  rbuf.Construct1(frame_height, frame_width, frame_bpp);
  utl.Fill(rbuf, 0);

  c.Constructrgba(0,0,0,255);

  NEW(pixf);
  pixf.Construct(rbuf);

  NEW(rbas);
  rbas.Construct(pixf);
  rbas.clear(c);

  NEW(raa);
  raa.Construct(rbas);

  NEW(ras);
  ras.Construct();
  NEW(sl);
  sl.Construct();

  c.Constructrgba(255, 0, 0, 255);
  raa.color := c;

  NEW(a);
  a.ConstructArc(100,100, 5, 5, 0, 2*bas.pi, TRUE);
  (*x, y, rx, ry, a1, a2: bas.double; ccw: BOOLEAN);*)

  c.Constructrgba(0, 0, 255, 255);
  raa.color := c;

  ras.add_path(a, 0);
  rns.render_scanlines(ras, sl, raa);


  utl.WritePPM("tut8.ppm", rbuf);
END tut8.
