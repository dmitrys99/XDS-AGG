<* +MAIN *>
MODULE tut5;

IMPORT
  arb := AggRenderingBuffer,
  apf := AggPixfmtRgb24,
  age := AggEllipse,
  reb := AggRendererBase,
  ars := AggRendererScanline,
  rsaa:= AggRasterizerScanlineAA,
  rns := AggRenderScanlines,
  scl := AggScanlineP,
  col := AggColor,
  utl := tut_utl;

CONST
  frame_width  = 320;
  frame_height = 200;
  frame_bpp    = 3;
VAR
  rbuf: arb.rendering_buffer_ptr;
  pixf: apf.pixel_format_rgb24_ptr;
  el  : age.ellipse_ptr;
  rbas: reb.renderer_base_ptr;
  c   : col.aggclr;
  raa : ars.renderer_scanline_aa_solid_ptr;
  ras : rsaa.rasterizer_scanline_aa_ptr;
  sl  : scl.scanline_p8_ptr;
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
  (*el.ConstructEl(frame_width DIV 2, frame_height DIV 2, frame_width * 2 - 2, frame_height * 2-10, 100, TRUE); *)
  NEW(el);
  el.ConstructEl(frame_width DIV 2, frame_height DIV 2, 30, 60, 100, TRUE);

  c.Constructrgba(0, 0, 255, 255);
  raa.color := c;

  ras.add_path(el, 0);
  rns.render_scanlines(ras, sl, raa);

  utl.WritePPM("tut5.ppm", rbuf);
END tut5.
