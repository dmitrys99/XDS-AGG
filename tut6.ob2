<* +MAIN *>
MODULE tut6;

IMPORT
  arb := AggRenderingBuffer,
  apf := AggPixfmtRgb24,
  age := AggEllipse,
  reb := AggRendererBase,
  ars := AggRendererOutlineAA,
  rsaa:= AggRasterizerOutlineAA,
  col := AggColor,
  (*aba := AggBezierArc,*)
  utl := tut_utl;

CONST
  frame_width  = 320;
  frame_height = 200;
  frame_bpp    =   3;

VAR
  rbuf: arb.rendering_buffer_ptr;
  pixf: apf.pixel_format_rgb24_ptr;
  el  : age.ellipse_ptr;
  rbas: reb.renderer_base_ptr;
  c   : col.aggclr;
  raa : ars.renderer_outline_aa_ptr;
  ras : rsaa.rasterizer_outline_aa_ptr;
  lp  : ars.line_profile_aa_ptr;
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

  NEW(lp);
  lp.Init();
  lp.set_width(10);

  NEW(raa);
  raa.Construct(rbas, lp);

  NEW(ras);
  ras.Construct(raa);

  NEW(el);
  el.ConstructEl(frame_width DIV 2, frame_height DIV 2, 30, 60, 100, TRUE);

  c.Constructrgba(0, 0, 255, 255);
  raa.color := c;

  ras.add_path(el, 0);

(*
  NEW(a);
  x, y, rx, ry, start_angle, sweep_angle: bas.double
  a.ConstructBArc(30, 30, 15, 18, 30, 90);

  ras.add_path(a, 1);
*)

  utl.WritePPM("tut6.ppm", rbuf);
END tut6.
