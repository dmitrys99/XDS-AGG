<* +MAIN *>
<* +O2EXTENSIONS *>
MODULE %TEMPLATE%;

IMPORT
  bas := AggBasics,
  aps := AggPlatformSupport,
  col := AggColor,

  agc := AggCtrl,
  sli := AggSliderCtrl,
  cbx := AggCboxCtrl,


  reb := AggRendererBase,
  ars := AggRendererScanline,
  rsaa:= AggRasterizerScanlineAA,
  scl := AggScanlineP,
  rns := AggRenderScanlines,

  avs := AggVertexSource,
  age := AggEllipse,
  arr := AggRoundedRect,
  acs := AggConvStroke,

  agl := AggGammaLut,
  math:= MathL,

  pfg := AggPixfmtBgr24Gamma
(*, pfg1 := AggPixfmtBgr24 *)
;

CONST
  a_flip_y = TRUE;

TYPE
  the_application_ptr = POINTER TO the_application;
  the_application = RECORD(aps.platform_support)
    x, y: ARRAY 2 OF bas.double;

    dx, dy: bas.double;

    idx : bas.int32;

    radius,
    gamma ,
    offset: sli.slider_ctrl_ptr;

    white_on_black: cbx.cbox_ctrl_ptr;
  END;

VAR
  app: the_application_ptr;

PROCEDURE (VAR ta: the_application) Construct(format: bas.int32; flip_y: BOOLEAN);
VAR
  rgba8: col.aggclr;

BEGIN
  ta.Construct^(format, flip_y);

  NEW(ta.radius);
  NEW(ta.gamma);
  NEW(ta.offset);

  NEW(ta.white_on_black);


  ta.radius.ConstructCtrl(10, 10, 600 - 10, 19, ~flip_y);
  ta.gamma. ConstructCtrl(10, 10 + 20, 600 - 10, 19 + 20, ~flip_y);
  ta.offset.ConstructCtrl(10, 10 + 40, 600 - 10, 19 + 40, ~flip_y);

  (*ta.white_on_black.ConstructCBox(10, 10 + 60, 'White on black', FALSE); *)
  ta.white_on_black.ConstructCBox(10, 10 + 60, 'White on black', FALSE);

  ta.idx := -1;

  ta.x[0] :=100; ta.y[0] := 100;
  ta.x[1] :=500; ta.y[1] := 350;

  ta.gamma.label := 'gamma=%4.3f';
  ta.gamma.set_range(0.0, 3.0);
  ta.gamma.set_value(1.8);

  ta.radius.label := 'radius=%4.3f';
  ta.radius.set_range(0.0, 50.0);
  ta.radius.set_value(25.0);

  ta.offset.label := 'subpixel offset=%4.3f';
  ta.offset.set_range(-2.0, 3.0);

  rgba8.Constructrgba(127, 127, 127, 255);

  ta.white_on_black.set_text_color(rgba8);
  ta.white_on_black.set_inactive_color(rgba8);

  ta.add_ctrl(ta.radius);
  ta.add_ctrl(ta.gamma);
  ta.add_ctrl(ta.offset);
  ta.add_ctrl(ta.white_on_black);
END Construct;

PROCEDURE (VAR ta: the_application) Destruct();
BEGIN
  ta.radius := NIL;
  ta.gamma := NIL;
  ta.offset := NIL;
  ta.white_on_black := NIL;
END Destruct;

PROCEDURE (VAR ta: the_application) on_draw();
VAR
  pixf : pfg.pixel_format_bgr24_gamma_ptr;
  (*pixf : pfg1.pixel_format_bgr24_ptr;   *)

  rb  : reb.renderer_base_ptr;
  ren : ars.renderer_scanline_aa_solid_ptr;
  ras : rsaa.rasterizer_scanline_aa_ptr;
  sl  : scl.scanline_p8_ptr;

  rgba  : col.aggclr;
  gamma : agl.gamma_lut_ptr;
  gm_no : avs.vertex_source_ptr;

  e : age.ellipse_ptr;
  r : arr.rounded_rect_ptr;
  p : acs.conv_stroke_ptr;
  d : bas.double;

BEGIN
  (* Initialize structures *)
  NEW(gamma);
  gamma.Construct1(ta.gamma.get_value(), 8, 8);


  NEW(pixf);
  pixf.ConstructGamma(ta.rbuf_window, gamma);
  (*pixf.Construct(ta.rbuf_window);*)

  NEW(rb);
  rb.Construct(pixf);
  NEW(ren);
  ren.Construct(rb);

  IF ta.white_on_black.status THEN
    rgba.ConstructDbl(0, 0, 0, 1)
  ELSE
    rgba.ConstructDbl(1, 1, 1, 1);
  END;

  rb.clear(rgba);

  NEW(ras);
  ras.Construct;
  NEW(sl);
  sl.Construct;

  (*Render two "control" circles*)

  NEW(e);
  e.Construct;

  rgba.Constructrgba(127, 127, 127, 255);
  ren.color := rgba;

  e.init      (ta.x[0], ta.y[0], 3, 3, 16, TRUE);
  ras.add_path(e, 1);

  rns.render_scanlines(ras, sl, ren);

  e.init      (ta.x[1], ta.y[1], 3, 3, 16, TRUE);
  ras.add_path(e, 2);

  rns.render_scanlines(ras, sl, ren);


  (* Creating a rounded rectangle *)
  d := ta.offset.get_value();

  NEW(r);

  r.ConstructRR(ta.x[0] + d, ta.y[0] + d, ta.x[1] + d, ta.y[1] + d, ta.radius.get_value());
  (*r.ConstructRR(150,150,200,200,10);*)
  r.normalize_radius();

  (* Drawing as an outline *)
  NEW(p);
  p.ConstructVS(r);
  p.set_width(1);

  (*rgba.Constructrgba(190, 0, 206, 255);*)

  IF ~ta.white_on_black.status THEN
    rgba.ConstructDbl(0, 0, 0, 1)
  ELSE
    rgba.ConstructDbl(1, 1, 1, 1);
  END;

  ren.color := rgba;
  ras.add_path(p, 0);




  IF ta.white_on_black.status THEN
    rgba.ConstructDbl(1, 1, 1, 1)
  ELSE
    rgba.ConstructDbl(0, 0, 0, 1);
  END;

  rns.render_scanlines(ras, sl, ren);


  NEW(gm_no);
  gm_no.Construct;
  ras.gamma(gm_no);

  (* Render the controls *)
  agc.render_ctrl(ras, sl, ren, ta.radius);
  agc.render_ctrl(ras, sl, ren, ta.gamma);
  agc.render_ctrl(ras, sl, ren, ta.offset);
  agc.render_ctrl(ras, sl, ren, ta.white_on_black);

  (* Free AGG resources *)
(*  ras.Destruct;

  sl.Destruct;
  gamma.Destruct;
  p.Destruct();*)


END on_draw;

PROCEDURE (VAR ta: the_application) on_mouse_button_down(x, y: bas.int32; flags: SET);
BEGIN
  IF flags * aps.mouse_left # {} THEN
    IF math.sqrt((x - ta.x[0]) * (x - ta.x[0]) + (y - ta.y[0] ) * (y - ta.y[0])) < 5.0 THEN
      ta.dx := x - ta.x[0];
      ta.dy := y - ta.y[0];
      ta.idx := 0;
    ELSIF math.sqrt((x - ta.x[1]) * (x - ta.x[1]) + (y - ta.y[1] ) * (y - ta.y[1])) < 5.0 THEN
      ta.dx := x - ta.x[1];
      ta.dy := y - ta.y[1];
      ta.idx := 1;
    END;
  END;
END on_mouse_button_down;

<* +WOFF301 *>
PROCEDURE (VAR ta: the_application) on_mouse_button_up(x, y: bas.int32; flags: SET);
BEGIN
  ta.idx := -1;
END on_mouse_button_up;
<* -WOFF301 *>

PROCEDURE (VAR ta: the_application) on_mouse_move(x, y: bas.int32; flags: SET);
BEGIN
  IF flags * aps.mouse_left # {} THEN
    IF ta.idx >= 0 THEN
      ta.x[ta.idx] := x - ta.dx;
      ta.y[ta.idx] := y - ta.dy;
      ta.force_redraw;
    END;
  ELSE
    ta.on_mouse_button_up(x, y, flags);
  END;
END on_mouse_move;

<* +WOFF301 *>
PROCEDURE (VAR ta: the_application) on_key(x, y: bas.int32; key: bas.int32; flags: SET);
BEGIN
  IF key = aps.key_f1 THEN
    ta.message(
     'Yet another example dedicated to Gamma Correction. If you have a CRT monitor: ' + 0DX +

     0DX + 0DX + 'Note: F2 key saves current "screenshot" file in this demo"s directory.  ' );
   END;
END on_key;
<* -WOFF301 *>

BEGIN
  NEW(app);

  app.Construct(aps.pix_format_bgr24, a_flip_y);
  IF app.init(600, 400, aps.window_resize) THEN
    IF app.run() = 0 THEN END;
  END;

  app.Destruct;
END rounded_rect.
