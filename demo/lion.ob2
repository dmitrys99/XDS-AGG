<* +MAIN *>
<* +O2EXTENSIONS *>
MODULE lion;

IMPORT
  Out,
  ParseLion,

  bas := AggBasics,
  aps := AggPlatformSupport,
  col := AggColor,

  agc := AggCtrl,
  sli := AggSliderCtrl,

  reb := AggRendererBase,
  ars := AggRendererScanline,
  rsaa:= AggRasterizerScanlineAA,
  scl := AggScanlineP,
  rns := AggRenderScanlines,

  pfg1 := AggPixfmtBgr24,

  pts := AggPathStorage,
  abr := AggBoundingRect,
  ata := AggTransAffine,
  act := AggConvTransform,

  math:= MathL;

VAR
  g_rasterizer : rsaa.rasterizer_scanline_aa_ptr;
  g_scanline   : scl.scanline_p8_ptr;

  g_path     : pts.path_storage_ptr;
  g_colors   : ARRAY 100 OF col.aggclr;
  g_path_idx : ARRAY 100 OF bas.int32u;

  g_npaths : bas.int32;

  g_x1 ,g_y1, g_x2 ,g_y2,
  g_base_dx,  g_base_dy,
  g_angle,    g_scale,
  g_skew_x,   g_skew_y:   bas.double;

  g_nclick: bas.int32;

CONST
  a_flip_y = TRUE;

TYPE
  the_application_ptr = POINTER TO the_application;
  the_application = RECORD(aps.platform_support)
    alpha_slider: sli.slider_ctrl_ptr;
  END;

VAR
  app: the_application_ptr;

PROCEDURE (VAR ta: the_application) Construct(format: bas.int32; flip_y: BOOLEAN);
(*VAR
  rgba8: col.aggclr;*)

BEGIN
  ta.Construct^(format, flip_y);

  NEW(ta.alpha_slider);

  ta.alpha_slider.ConstructCtrl(5, 5, 512 - 5, 12, ~a_flip_y);

  ta.add_ctrl(ta.alpha_slider);

  ta.alpha_slider.no_transform();
  ta.alpha_slider.label := 'Alpha%3.3f';
  ta.alpha_slider.set_value(0.1);

  g_npaths := ParseLion.parse_lion(g_path, g_colors, g_path_idx);

  IF abr.bounding_rect(g_path, g_path_idx, 0, g_npaths, g_x1, g_y1, g_x2, g_y2) THEN END;

  g_base_dx := (g_x2 - g_x1 ) / 2.0;
  g_base_dy := (g_y2 - g_y1 ) / 2.0;


END Construct;

PROCEDURE (VAR ta: the_application) transform(width, height, x, y: bas.double);
BEGIN
 x := x - (width / 2);
 y := y - (height / 2);

 g_angle := math.arctan2(x, y);
 g_scale := math.sqrt   (y * y + x * x ) / 100.0;
END transform;

PROCEDURE (VAR ta: the_application) on_mouse_button_down(x, y: bas.int32; flags: SET);
VAR
  width, height: bas.int32;
BEGIN
  IF aps.mouse_left * flags # {} THEN
    width  := ta.rbuf_window.byte_width DIV 3;
    height := ta.rbuf_window.byte_height;

    ta.transform(width, height, x, y);
    ta.force_redraw();
  END;

  IF flags * aps.mouse_right # {} THEN
    g_skew_x := x;
    g_skew_y := y;
    ta.force_redraw();
  END;

END on_mouse_button_down;

PROCEDURE (VAR ta: the_application) on_mouse_move(x, y: bas.int32; flags: SET);
BEGIN
  ta.on_mouse_button_down(x, y, flags);
END on_mouse_move;

PROCEDURE (VAR ta: the_application) on_draw();
VAR
  pixf: pfg1.pixel_format_bgr24_ptr;
  rb:   reb.renderer_base_ptr;
  ren:  ars.renderer_scanline_aa_solid_ptr;

  i: bas.int32;

  width, height: bas.int32;

  mtx: ata.trans_affine_ptr;
  tat: ata.trans_affine_translation_ptr;
  tas: ata.trans_affine_scaling_ptr;
  tar: ata.trans_affine_rotation_ptr;
  taw: ata.trans_affine_skewing_ptr;

  trans: act.conv_transform_ptr;
BEGIN
  NEW(pixf);
  pixf.Construct(ta.rbuf_window);

  NEW(rb);
  rb.Construct(pixf);

  NEW(ren);
  ren.Construct(rb);

  width  := ta.rbuf_window.byte_width DIV 3;
  height := ta.rbuf_window.byte_height;

  FOR i := 0 TO g_npaths - 1 DO
    g_colors[i].a := bas.agg_getbyte(ENTIER(ta.alpha_slider.get_value() * 255));
  END;

  NEW(mtx);
  mtx.Construct();

  NEW(tat);
  tat.ConstructT(-g_base_dx, -g_base_dy);
  mtx.multiply (tat);

  NEW(tas);
  tas.ConstructS(g_scale, g_scale);
  mtx.multiply (tas);

  NEW(tar);
  tar.ConstructR(g_angle + bas.pi);
  mtx.multiply (tar);

  NEW(taw);
  taw.ConstructS(g_skew_x / 1000.0, g_skew_y / 1000.0);
  mtx.multiply (taw);

  tat.ConstructT(width / 2, height / 2);
  mtx.multiply (tat);

  (* This code renders the lion*)
  NEW(trans);
  trans.ConstructCT(g_path, mtx);

  rns.render_all_paths(g_rasterizer, g_scanline, ren, trans, g_colors, g_path_idx, g_npaths);
  agc.render_ctrl(g_rasterizer, g_scanline, ren, ta.alpha_slider);
END on_draw;

PROCEDURE (VAR ta: the_application) on_key(x, y: bas.int32; key: bas.int32; flags: SET);
BEGIN
  IF key = aps.key_f1 THEN
    ta.message(
      'This is the first example I used to implement and debug the scanline rasterizer, ' + 0DX +
      'affine transformer, and basic renderers. The image is drawn over the old one     ' + 0DX +
      'with a cetrain opacity value.' + 0DX + 0DX +
      'How to play with:' + 0DX + 0DX +
      'You can rotate and scale the "Lion" with the left mouse button. ' + 0DX +
      'Right mouse button adds "skewing" transformations, ' + 0DX +
      'proportional to the "X" coordinate. ' + 0DX +
      'Change "Alpha" to draw funny looking "lions". ' + 0DX +
      'Change window size to clear the window.' +
       0DX + 0DX + "Note: F2 key saves current 'screenshot' file in this demo's directory.  ");
  END;
END on_key;

BEGIN

  (* Rendering*)
  NEW(g_rasterizer);
  g_rasterizer.Construct();

  NEW(g_scanline);
  g_scanline.Construct();

  NEW(g_path);
  g_path.Construct;

  g_npaths := 0;

  g_x1 := 0;
  g_y1 := 0;
  g_x2 := 0;
  g_y2 := 0;

  g_base_dx := 0;
  g_base_dy := 0;

  g_angle := 0;
  g_scale := 1.0;

  g_skew_x := 0;
  g_skew_y := 0;
  g_nclick := 0;

  (* App *)
  NEW(app);

  app.Construct(aps.pix_format_bgr24, a_flip_y);
  app.setcaption('AGG Example. Lion (F1-Help)');
  IF app.init(512, 400, aps.window_resize) THEN
    IF app.run() = 0 THEN END;
  END;

  app.Destruct;

(* Free
 g_rasterizer.Destruct;
 g_scanline.Destruct;
 g_path.Destruct;*)

END lion.
