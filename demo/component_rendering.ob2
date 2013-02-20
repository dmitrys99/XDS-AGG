<* +MAIN *>
<* +O2EXTENSIONS *>
MODULE component_rendering;

IMPORT
  Out,

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

  age := AggEllipse,

  pf1 := AggPixfmtBgr24,
  pf2 := AggPixfmtGray;

CONST
  a_flip_y = TRUE;

TYPE
  the_application_ptr = POINTER TO the_application;
  the_application = RECORD(aps.platform_support)
    alpha: sli.slider_ctrl_ptr;
  END;

VAR
  app: the_application_ptr;

PROCEDURE (VAR ta: the_application) Construct(format: bas.int32; flip_y: BOOLEAN);
BEGIN
  ta.Construct^(format, flip_y);

  NEW(ta.alpha);

  ta.alpha.ConstructCtrl(5, 5, 320 - 5, 10 + 5, ~a_flip_y );

  ta.alpha.label := 'Alpha=%1.0f';
  ta.alpha.set_range(0, 255);
  ta.alpha.set_value(255);

  ta.add_ctrl(ta.alpha);
END Construct;

PROCEDURE (VAR ta: the_application) Destruct();
BEGIN
  ta.alpha := NIL;
END Destruct;

PROCEDURE (VAR ta: the_application) on_draw();
VAR
 pf  : pf1.pixel_format_bgr24_ptr;
 pfr : pf2.pixel_format_gray24r_ptr;
 pfg : pf2.pixel_format_gray24g_ptr;
 pfb : pf2.pixel_format_gray24b_ptr;

 rbase ,
 rbr ,
 rbg ,
 rbb : reb.renderer_base_ptr;

 r  ,
 rr ,
 rg ,
 rb : ars.renderer_scanline_aa_solid_ptr;

 ras : rsaa.rasterizer_scanline_aa_ptr;
 sl  : scl.scanline_p8_ptr;

 rgba ,
 gray : col.aggclr;

 er ,
 eg ,
 eb : age.ellipse_ptr;

BEGIN
 NEW(pf);  pf. Construct(ta.rbuf_window);
 NEW(pfr); pfr.Construct(ta.rbuf_window);
 NEW(pfg); pfg.Construct(ta.rbuf_window);
 NEW(pfb); pfb.Construct(ta.rbuf_window);

 NEW(rbase); rbase.Construct(pf);
 NEW(rbr);   rbr.Construct(pfr);
 NEW(rbg);   rbg.Construct(pfg);
 NEW(rbb);   rbb.Construct(pfb);

 NEW(r);  r. Construct(rbase);
 NEW(rr); rr.Construct(rbr);
 NEW(rg); rg.Construct(rbg);
 NEW(rb); rb.Construct(rbb);

 NEW(ras); ras.Construct;
 NEW(sl); sl.Construct;

(* Setup colors & background *)

 rgba.ConstructDbl(1, 1, 1, 1);
 gray.ConstructV(0, bas.agg_getbyte(ENTIER(ta.alpha.get_value())));

 rbase.clear(rgba);

(* Draw ellipses *)
 NEW(er); er.ConstructEl((ta.rbuf_window.byte_width DIV 3) / 2 - 0.87 * 50, ta.rbuf_window.byte_height / 2 - 0.5 * 50, 100, 100, 100, FALSE);
 rr.color := gray;
 ras.add_path    (er, 0);
 rns.render_scanlines(ras, sl, rr);

 NEW(eg); eg.ConstructEl((ta.rbuf_window.byte_width DIV 3) / 2 + 0.87 * 50, (ta.rbuf_window.byte_height) / 2 - 0.5 * 50, 100, 100, 100, FALSE);
 rg.color := gray;
 ras.add_path    (eg, 0);
 rns.render_scanlines(ras, sl, rg);

 NEW(eb); eb.ConstructEl((ta.rbuf_window.byte_width DIV 3) / 2 ,(ta.rbuf_window.byte_height) / 2 + 50, 100, 100, 100, FALSE);
 rb.color := gray;
 ras.add_path    (eb, 0);
 rns.render_scanlines(ras, sl, rb);

(* Render control *)
  agc.render_ctrl(ras, sl, r, ta.alpha);
END on_draw;

<* +WOFF301 *>
PROCEDURE (VAR ta: the_application) on_key(x, y: bas.int32; key: bas.int32; flags: SET);
BEGIN
  IF key = aps.key_f1 THEN
    ta.message(
   'AGG has a gray-scale renderer that can use any 8-bit color channel ' + 0DX +
   'of an RGB or RGBA frame buffer. Most likely it will be used to draw '+ 0DX +
   'gray-scale images directly in the alpha-channel.' +
    0DX+0DX+'Note: F2 key saves current "screenshot" file in this demo"s directory.');
   END;
END on_key;
<* -WOFF301 *>

BEGIN
  NEW(app);

  app.Construct(aps.pix_format_bgr24, a_flip_y);
  IF app.init(320, 320, {}) THEN
    IF app.run() = 0 THEN END;
  END;

  app.Destruct;
END component_rendering.
