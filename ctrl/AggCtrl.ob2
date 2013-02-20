<*+WOFF301*>
MODULE AggCtrl;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource,
  ata := AggTransAffine,
  col := AggColor,
  ars := AggRasterizerScanline,
  ags := AggScanline,
  rns := AggRenderScanlines,
  res := AggRendererScanline;

TYPE
  color_ptr_array_ptr* = POINTER TO ARRAY OF col.aggclr_ptr;

  ctrl_ptr* = POINTER TO ctrl;
  ctrl* = RECORD(avs.vertex_source)
    x1-, y1-, x2-, y2-: bas.double;
    flip_y: BOOLEAN;
    mtx-: ata.trans_affine_ptr;
    colors*: color_ptr_array_ptr;
  END;

PROCEDURE (c: ctrl_ptr) ConstructCtrl*(x1, y1, x2, y2: bas.double; flip_y: BOOLEAN);
BEGIN
  c.Construct();

  c.x1 := x1;
  c.y1 := y1;
  c.x2 := x2;
  c.y2 := y2;

  c.flip_y := flip_y;

  c.mtx := NIL;
  c.colors := NIL;
END ConstructCtrl;

PROCEDURE (c: ctrl_ptr) Destruct*();
BEGIN
  c.Destruct^();
END Destruct;

PROCEDURE (c: ctrl_ptr) in_rect*(x, y: bas.double): BOOLEAN;
BEGIN
  RETURN FALSE;
END in_rect;

PROCEDURE (c: ctrl_ptr) on_mouse_button_down*(x, y: bas.double): BOOLEAN;
BEGIN
  RETURN FALSE;
END on_mouse_button_down;

PROCEDURE (c: ctrl_ptr) on_mouse_button_up*  (x, y: bas.double): BOOLEAN;
BEGIN
  RETURN FALSE;
END on_mouse_button_up;

PROCEDURE (c: ctrl_ptr) on_mouse_move*(x, y: bas.double; button_flag: BOOLEAN): BOOLEAN;
BEGIN
  RETURN FALSE;
END on_mouse_move;

PROCEDURE (c: ctrl_ptr) on_arrow_keys*(left, right, down, up: BOOLEAN): BOOLEAN;
BEGIN
  RETURN FALSE;
END on_arrow_keys;

PROCEDURE (c: ctrl_ptr) transform*(mtx: ata.trans_affine_ptr);
BEGIN
  c.mtx := mtx;
END transform;

PROCEDURE (c: ctrl_ptr) no_transform*();
BEGIN
  c.mtx := NIL;
END no_transform;

PROCEDURE (c: ctrl_ptr) transform_xy*(VAR x, y: bas.double);
BEGIN
  IF c.flip_y THEN
    y := c.y1 + c.y2 - y;
  END;

  IF c.mtx # NIL THEN
    c.mtx.transform(c.mtx, x, y);
  END;
END transform_xy;

PROCEDURE (c: ctrl_ptr) inverse_transform_xy*(VAR x, y: bas.double);
BEGIN
  IF c.mtx # NIL THEN
    c.mtx.inverse_transform(c.mtx, x, y);
  END;

  IF c.flip_y THEN
    y := c.y1 + c.y2 - y;
  END;
END inverse_transform_xy;

PROCEDURE (c: ctrl_ptr) scale*(): bas.double;
BEGIN
  IF c.mtx # NIL THEN
    RETURN c.mtx.scale()
  ELSE
    RETURN 1.0;
  END;
END scale;

PROCEDURE render_ctrl*(ras: ars.rasterizer_scanline_ptr; sl: ags.scanline_ptr; r: res.renderer_scanline_ptr; c: ctrl_ptr);
VAR
  i: bas.int32;
BEGIN
  IF c.num_paths() > 0 THEN
    FOR i := 0 TO c.num_paths() - 1 DO
      ras.reset();
      ras.add_path(c, i);
      r.color := c.colors[i]^;
      rns.render_scanlines(ras, sl, r);
    END;
  END;
END render_ctrl;

END AggCtrl.
<*-WOFF301*>