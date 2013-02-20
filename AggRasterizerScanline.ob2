MODULE AggRasterizerScanline;
IMPORT
  bas := AggBasics,
  scl := AggScanline,
  vrs := AggVertexSource;

CONST
  fill_non_zero* = 0;
  fill_even_odd* = 1;

  status_initial* = 0;
  status_line_to* = 1;
  status_closed*  = 2;

TYPE
  rasterizer_scanline_ptr* = POINTER TO rasterizer_scanline;
  rasterizer_scanline* = RECORD
    filling_rule*: bas.int32;
  END;

PROCEDURE (rs: rasterizer_scanline_ptr) reset*();
BEGIN
END reset;

PROCEDURE (rs: rasterizer_scanline_ptr) clip_box*(x1, y1, x2, y2: bas.double);
BEGIN

END clip_box;

PROCEDURE (rs: rasterizer_scanline_ptr) gamma*(gamma_function: vrs.vertex_source_ptr);
BEGIN

END gamma;

PROCEDURE (rs: rasterizer_scanline_ptr) add_path*(vs: vrs.vertex_source_ptr; path_id: bas.int32);
BEGIN
END add_path;

PROCEDURE (rs: rasterizer_scanline_ptr) add_vertex*(x, y: bas.double; cmd: SET);
BEGIN
END add_vertex;

PROCEDURE (rs: rasterizer_scanline_ptr) sort*();
BEGIN
END sort;

PROCEDURE (rs: rasterizer_scanline_ptr) rewind_scanlines*(): BOOLEAN;
BEGIN
  RETURN FALSE;
END rewind_scanlines;

PROCEDURE (rs: rasterizer_scanline_ptr) sweep_scanline*(sl: scl.scanline_ptr): BOOLEAN;
BEGIN
  RETURN sl.y = 0;
END sweep_scanline;

PROCEDURE (rs: rasterizer_scanline_ptr) sweep_scanline_em*(sl: scl.scanline_ptr): BOOLEAN;
BEGIN
  RETURN sl.y = 0;
END sweep_scanline_em;

PROCEDURE (rs: rasterizer_scanline_ptr) hit_test(tx, ty: bas.int32): BOOLEAN;
BEGIN
  RETURN tx = ty;
END hit_test;

PROCEDURE (rs: rasterizer_scanline_ptr) min_x*(): bas.int32;
BEGIN
  RETURN 0;
END min_x;

PROCEDURE (rs: rasterizer_scanline_ptr) min_y*(): bas.int32;
BEGIN
  RETURN 0;
END min_y;

PROCEDURE (rs: rasterizer_scanline_ptr) max_x*(): bas.int32;
BEGIN
  RETURN 0;
END max_x;

PROCEDURE (rs: rasterizer_scanline_ptr) max_y*(): bas.int32;
BEGIN
  RETURN 0;
END max_y;

END AggRasterizerScanline.
