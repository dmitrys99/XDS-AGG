MODULE AggRasterizerOutline;

IMPORT

  bas := AggBasics,
  arp := AggRendererPrimitives,
  pfb := AggPixfmtBase,
  avs := AggVertexSource;

TYPE
  rasterizer_outline_ptr* = POINTER TO rasterizer_outline;
  rasterizer_outline* = RECORD(arp.renderer_primitives)
    start_x* : bas.int32;
    start_y* : bas.int32;
    vertices*: bas.int32;
  END;

PROCEDURE (ro: rasterizer_outline_ptr) Construct*(pf: pfb.pixel_formats_ptr);
BEGIN
  ro.Construct^(pf);
  ro.start_x  := 0;
  ro.start_y  := 0;
  ro.vertices := 0;
END Construct;

PROCEDURE (ro: rasterizer_outline_ptr) move_to*(x, y: bas.int32);
BEGIN
  ro.vertices := 1;

  ro.start_x := x;
  ro.start_y := y;

  ro.move_to^(x, y);
END move_to;

PROCEDURE (ro: rasterizer_outline_ptr) line_to*(x, y: bas.int32);
BEGIN
  INC(ro.vertices);
  ro.line_to^(x, y);
END line_to;

PROCEDURE (ro: rasterizer_outline_ptr) move_to_d*(x, y: bas.double);
BEGIN
  ro.move_to(ro.coord(x), ro.coord(y));
END move_to_d;

PROCEDURE (ro: rasterizer_outline_ptr) line_to_d*(x, y: bas.double);
BEGIN
  ro.line_to(ro.coord(x), ro.coord(y));
END line_to_d;

PROCEDURE (ro: rasterizer_outline_ptr) close*();
BEGIN
  IF ro.vertices > 2 THEN
    ro.line_to(ro.start_x, ro.start_y);
  END;

  ro.vertices:=0;
END close;

PROCEDURE (ro: rasterizer_outline_ptr) add_vertex*(x, y: bas.double; cmd: SET);
BEGIN
  IF bas.is_move_to(cmd) THEN
    ro.move_to_d(x, y)
  ELSE
    IF bas.is_end_poly(cmd ) THEN
      IF bas.is_closed(cmd) THEN
        ro.close
      END;
    ELSE
      ro.line_to_d(x, y);
    END;
  END;
END add_vertex;

PROCEDURE (ro: rasterizer_outline_ptr) add_path*(vs: avs.vertex_source_ptr; path_id: bas.int32);
VAR
  cmd: SET;
  x, y: bas.double;

BEGIN
  vs.rewind(path_id);

  cmd := vs.vertex(x, y);

  WHILE ~bas.is_stop(cmd) DO
    ro.add_vertex(x, y, cmd);
    cmd := vs.vertex(x, y);
  END;
END add_path;

END AggRasterizerOutline.
