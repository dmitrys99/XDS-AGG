MODULE AggRasterizerScanlineAA;
IMPORT
(* DEBUG
  STextIO,
  SWholeIO,
  Out,
*)

  bas := AggBasics,
  ars := AggRasterizerScanline,
  arc := AggRasterizerCellsAA,
  clb := AggClipLiangBarsky,
  scl := AggScanline,
  bit := AggBit,
  vsc := AggVertexSource,
  SYSTEM;

CONST
  aa_shift * = 8;
  aa_num   * = ASH(1, aa_shift);
  aa_mask  * = aa_num - 1;
  aa_2num  * = aa_num * 2;
  aa_2mask * = aa_2num - 1;

  fill_non_zero* =  ars.fill_non_zero;
  fill_even_odd* =  ars.fill_even_odd;

  status_initial* = ars.status_initial;
  status_line_to* = ars.status_line_to;
  status_closed*  = ars.status_closed;

  poly_base_shift* = arc.poly_base_shift;
  poly_base_size * = arc.poly_base_size;
  poly_base_mask * = arc.poly_base_mask;

TYPE
  rasterizer_scanline_aa_ptr* = POINTER TO rasterizer_scanline_aa;
  rasterizer_scanline_aa* = RECORD(ars.rasterizer_scanline)
    outline: arc.rasterizer_cells_aa;
    gammas: ARRAY aa_num OF bas.int8u;

    clipped_start_x,
    clipped_start_y,

    start_x,
    start_y,
    prev_x,
    prev_y: bas.int32;

    prev_flags : SET;
    status     : bas.int32;

    clipbox: bas.rect;
    clipping: BOOLEAN;

    cur_y,
    XScale: bas.int32;
  END;

(* poly_coord *)

PROCEDURE poly_coord(c: bas.double): bas.int32;
BEGIN
  RETURN ENTIER(c * arc.poly_base_size);
END poly_coord;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) Construct*();
VAR
  i: bas.int16;
BEGIN
  rsa.outline.Construct();
  rsa.clipbox.ConstructI();

  rsa.filling_rule := fill_non_zero;

  rsa.clipped_start_x := 0;
  rsa.clipped_start_y := 0;

  rsa.start_x := 0;
  rsa.start_y := 0;
  rsa.prev_x  := 0;
  rsa.prev_y  := 0;

  rsa.prev_flags := {};
  rsa.status     := status_initial;
  rsa.clipping   := FALSE;

  rsa.XScale := 1;

  FOR i := 0 TO aa_num - 1 DO
    rsa.gammas[i] := bas.agg_getbyte(i);
  END;

END Construct;


(* reset *)

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) reset*();
BEGIN
  rsa.outline.reset();
  rsa.status := status_initial;
END reset;

(* clip_box *)

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) clip_box*(x1, y1, x2, y2: bas.double);
BEGIN
  rsa.reset();

  rsa.clipbox.x1 := poly_coord(x1);
  rsa.clipbox.y1 := poly_coord(y1);
  rsa.clipbox.x2 := poly_coord(x2);
  rsa.clipbox.y2 := poly_coord(y2);

  rsa.clipbox.Normalize();

  rsa.clipping := TRUE;
END clip_box;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) gamma*(gamma_function: vsc.vertex_source_ptr);
VAR
  i: bas.int32;
BEGIN
  FOR i := 0 TO aa_num - 1 DO
    rsa.gammas[i] := bas.agg_getbyte(ENTIER(gamma_function.func_operator_gamma(i / aa_mask) * aa_mask));
  END;
END gamma;

(* apply_gamma *)

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) apply_gamma*(cover: bas.int8u): bas.int8u;
BEGIN
  RETURN rsa.gammas[cover];
END apply_gamma;

(* close_polygon_no_clip *)

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) close_polygon_no_clip*();
BEGIN
  IF rsa.status = status_line_to THEN
    rsa.outline.line_to(rsa.clipped_start_x * rsa.XScale, rsa.clipped_start_y);
    rsa.status := status_closed;
  END;
END close_polygon_no_clip;

(* move_to_no_clip *)

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) move_to_no_clip*(x, y: bas.int32);
BEGIN
  IF rsa.status = status_line_to THEN
    rsa.close_polygon_no_clip();
  END;

  rsa.outline.move_to(x * rsa.XScale, y);

  rsa.clipped_start_x := x;
  rsa.clipped_start_y := y;

  rsa.status := status_line_to;
END move_to_no_clip;


(* line_to_no_clip *)
PROCEDURE (rsa: rasterizer_scanline_aa_ptr) line_to_no_clip*(x, y: bas.int32);
BEGIN
  IF rsa.status # status_initial THEN
    rsa.outline.line_to(x * rsa.XScale, y);
    rsa.status := status_line_to;
  END;
END line_to_no_clip;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) clip_segment*(x, y: bas.int32);
VAR
  flags: SET;
  n: bas.int32;

  cx, cy: bas.int_4;
  px, py: bas.int32;

BEGIN
  flags := clb.clipping_flags_int(x, y, rsa.clipbox);

  IF rsa.prev_flags = flags THEN
    IF flags = {} THEN
      IF rsa.status = status_initial THEN
        rsa.move_to_no_clip(x, y)
      ELSE
        rsa.line_to_no_clip(x, y)
      END;
    END;
  ELSE
    n := clb.clip_liang_barsky_int(rsa.prev_x, rsa.prev_y, x, y, rsa.clipbox, cx, cy);

    px := 0;
    py := 0;

    WHILE n > 0 DO
      IF rsa.status = status_initial THEN
        rsa.move_to_no_clip(cx[px], cy[py])
      ELSE
        rsa.line_to_no_clip(cx[px], cy[py])
      END;
      INC(px);
      INC(py);
      DEC(n);
    END;
  END;

  rsa.prev_flags := flags;

  rsa.prev_x := x;
  rsa.prev_y := y;
END clip_segment;

(* close_polygon *)

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) close_polygon*();
BEGIN
  IF rsa.clipping THEN
    rsa.clip_segment(rsa.start_x, rsa.start_y);
  END;

  rsa.close_polygon_no_clip();
END close_polygon;

(* line_to *)

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) line_to*(x, y: bas.int32);
BEGIN
  IF rsa.clipping THEN
    rsa.clip_segment(x, y)
  ELSE
    rsa.line_to_no_clip(x, y);
  END;
END line_to;

(* move_to *)

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) move_to*(x, y: bas.int32);
BEGIN
  IF rsa.clipping THEN
    IF rsa.outline.sorted THEN
      rsa.reset;
    END;

    IF rsa.status = status_line_to THEN
      rsa.close_polygon();
    END;

    rsa.prev_x  := x;
    rsa.start_x := x;
    rsa.prev_y  := y;
    rsa.start_y := y;
    rsa.status  := status_initial;

    rsa.prev_flags := clb.clipping_flags_int(x, y, rsa.clipbox);

    IF rsa.prev_flags = {} THEN
      rsa.move_to_no_clip(x, y);
    END
  ELSE
    rsa.move_to_no_clip(x, y);
  END;
END move_to;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) sort*();
BEGIN
  rsa.outline.sort_cells();
END sort;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) rewind_scanlines*(): BOOLEAN;
BEGIN
  rsa.close_polygon;
  rsa.outline.sort_cells();

  IF rsa.outline.total_cells() = 0 THEN
    RETURN FALSE;
  END;

  rsa.cur_y := rsa.outline.min_y;
  RETURN TRUE;
END rewind_scanlines;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) navigate_scanline*(y: bas.int32): BOOLEAN;
BEGIN
  rsa.close_polygon();
  rsa.outline.sort_cells();

  IF (rsa.outline.total_cells() = 0 ) OR (y < rsa.outline.min_y) OR (y > rsa.outline.max_y) THEN
    RETURN FALSE;
  END;

  rsa.cur_y := y;
  RETURN TRUE;
END navigate_scanline;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) min_x*(): bas.int32;
BEGIN
  RETURN rsa.outline.min_x;
END min_x;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) min_y*(): bas.int32;
BEGIN
  RETURN rsa.outline.min_y;
END min_y;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) max_x*(): bas.int32;
BEGIN
  RETURN rsa.outline.max_x;
END max_x;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) max_y*(): bas.int32;
BEGIN
  RETURN rsa.outline.max_y;
END max_y;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) calculate_alpha*(area: bas.int32): bas.int8u;
VAR
  cover: bas.int32;

BEGIN
  cover := bas.shr_int32(area, poly_base_shift * 2 + 1 - aa_shift);

  IF cover < 0 THEN
    cover := -cover;
  END;

  IF rsa.filling_rule = fill_even_odd THEN
    cover := bit.and32(cover, aa_2mask);

   IF cover > aa_num THEN
     cover := aa_2num - cover;
   END;
  END;

  IF cover > aa_mask THEN
    cover := aa_mask;
  END;

  RETURN rsa.gammas[cover];
END calculate_alpha;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) sweep_scanline*(sl: scl.scanline_ptr): BOOLEAN;
VAR
  x,
  area,
  cover: bas.int32;
  alpha: bas.int8u;
  c_num, c_start, c_idx, A, B: bas.int32;
BEGIN
  LOOP
(* DEBUG
    STextIO.WriteString("[rsa.cur_y = ");
    SWholeIO.WriteInt(rsa.cur_y, 3);
    STextIO.WriteString("]");
    STextIO.WriteLn();
*)
    IF rsa.cur_y > rsa.outline.max_y THEN
      RETURN FALSE;
    END;

    sl.reset_spans();

    c_num   := rsa.outline.scanline_num_cells  (rsa.cur_y);
    c_start := rsa.outline.scanline_start_cells(rsa.cur_y);

    cover := 0;
    c_idx := c_start;

    WHILE c_num > 0 DO

      A := rsa.outline.sorted_cells^[c_idx].A;
      B := rsa.outline.sorted_cells^[c_idx].B;

      x    := rsa.outline.cells^[A, B].x;
      area := rsa.outline.cells^[A, B].area;

      INC(cover, rsa.outline.cells^[A, B].cover);

      (* accumulate all cells with the same X *)
      DEC(c_num);

      LOOP
        IF (c_num <= 0) THEN EXIT END;

        INC(c_idx);

        A := rsa.outline.sorted_cells^[c_idx].A;
        B := rsa.outline.sorted_cells^[c_idx].B;

        IF (x # rsa.outline.cells^[A, B].x) THEN EXIT END;

        INC(area , rsa.outline.cells^[A, B].area);
        INC(cover, rsa.outline.cells^[A, B].cover);

        DEC(c_num);
      END;

      IF area # 0 THEN
        alpha := rsa.calculate_alpha(ASH(cover, (poly_base_shift + 1)) - area);

        IF alpha # 0 THEN
(* DEBUG
          STextIO.WriteString("sl.add_cell( x = ");
          SWholeIO.WriteInt(x, 3);
          STextIO.WriteString(", alpha = ");
          SWholeIO.WriteCard(alpha, 3);
          STextIO.WriteString(")");
          STextIO.WriteLn();
*)
          sl.add_cell(x, alpha);
        END;

        INC(x);
      END;

      IF (c_num # 0) & (rsa.outline.cells^[A, B].x > x) THEN
        alpha := rsa.calculate_alpha(ASH(cover, (poly_base_shift + 1)));

        IF alpha # 0 THEN
(* DEBUG
          STextIO.WriteString("sl.add_span( x = ");
          SWholeIO.WriteInt(x, 3);
          STextIO.WriteString(", len = ");
          SWholeIO.WriteCard(rsa.outline.cells^[A, B].x - x, 4);
          STextIO.WriteString(", alpha = ");
          SWholeIO.WriteCard(alpha, 3);
          STextIO.WriteString(")");
          STextIO.WriteLn();
*)
          sl.add_span(x, rsa.outline.cells^[A, B].x - x, alpha);
        END;
      END;
    END;

    IF sl.num_spans() > 0 THEN
      EXIT;
    END;

    INC(rsa.cur_y);

  END;
  sl.finalize(rsa.cur_y);

  INC(rsa.cur_y);

  RETURN TRUE;
END sweep_scanline;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) hit_test*(tx, ty: bas.int32): BOOLEAN;
VAR
  sl: arc.scanline_hit_test_ptr;
  d: BOOLEAN;
BEGIN
  IF ~rsa.navigate_scanline(ty) THEN
    RETURN FALSE;
  END;

  NEW(sl);
  sl.ConstructX(tx);

<*+WOFF903*>
  d := rsa.sweep_scanline(sl);
<*-WOFF903*>

  RETURN sl.hit;
END hit_test;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) add_vertex*(x, y: bas.double; cmd: SET);
BEGIN
  IF bas.is_close(cmd) THEN
    rsa.close_polygon
  ELSE
    IF bas.is_move_to(cmd) THEN
      rsa.move_to(poly_coord(x), poly_coord(y))
    ELSE
      IF bas.is_vertex(cmd) THEN
        rsa.line_to(poly_coord(x), poly_coord(y));
      END;
    END;
  END;
END add_vertex;

PROCEDURE (rsa: rasterizer_scanline_aa_ptr) add_path* (vs: vsc.vertex_source_ptr; path_id: bas.int32);
VAR
  cmd: SET;
  x, y: bas.double;
BEGIN
  vs.rewind(path_id);
(*  Out.String("path_id="); Out.Int(SYSTEM.VAL(bas.int32, path_id), 6); Out.Ln();*)
  cmd := vs.vertex(x, y);
(*
  Out.String('---------------------------------------------'); Out.Ln();
  Out.String('cmd='); Out.Int(SYSTEM.VAL(LONGINT, cmd), 8);
  Out.String(' x='); Out.Int(ENTIER(x*100), 5);
  Out.String(' y='); Out.Int(ENTIER(y*100), 5);
  Out.Ln();
*)
  WHILE ~bas.is_stop(cmd) DO
    rsa.add_vertex(x, y, cmd);
    cmd := vs.vertex(x, y);
(*
    Out.String('cmd='); Out.Int(SYSTEM.VAL(LONGINT, cmd), 8);
    Out.String(' x='); Out.Int(ENTIER(x*100), 5);
    Out.String(' y='); Out.Int(ENTIER(y*100), 5);
    Out.Ln();
*)
  END;
END add_path;

END AggRasterizerScanlineAA.
