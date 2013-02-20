MODULE AggRasterizerOutlineAA;

IMPORT
  bas := AggBasics,
  lab := AggLineAABasics,
  vsq := AggVertexSequence,
  roa := AggRendererOutlineAA,
  mth := MathL,
  avs := AggVertexSource,
  col := AggColor,
  ctl := AggCtrl;

TYPE
(**
  Vertex (x, y) with the distance to the next one. The last vertex has
  the distance between the last and the first points *)

  line_aa_vertex_ptr* = POINTER TO line_aa_vertex;
  line_aa_vertex* = RECORD(vsq.vertex_dist)
    ax, ay, len: bas.int32;
  END;

  draw_vars_ptr* = POINTER TO draw_vars;
  draw_vars* = RECORD
    idx: bas.int32;
    x1, y1, x2, y2: bas.int32;
    curr, next: lab.line_parameters_ptr;
    lcurr, lnext, xb1, yb1, xb2, yb2: bas.int32;
    flags: SET;
  END;


  rasterizer_outline_aa_ptr* = POINTER TO rasterizer_outline_aa;
  rasterizer_outline_aa* = RECORD
    ren: roa.renderer_outline_ptr;
    src_vertices: vsq.vertex_sequence_ptr;
    accurate_join*,
    round_cap    *: BOOLEAN;
    start_x,
    start_y: bas.int32;
  END;


PROCEDURE cmp_dist_start(d: bas.int32): BOOLEAN;
BEGIN
  RETURN d > 0;
END cmp_dist_start;

(* CMP_DIST_END *)
PROCEDURE cmp_dist_end(d: bas.int32): BOOLEAN;
BEGIN
  RETURN d <= 0;
END cmp_dist_end;



PROCEDURE (lav: line_aa_vertex_ptr) Construct*();
BEGIN
  lav.ax   := 0;
  lav.ay   := 0;
  lav.len  := 0;
END Construct;

PROCEDURE (lav: line_aa_vertex_ptr) Construct1*(x, y: bas.int32);
BEGIN
  lav.ax   := x;
  lav.ay   := y;
  lav.len  := 0;
END Construct1;


PROCEDURE line_aa_vertex_func_operator(this, val: vsq.vertex_dist_ptr): BOOLEAN;
VAR
  dx, dy: bas.double;

BEGIN
  dx := val(line_aa_vertex_ptr).ax - this(line_aa_vertex_ptr).ax;
  dy := val(line_aa_vertex_ptr).ay - this(line_aa_vertex_ptr).ay;

  this(line_aa_vertex_ptr).len := ENTIER(mth.sqrt(dx * dx + dy * dy));

  RETURN this(line_aa_vertex_ptr).len > (lab.line_subpixel_size + lab.line_subpixel_size DIV 2);
END line_aa_vertex_func_operator;






PROCEDURE (roaa: rasterizer_outline_aa_ptr) Construct*(ren: roa.renderer_outline_ptr);
BEGIN
  NEW(roaa.src_vertices);
  roaa.src_vertices.ConstructShiftFunc(6, line_aa_vertex_func_operator);

  roaa.ren := ren;

  roaa.accurate_join := roaa.ren.accurate_join_only();
  roaa.round_cap     := FALSE;

  roaa.start_x:=0;
  roaa.start_y:=0;
END Construct;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) Destruct*();
BEGIN
  roaa.src_vertices.Destruct();
  roaa.src_vertices := NIL;
END Destruct;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) draw(dv: draw_vars_ptr; start, end: bas.int32);
VAR
  i: bas.int32;
  v: line_aa_vertex_ptr;

BEGIN
  i := start;

  WHILE i < end DO

    IF dv.flags = {0,1} THEN roaa.ren.line0(dv.curr); END;
    IF dv.flags =   {1} THEN roaa.ren.line1(dv.curr, dv.xb1, dv.yb1); END;
    IF dv.flags =   {0} THEN roaa.ren.line2(dv.curr, dv.xb2, dv.yb2); END;
    IF dv.flags =    {} THEN roaa.ren.line3(dv.curr, dv.xb1, dv.yb1, dv.xb2, dv.yb2); END;


    dv.x1 := dv.x2;
    dv.y1 := dv.y2;

    dv.lcurr := dv.lnext;
    dv.lnext := roaa.src_vertices.array[dv.idx](line_aa_vertex_ptr).len;

    INC(dv.idx);

    IF dv.idx >= roaa.src_vertices.size THEN
      dv.idx := 0;
    END;

    v := roaa.src_vertices.array[dv.idx](line_aa_vertex_ptr);

    dv.x2 := v.ax;
    dv.y2 := v.ay;

    dv.curr^ := dv.next^;

    dv.next.Construct(dv.x1, dv.y1, dv.x2, dv.y2, dv.lnext);

    dv.xb1 := dv.xb2;
    dv.yb1 := dv.yb2;

    IF roaa.accurate_join THEN
      dv.flags := {}
    ELSE
      IF dv.flags * {1} = {1} THEN dv.flags := {0} ELSE dv.flags := {} END;

      IF dv.curr.diagonal_quadrant() = dv.next.diagonal_quadrant() THEN
        INCL(dv.flags, 1);
      END;
    END;

    IF dv.flags * {1} = {} THEN
      lab.bisectrix(dv.curr^, dv.next^, dv.xb2, dv.yb2);
    END;

    INC(i);
  END;
END draw;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) set_accurate_join*(v: BOOLEAN);
BEGIN
  IF roaa.ren.accurate_join_only() THEN
    roaa.accurate_join := TRUE
  ELSE
    roaa.accurate_join := v;
  END;
END set_accurate_join;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) move_to*(x, y: bas.int32);
VAR
  vt: line_aa_vertex_ptr;
BEGIN
 roaa.start_x := x;
 roaa.start_y := y;

 NEW(vt);
 vt.Construct1(x, y);

 roaa.src_vertices.modify_last(vt);
END move_to;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) line_to*(x, y: bas.int32);
VAR
  vt: line_aa_vertex_ptr;

BEGIN
  NEW(vt);
  vt.Construct1(x, y);

  roaa.src_vertices.add(vt);
END line_to;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) move_to_d*(x, y: bas.double);
BEGIN
  roaa.move_to(lab.line_coord(x), lab.line_coord(y));
END move_to_d;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) line_to_d*(x, y: bas.double);
BEGIN
  roaa.line_to(lab.line_coord(x), lab.line_coord(y));
END line_to_d;


PROCEDURE (roaa: rasterizer_outline_aa_ptr) render*(close_polygon: BOOLEAN);
VAR
  dv: draw_vars_ptr;
  v : line_aa_vertex_ptr;
  x1, y1, x2, y2, lprev, x3, y3, lnext: bas.int32;
  prev, lp, lp1, lp2: lab.line_parameters_ptr;
BEGIN
  NEW(dv);
  NEW(dv.curr);
  NEW(dv.next);

  NEW(prev);
  NEW(lp);
  NEW(lp1);
  NEW(lp2);

  roaa.src_vertices.close(close_polygon);

  IF close_polygon THEN
    IF roaa.src_vertices.size >= 3 THEN

      dv.idx:=2;

      v     := roaa.src_vertices.array[roaa.src_vertices.size - 1](line_aa_vertex_ptr);
      x1    := v.ax;
      y1    := v.ay;
      lprev := v.len;

      v  := roaa.src_vertices.array[0](line_aa_vertex_ptr);
      x2 := v.ax;
      y2 := v.ay;

      dv.lcurr := v.len;

      prev.Construct(x1, y1, x2, y2, lprev);

      v     := roaa.src_vertices.array[1](line_aa_vertex_ptr);
      dv.x1 := v.ax;
      dv.y1 := v.ay;

      dv.lnext := v.len;

      dv.curr.Construct(x2, y2, dv.x1, dv.y1, dv.lcurr);

      v     := roaa.src_vertices.array[dv.idx](line_aa_vertex_ptr);
      dv.x2 := v.ax;
      dv.y2 := v.ay;

      dv.next.Construct(dv.x1, dv.y1, dv.x2, dv.y2, dv.lnext);

      dv.xb1 := 0;
      dv.yb1 := 0;
      dv.xb2 := 0;
      dv.yb2 := 0;

      IF roaa.accurate_join THEN
        dv.flags := {}
      ELSE
        IF prev.diagonal_quadrant() = dv.curr.diagonal_quadrant() THEN dv.flags := {0} ELSE dv.flags := {}; END;
        IF dv.curr.diagonal_quadrant() = dv.next.diagonal_quadrant() THEN INCL(dv.flags, 1); END;
      END;

      IF dv.flags * {0} = {} THEN
        lab.bisectrix(prev^, dv.curr^, dv.xb1, dv.yb1);
      END;

      IF dv.flags * {1} = {} THEN
        lab.bisectrix(dv.curr^, dv.next^, dv.xb2, dv.yb2);
      END;
      roaa.draw(dv, 0, roaa.src_vertices.size);
    END;
  ELSE
    CASE roaa.src_vertices.size OF
     2:
       v     := roaa.src_vertices.array[0](line_aa_vertex_ptr);
       x1    := v.ax;
       y1    := v.ay;
       lprev := v.len;
       v     := roaa.src_vertices.array[1](line_aa_vertex_ptr);
       x2    := v.ax;
       y2    := v.ay;

       lp.Construct(x1, y1, x2, y2, lprev);

       IF roaa.round_cap THEN
         roaa.ren.semidot(cmp_dist_start, x1, y1, x1 + (y2 - y1), y1 - (x2 - x1));
       END;

       roaa.ren.line3(
        lp,
        x1 + (y2 - y1),
        y1 - (x2 - x1),
        x2 + (y2 - y1),
        y2 - (x2 - x1));

       IF roaa.round_cap THEN
         roaa.ren.semidot(cmp_dist_end, x2, y2, x2 + (y2 - y1), y2 - (x2 - x1));
       END;

    | 3:
      v   :=roaa.src_vertices.array[0](line_aa_vertex_ptr);
      x1  :=v.ax;
      y1  :=v.ay;
      lprev:=v.len;
      v   :=roaa.src_vertices.array[1](line_aa_vertex_ptr);
      x2  :=v.ax;
      y2  :=v.ay;
      lnext:=v.len;
      v   :=roaa.src_vertices.array[2](line_aa_vertex_ptr);
      x3  :=v.ax;
      y3  :=v.ay;

      lp1.Construct(x1, y1, x2, y2, lprev);
      lp2.Construct(x2, y2, x3, y3, lnext);

      lab.bisectrix(lp1^, lp2^, dv.xb1, dv.yb1);

      IF roaa.round_cap THEN
        roaa.ren.semidot(cmp_dist_start, x1, y1, x1 + (y2 - y1), y1 - (x2 - x1));
      END;

      roaa.ren.line3(
       lp1,
       x1 + (y2 - y1),
       y1 - (x2 - x1),
       dv.xb1,
       dv.yb1);

      roaa.ren.line3(
       lp2,
       dv.xb1,
       dv.yb1,
       x3 + (y3 - y2),
       y3 - (x3 - x2));

      IF roaa.round_cap THEN
        roaa.ren.semidot(cmp_dist_end, x3, y3, x3 + (y3 - y2), y3 - (x3 - x2));
      END;

    | 0: bas.NoP();
    | 1: bas.NoP();
    ELSE
      dv.idx := 3;

      v      := roaa.src_vertices.array[0](line_aa_vertex_ptr);
      x1     := v.ax;
      y1     := v.ay;
      lprev  := v.len;

      v  := roaa.src_vertices.array[1](line_aa_vertex_ptr);
      x2 := v.ax;
      y2 := v.ay;

      dv.lcurr := v.len;

      prev.Construct(x1, y1, x2, y2, lprev);

      v     := roaa.src_vertices.array[2](line_aa_vertex_ptr);
      dv.x1 := v.ax;
      dv.y1 := v.ay;

      dv.lnext := v.len;

      dv.curr.Construct(x2, y2, dv.x1, dv.y1, dv.lcurr);

      v     := roaa.src_vertices.array[dv.idx](line_aa_vertex_ptr);
      dv.x2 := v.ax;
      dv.y2 := v.ay;

      dv.next.Construct(dv.x1, dv.y1, dv.x2, dv.y2, dv.lnext);

      dv.xb1 := 0;
      dv.yb1 := 0;
      dv.xb2 := 0;
      dv.yb2 := 0;

      IF roaa.accurate_join THEN
       dv.flags := {}
      ELSE
        dv.flags := {};
        IF prev.diagonal_quadrant() = dv.curr.diagonal_quadrant()    THEN INCL(dv.flags, 0) END;
        IF dv.curr.diagonal_quadrant() = dv.next.diagonal_quadrant() THEN INCL(dv.flags, 1) END;
      END;

      IF dv.flags * {0} = {} THEN
        lab.bisectrix (prev^, dv.curr^, dv.xb1, dv.yb1);
        roaa.ren.line3(
         prev,
         x1 + (y2 - y1),
         y1 - (x2 - x1),
         dv.xb1,
         dv.yb1);
      ELSE
        roaa.ren.line1(prev, x1 + (y2 - y1), y1 - (x2 - x1));
      END;

      IF roaa.round_cap THEN
        roaa.ren.semidot(cmp_dist_start, x1, y1, x1 + (y2 - y1), y1 - (x2 - x1));
      END;

      IF dv.flags * {1} = {} THEN
        lab.bisectrix(dv.curr^, dv.next^, dv.xb2, dv.yb2);
      END;

      roaa.draw(dv, 1, roaa.src_vertices.size - 2);

      IF dv.flags * {0} = {} THEN
       roaa.ren.line3(
        dv.curr,
        dv.xb1,
        dv.yb1,
        dv.curr.x2 + (dv.curr.y2 - dv.curr.y1),
        dv.curr.y2 - (dv.curr.x2 - dv.curr.x1))
      ELSE
       roaa.ren.line2(
        dv.curr,
        dv.curr.x2 + (dv.curr.y2 - dv.curr.y1),
        dv.curr.y2 - (dv.curr.x2 - dv.curr.x1));
      END;

      IF roaa.round_cap THEN
       roaa.ren.semidot(
        cmp_dist_end, dv.curr.x2, dv.curr.y2,
        dv.curr.x2 + (dv.curr.y2 - dv.curr.y1),
        dv.curr.y2 - (dv.curr.x2 - dv.curr.x1));
      END;
    END;
  END;

  roaa.src_vertices.remove_all();
END render;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) add_vertex*(x, y: bas.double; cmd: SET);
BEGIN
  IF bas.is_move_to(cmd) THEN
    roaa.render(FALSE);
    roaa.move_to_d(x, y);
  ELSE
    IF bas.is_end_poly(cmd) THEN

       roaa.render(bas.is_closed(cmd));

       IF bas.is_closed(cmd) THEN
         roaa.move_to(roaa.start_x, roaa.start_y);
       END;
    ELSE
      roaa.line_to_d(x, y);
    END;
  END;
END add_vertex;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) add_path*(vs: avs.vertex_source_ptr; path_id: bas.int32);
VAR
  x, y: bas.double;
  cmd: SET;

BEGIN
  vs.rewind(path_id);
  cmd:=vs.vertex(x, y);

  WHILE ~bas.is_stop(cmd) DO
    roaa.add_vertex(x, y, cmd);
    cmd := vs.vertex(x, y);
  END;

  roaa.render(FALSE);
END add_path;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) render_all_paths*(vs: avs.vertex_source_ptr; colors: col.colors_array_ptr; path_id: bas.int_array_ptr; num_paths: bas.int32);
VAR
  i: bas.int32;
BEGIN
  FOR i := 0 TO num_paths - 1 DO
    roaa.ren.color := colors[i];
    roaa.add_path(vs, path_id[i]);
  END;
END render_all_paths;

PROCEDURE (roaa: rasterizer_outline_aa_ptr) render_ctrl*(c: ctl.ctrl_ptr);
VAR
  i: bas.int32;
BEGIN
  FOR i := 0 TO c.num_paths() - 1 DO
    roaa.ren.color := c.colors[i]^;
    roaa.add_path(c, i);
  END;
END render_ctrl;

END AggRasterizerOutlineAA.
PROCEDURE (roaa: rasterizer_outline_aa_ptr)