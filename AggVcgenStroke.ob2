MODULE AggVcgenStroke;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource,
  vsq := AggVertexSequence,
  ams := AggMathStroke,
  asp := AggShortenPath,
  mat := MathL;

CONST
  initial*      =  0;
  ready*        =  1;
  cap1*         =  2;
  cap2*         =  3;
  outline1*     =  4;
  close_first*  =  5;
  outline2*     =  6;
  out_vertices* =  7;
  end_poly1*    =  8;
  end_poly2*    =  9;
  stop*         = 10;

TYPE
  vcgen_stroke_ptr* = POINTER TO vcgen_stroke;
  vcgen_stroke* = RECORD(avs.vertex_source)
    src_vertices      : vsq.vertex_sequence_ptr;
    out_vertices      : vsq.vertex_sequence_ptr;

    awidth            ,
    amiter_limit      ,
    ainner_miter_limit,
    approx_scale      ,
    ashorten          : bas.double;

    aline_cap*        ,
    aline_join*       ,
    ainner_join*      : bas.int32;

    closed            : BOOLEAN;

    status            ,
    prev_status       : bas.int32;

    src_vertex        ,
    out_vertex        : bas.int32;
  END;

PROCEDURE (vs: vcgen_stroke_ptr) width*(): bas.double;
BEGIN
  RETURN vs.awidth * 2.0;
END width;

PROCEDURE (vs: vcgen_stroke_ptr) set_width*(width: bas.double);
BEGIN
  vs.awidth := width * 0.5;
END set_width;

PROCEDURE (vs: vcgen_stroke_ptr) miter_limit*(): bas.double;
BEGIN
  RETURN vs.amiter_limit;
END miter_limit;

PROCEDURE (vs: vcgen_stroke_ptr) set_miter_limit*(miter_limit: bas.double);
BEGIN
  vs.amiter_limit := miter_limit;
END set_miter_limit;

PROCEDURE (vs: vcgen_stroke_ptr) inner_miter_limit*(): bas.double;
BEGIN
  RETURN vs.ainner_miter_limit;
END inner_miter_limit;

PROCEDURE (vs: vcgen_stroke_ptr) set_inner_miter_limit*(inner_miter_limit: bas.double);
BEGIN
  vs.ainner_miter_limit := inner_miter_limit;
END set_inner_miter_limit;

PROCEDURE (vs: vcgen_stroke_ptr) approximation_scale*(): bas.double;
BEGIN
  RETURN vs.approx_scale;
END approximation_scale;

PROCEDURE (vs: vcgen_stroke_ptr) set_approximation_scale*(approximation_scale: bas.double);
BEGIN
  vs.approx_scale := approximation_scale;
END set_approximation_scale;

PROCEDURE (vs: vcgen_stroke_ptr) shorten*(): bas.double;
BEGIN
  RETURN vs.ashorten;
END shorten;

PROCEDURE (vs: vcgen_stroke_ptr) set_shorten*(shorten: bas.double);
BEGIN
  vs.ashorten := shorten;
END set_shorten;

PROCEDURE (vs: vcgen_stroke_ptr) line_cap*(): bas.int32;
BEGIN
  RETURN vs.aline_cap;
END line_cap;

PROCEDURE (vs: vcgen_stroke_ptr) set_line_cap*(line_cap: bas.int32);
BEGIN
  vs.aline_cap := line_cap;
END set_line_cap;

PROCEDURE (vs: vcgen_stroke_ptr) line_join*(): bas.int32;
BEGIN
  RETURN vs.aline_join;
END line_join;

PROCEDURE (vs: vcgen_stroke_ptr) set_line_join*(line_join: bas.int32);
BEGIN
  vs.aline_join := line_join;
END set_line_join;

PROCEDURE (vs: vcgen_stroke_ptr) inner_join*(): bas.int32;
BEGIN
  RETURN vs.ainner_join;
END inner_join;

PROCEDURE (vs: vcgen_stroke_ptr) set_inner_join*(inner_join: bas.int32);
BEGIN
  vs.ainner_join := inner_join;
END set_inner_join;


PROCEDURE (vs: vcgen_stroke_ptr) Construct*();
BEGIN
  NEW(vs.src_vertices);
  vs.src_vertices.Construct();

  NEW(vs.out_vertices);
  vs.out_vertices.Construct();

  vs.set_width(0.5);
  vs.amiter_limit      := 4.0;
  vs.ainner_miter_limit:= 1.01;
  vs.approx_scale      := 1.0;
  vs.ashorten          := 0.0;
  vs.aline_cap         := ams.butt_cap;
  vs.aline_join        := ams.miter_join;
  vs.ainner_join       := ams.inner_miter;
  vs.closed            := FALSE;
  vs.status            := initial;
  vs.src_vertex        := 0;
  vs.out_vertex        := 0;
END Construct;

PROCEDURE (vs: vcgen_stroke_ptr) Destruct*();
BEGIN
  vs.src_vertices.Destruct();
  vs.out_vertices.Destruct();
END Destruct;

PROCEDURE (vs: vcgen_stroke_ptr) set_miter_limit_theta*(t: bas.double);
BEGIN
  vs.amiter_limit := 1.0 / mat.sin(t * 0.5);
END set_miter_limit_theta;

PROCEDURE (vs: vcgen_stroke_ptr) remove_all*();
BEGIN
  vs.src_vertices.remove_all();
  vs.closed := FALSE;
  vs.status := initial;
END remove_all;

PROCEDURE (vs: vcgen_stroke_ptr) add_vertex*(x, y: bas.double; cmd: SET);
VAR
  vd: vsq.vertex_dist_ptr;
BEGIN
  vs.status := initial;

  NEW(vd);

  vd.x := x;
  vd.y := y;

  vd.dist := 0;

  IF bas.is_move_to(cmd) THEN
    vs.src_vertices.modify_last(vd)
  ELSE
    IF bas.is_vertex(cmd) THEN
      vs.src_vertices.addfunc(vd)
    ELSE
      vs.closed := bas.get_close_flag(cmd);
    END;
  END;
END add_vertex;


<*+WOFF301*>
PROCEDURE (vs: vcgen_stroke_ptr) rewind*(path_id: bas.int32u);
BEGIN
  IF vs.status = initial THEN
    vs.src_vertices.close(vs.closed);

    asp.shorten_path(vs.src_vertices, vs.shorten(), vs.closed);

    IF vs.src_vertices.size < 3 THEN
      vs.closed := FALSE;
    END;
  END;

  vs.status := ready;
  vs.src_vertex := 0;
  vs.out_vertex := 0;
END rewind;
<*-WOFF301*>

PROCEDURE (vs: vcgen_stroke_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  (*c: bas.point_type_ptr;*)
  c: vsq.vertex_dist_ptr;
  t: bas.int32;
  cmd: SET;
  prv,cur,nxt: bas.array_item_ptr;
BEGIN
  cmd := bas.path_cmd_line_to;

  WHILE ~bas.is_stop(cmd) DO
    IF (vs.status = initial) OR (vs.status = ready) THEN
      IF vs.status = initial THEN vs.rewind(0); END;
      IF vs.closed THEN t := 1 ELSE t := 0 END;
      IF vs.src_vertices.size < (2 + t) THEN
        cmd := bas.path_cmd_stop;
      ELSE
        IF (vs.closed) THEN
          vs.status := outline1
        ELSE
          vs.status := cap1;
        END;

        cmd := bas.path_cmd_move_to;

        vs.src_vertex := 0;
        vs.out_vertex := 0;
      END;
    ELSIF vs.status = cap1 THEN
      ams.stroke_calc_cap(vs.out_vertices,
                          vs.src_vertices.array[0](vsq.vertex_dist_ptr),
                          vs.src_vertices.array[1](vsq.vertex_dist_ptr),
                          vs.src_vertices.array[0](vsq.vertex_dist_ptr).dist,
                          vs.line_cap(),
                          vs.awidth,
                          vs.approximation_scale());

      vs.src_vertex  := 1;
      vs.prev_status := outline1;
      vs.status      := out_vertices;
      vs.out_vertex  := 0;
    ELSIF vs.status = cap2 THEN
      ams.stroke_calc_cap(vs.out_vertices,
                          vs.src_vertices.array[vs.src_vertices.size - 1](vsq.vertex_dist_ptr),
                          vs.src_vertices.array[vs.src_vertices.size - 2](vsq.vertex_dist_ptr),
                          vs.src_vertices.array[vs.src_vertices.size - 2](vsq.vertex_dist_ptr).dist,
                          vs.line_cap(),
                          vs.awidth,
                          vs.approximation_scale());

      vs.prev_status := outline2;
      vs.status      := out_vertices;
      vs.out_vertex  := 0;
    ELSIF vs.status = outline1 THEN


      IF vs.closed & (vs.src_vertex >= vs.src_vertices.size) THEN
        vs.prev_status := close_first;
        vs.status      := end_poly1;
      ELSIF ~(vs.closed) & (vs.src_vertex >= vs.src_vertices.size - 1) THEN
        vs.status:=cap2;
      ELSE
        prv := vs.src_vertices.prev(vs.src_vertex);
        cur := vs.src_vertices.curr(vs.src_vertex);
        nxt := vs.src_vertices.next(vs.src_vertex);

        ams.stroke_calc_join(vs.out_vertices,
                             prv(vsq.vertex_dist_ptr),
                             cur(vsq.vertex_dist_ptr),
                             nxt(vsq.vertex_dist_ptr),
                             prv(vsq.vertex_dist_ptr).dist,
                             cur(vsq.vertex_dist_ptr).dist, (*vertex_dist_ptr*)
                             vs.awidth,
                             vs.line_join(),
                             vs.inner_join(),
                             vs.miter_limit(),
                             vs.inner_miter_limit(),
                             vs.approximation_scale());

        INC(vs.src_vertex);

        vs.prev_status := vs.status;
        vs.status      := out_vertices;
        vs.out_vertex  := 0;
      END;
    ELSIF (vs.status = close_first) OR (vs.status = outline2) THEN
      IF (vs.status = close_first) THEN
        vs.status:=outline2;
        cmd := bas.path_cmd_move_to;
      END;
      IF ~vs.closed THEN t := 1 ELSE t := 0; END;
      IF vs.src_vertex <= t THEN
        vs.status      := end_poly2;
        vs.prev_status := stop;
      ELSE
        DEC(vs.src_vertex);

        prv := vs.src_vertices.prev(vs.src_vertex);
        cur := vs.src_vertices.curr(vs.src_vertex);
        nxt := vs.src_vertices.next(vs.src_vertex);

        ams.stroke_calc_join(vs.out_vertices ,
                             nxt(vsq.vertex_dist_ptr),
                             cur(vsq.vertex_dist_ptr),
                             prv(vsq.vertex_dist_ptr),
                             cur(vsq.vertex_dist_ptr).dist,
                             prv(vsq.vertex_dist_ptr).dist,
                             vs.awidth,
                             vs.line_join(),
                             vs.inner_join(),
                             vs.miter_limit(),
                             vs.inner_miter_limit(),
                             vs.approximation_scale());

        vs.prev_status := vs.status;
        vs.status      := out_vertices;
        vs.out_vertex  := 0;
      END;
    ELSIF vs.status = out_vertices THEN
      IF vs.out_vertex >= vs.out_vertices.size THEN
        vs.status := vs.prev_status
      ELSE
        (*c := vs.out_vertices.array[vs.out_vertex](bas.point_type_ptr);*)
        c := vs.out_vertices.array[vs.out_vertex](vsq.vertex_dist_ptr);
        INC(vs.out_vertex);

        x := c.x;
        y := c.y;

        RETURN cmd;
      END;
    ELSIF vs.status = end_poly1 THEN
      vs.status := vs.prev_status;
      RETURN bas.path_cmd_end_poly + bas.path_flags_close + bas.path_flags_ccw;
    ELSIF vs.status = end_poly2 THEN
      vs.status := vs.prev_status;
      RETURN bas.path_cmd_end_poly + bas.path_flags_close + bas.path_flags_cw;
    ELSIF vs.status = stop THEN
      cmd := bas.path_cmd_stop;
    END;
  END;

  RETURN cmd;
END vertex;

END AggVcgenStroke.
