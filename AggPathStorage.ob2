MODULE AggPathStorage;

IMPORT
  bas  := AggBasics,
  math := AggMath,
  aba  := AggBezierArc,
  avs  := AggVertexSource,
  bit  := AggBit;


CONST
  list_shift* = 8;
  list_size*  = ASH(1, list_shift);
  list_mask*  = list_size - 1;
  list_pool*  = 256;

TYPE
  vertex_item = RECORD
    x, y: bas.double;
    cmd: SET;
  END;

  vi_array_ptr = POINTER TO ARRAY OF vertex_item;
  vi_array_list_ptr = POINTER TO ARRAY OF vi_array_ptr;

  path_storage_ptr* = POINTER TO path_storage;

  ps_vertex_source_ptr* = POINTER TO ps_vertex_source;
  ps_vertex_source* = RECORD(avs.vertex_source)
    path: path_storage_ptr;
    vertex_idx: bas.int32;
  END;


(**
------------------------------------------------------------path_storage
  A container to store vertices with their flags.
  A path consists of a number of contours separated with "move_to"
  commands. The path storage can keep and maintain more than one
  path.
  To navigate to the beginning of a particular path, use rewind(path_id);
  Where path_id is what start_new_path() returns. So, when you call
  start_new_path() you need to store its return value somewhere else
  to navigate to the path afterwards. *)
  path_storage* = RECORD(avs.vertex_source)
    total_vertices-,
    total_lists,
    max_lists: bas.int32;
    items: vi_array_list_ptr;
    iterator: bas.int32;
  END;

PROCEDURE (pvs: ps_vertex_source_ptr) Construct*();
BEGIN
  pvs.path := NIL;
  pvs.vertex_idx := 0;
END Construct;


PROCEDURE (pvs: ps_vertex_source_ptr) Construct1*(p: path_storage_ptr);
BEGIN
  pvs.path := p;
  pvs.vertex_idx := 0;
END Construct1;

PROCEDURE (pvs: ps_vertex_source_ptr) rewind*(path_id: bas.int32u);
BEGIN
  pvs.vertex_idx := path_id;
END rewind;

(*=================================================================*)
PROCEDURE (ps: path_storage_ptr) get_vertex(idx: bas.int32; VAR x, y: bas.double): SET;
VAR
  list, item: bas.int32;
BEGIN
  list := bas.shr_int32(idx, list_shift);
  item := bit.and32    (idx, list_mask );

  x := ps.items^[list][item].x;
  y := ps.items^[list][item].y;
  RETURN ps.items^[list][item].cmd;
END get_vertex;
(*=================================================================*)

PROCEDURE (pvs: ps_vertex_source_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  result: SET;
BEGIN
  IF pvs.vertex_idx < pvs.path.total_vertices THEN
    result := pvs.path.get_vertex(pvs.vertex_idx, x, y);
    INC(pvs.vertex_idx);
  ELSE
    result := bas.path_cmd_stop;
  END;
  RETURN result;
END vertex;

PROCEDURE (ps: path_storage_ptr) Construct*();
BEGIN
  ps.total_vertices := 0;
  ps.total_lists    := 0;
  ps.max_lists      := 0;
  ps.items          := NIL;
  ps.iterator       := 0;
END Construct;

PROCEDURE (ps: path_storage_ptr) allocate_list(nl: bas.int32);
VAR
  new_items: vi_array_list_ptr;
  i: bas.int32;
BEGIN
  IF nl >= ps.max_lists THEN
    NEW(new_items, ps.max_lists + list_pool);
    IF ps.items # NIL THEN
      FOR i := 0 TO LEN(ps.items^)-1 DO
        new_items[i] := ps.items^[i];
      END;
    END;
    ps.items := new_items;
    INC(ps.max_lists, list_pool);
  END;
  NEW(ps.items^[nl], list_size);
  INC(ps.total_lists);
END allocate_list;

PROCEDURE (ps: path_storage_ptr) command*(idx: bas.int32): SET;
VAR
  list, item: bas.int32;
BEGIN
  list := bas.shr_int32(idx, list_shift);
  item := bit.and32    (idx, list_mask );
  RETURN ps.items^[list][item].cmd;
END command;

PROCEDURE (ps: path_storage_ptr) next_item(VAR list, item: bas.int32);
VAR
  nl: bas.int32;
BEGIN
  nl := bas.shr_int32(ps.total_vertices, list_shift);

  IF nl >= ps.total_lists THEN
    ps.allocate_list(nl);
  END;

  list := nl;
  item := bit.and32(ps.total_vertices, list_mask);
END next_item;

(*
This function adds a vertex with its flags directly. Since there's no
checking for errors, keeping proper path integrity is the responsibility
of the caller. It can be said the function is "not very public".
*)
PROCEDURE (ps: path_storage_ptr) add_vertex*(x, y: bas.double; cmd: SET);
VAR
  list, item: bas.int32;
BEGIN
  ps.next_item(list, item);
  ps.items^[list][item].x := x;
  ps.items^[list][item].y := y;
  ps.items^[list][item].cmd := cmd;

  INC(ps.total_vertices);
END add_vertex;

PROCEDURE (ps: path_storage_ptr) remove_all*();
BEGIN
  ps.total_vertices := 0;
  ps.iterator       := 0;
END remove_all;

PROCEDURE (ps: path_storage_ptr) copy_from*(aps: path_storage_ptr);
VAR
  i: bas.int32;
  cmd: SET;
  x, y: bas.double;
BEGIN
  ps.remove_all();

  FOR i := 0 TO aps.total_vertices - 1 DO
    cmd := aps.get_vertex(i, x, y);
    ps.add_vertex(x, y, cmd);
  END;
END copy_from;

PROCEDURE (ps: path_storage_ptr) Construct1*(aps: path_storage_ptr);
BEGIN
  ps.total_vertices := 0;
  ps.total_lists    := 0;
  ps.max_lists      := 0;
  ps.items          := NIL;
  ps.iterator       := 0;

  ps.copy_from(aps);
END Construct1;

PROCEDURE (ps: path_storage_ptr) Destruct*();
BEGIN
  ps.total_lists := 0;
  ps.items := NIL;
END Destruct;

PROCEDURE (ps: path_storage_ptr) last_vertex*(VAR x, y: bas.double): SET;
BEGIN
  IF ps.total_vertices # 0 THEN
    RETURN ps.get_vertex(ps.total_vertices - 1, x, y)
  ELSE
    RETURN bas.path_cmd_stop;
  END;
END last_vertex;

PROCEDURE (ps: path_storage_ptr) prev_vertex*(VAR x, y: bas.double): SET;
BEGIN
  IF ps.total_vertices > 1 THEN
    RETURN ps.get_vertex(ps.total_vertices - 2, x, y)
  ELSE
    RETURN bas.path_cmd_stop;
  END;
END prev_vertex;

PROCEDURE (ps: path_storage_ptr) last_x*(): bas.double;
VAR
  idx, list, item: bas.int32;
BEGIN
  IF ps.total_vertices # 0 THEN
    idx := ps.total_vertices - 1;
    list := bas.shr_int32(idx, list_shift);
    item := bit.and32    (idx, list_mask );
    RETURN ps.items^[list][item].x;
  ELSE
    RETURN 0.0;
  END;
END last_x;

PROCEDURE (ps: path_storage_ptr) last_y*(): bas.double;
VAR
  idx, list, item: bas.int32;
BEGIN
  IF ps.total_vertices # 0 THEN
    idx := ps.total_vertices - 1;
    list := bas.shr_int32(idx, list_shift);
    item := bit.and32    (idx, list_mask );
    RETURN ps.items^[list][item].y;
  ELSE
    RETURN 0.0;
  END;
END last_y;

PROCEDURE (ps: path_storage_ptr) rel_to_abs*(VAR x, y: bas.double);
VAR
  x2, y2: bas.double;
BEGIN
  IF ps.total_vertices # 0 THEN
    IF bas.is_vertex(ps.get_vertex(ps.total_vertices - 1, x2, y2)) THEN
      x := x + x2;
      y := y + y2;
    END;
  END;
END rel_to_abs;

PROCEDURE (ps: path_storage_ptr) move_to*(x, y: bas.double);
BEGIN
  ps.add_vertex(x, y, bas.path_cmd_move_to);
END move_to ;

PROCEDURE (ps: path_storage_ptr) move_rel*(dx, dy: bas.double);
BEGIN
  ps.rel_to_abs(dx, dy);
  ps.add_vertex(dx, dy, bas.path_cmd_move_to);
END move_rel;

PROCEDURE (ps: path_storage_ptr) line_to*(x, y: bas.double);
BEGIN
  ps.add_vertex(x, y, bas.path_cmd_line_to);
END line_to ;

PROCEDURE (ps: path_storage_ptr) line_rel*(dx, dy: bas.double);
BEGIN
  ps.rel_to_abs(dx, dy);
  ps.add_vertex(dx, dy, bas.path_cmd_line_to);
END line_rel;

PROCEDURE (ps: path_storage_ptr) hline_to*(x: bas.double);
BEGIN
  ps.add_vertex(x, ps.last_y(), bas.path_cmd_line_to);
END hline_to;

PROCEDURE (ps: path_storage_ptr) hline_rel*(dx: bas.double);
VAR
  dy: bas.double;
BEGIN
  dy := 0;

  ps.rel_to_abs(dx, dy);
  ps.add_vertex(dx, dy, bas.path_cmd_line_to);
END hline_rel;

PROCEDURE (ps: path_storage_ptr) vline_to*(y : bas.double);
BEGIN
  ps.add_vertex(ps.last_x(), y, bas.path_cmd_line_to);
END vline_to;

PROCEDURE (ps: path_storage_ptr) vline_rel*(dy: bas.double);
VAR
  dx: bas.double;
BEGIN
  dx := 0;

  ps.rel_to_abs(dx, dy);
  ps.add_vertex(dx, dy, bas.path_cmd_line_to);
END vline_rel;

PROCEDURE (ps: path_storage_ptr) add_path*(vs: avs.vertex_source_ptr; path_id: bas.int32u; solid_path: BOOLEAN);
VAR
  cmd: SET;
  x, y: bas.double;
BEGIN
  vs.rewind(path_id);
  cmd := vs.vertex(x, y);

  WHILE ~bas.is_stop(cmd) DO
    IF bas.is_move_to(cmd) & solid_path & (ps.total_vertices # 0) THEN
      cmd := bas.path_cmd_line_to;
    END;
    ps.add_vertex(x, y, cmd);
    cmd := vs.vertex(x, y);
  END;
END add_path;

PROCEDURE (ps: path_storage_ptr) arc_to*(rx, ry, angle: bas.double; large_arc_flag, sweep_flag: BOOLEAN; x, y: bas.double);
VAR
  a: aba.bezier_arc_svg_ptr;
  x0, y0: bas.double;
CONST
  epsilon = 1.0E-30;
BEGIN
  a := NIL;

  IF (ps.total_vertices # 0) & bas.is_vertex(ps.command(ps.total_vertices - 1)) THEN

    x0 := 0.0;
    y0 := 0.0;

    IF ps.last_vertex(x0, y0) = {} THEN END;

    rx := ABS(rx);
    ry := ABS(ry);

    (*Ensure radii are valid*)
    IF (rx < epsilon) & (ry < epsilon) THEN
      ps.line_to(x, y);
      RETURN;
    END;

   (* If the endpoints (x, y) and (x0, y0) are identical, then this
    is equivalent to omitting the elliptical arc segment entirely. *)
    IF math.calc_distance(x0, y0, x, y) < epsilon THEN
      RETURN;
    END;

    NEW(a);
    a.ConstructBASVG(x0, y0, rx, ry, angle, large_arc_flag, sweep_flag, x, y);

    IF a.radii_ok THEN
      ps.add_path(a, 0, TRUE)
    ELSE
      ps.line_to(x, y);
    END;
  ELSE
    ps.move_to(x, y);
  END;

  IF a # NIL THEN
    a.Destruct();
  END;
END arc_to;

PROCEDURE (ps: path_storage_ptr) arc_rel*(rx, ry, angle: bas.double; large_arc_flag, sweep_flag: BOOLEAN; dx, dy: bas.double);
BEGIN
  ps.rel_to_abs(dx, dy);
  ps.arc_to    (rx, ry, angle, large_arc_flag, sweep_flag, dx, dy);
END arc_rel;

PROCEDURE (ps: path_storage_ptr) curve3a*(x_ctrl, y_ctrl, x_to, y_to: bas.double);
BEGIN
  ps.add_vertex(x_ctrl, y_ctrl, bas.path_cmd_curve3);
  ps.add_vertex(x_to  , y_to  , bas.path_cmd_curve3);
END curve3a;

PROCEDURE (ps: path_storage_ptr) curve3a_rel*(dx_ctrl, dy_ctrl, dx_to, dy_to: bas.double);
BEGIN
  ps.rel_to_abs(dx_ctrl, dy_ctrl);
  ps.rel_to_abs(dx_to  , dy_to);
  ps.add_vertex(dx_ctrl, dy_ctrl, bas.path_cmd_curve3);
  ps.add_vertex(dx_to  , dy_to  , bas.path_cmd_curve3);
END curve3a_rel;

PROCEDURE (ps: path_storage_ptr) curve3b*(x_to, y_to: bas.double);
VAR
  cmd: SET;
  x0, y0, x_ctrl, y_ctrl : bas.double;
BEGIN
  IF bas.is_vertex(ps.last_vertex(x0, y0)) THEN
    cmd := ps.prev_vertex(x_ctrl, y_ctrl);

    IF bas.is_curve(cmd) THEN
      x_ctrl := x0 + x0 - x_ctrl;
      y_ctrl := y0 + y0 - y_ctrl;
    ELSE
      x_ctrl := x0;
      y_ctrl := y0;
    END;

    ps.curve3a(x_ctrl, y_ctrl, x_to, y_to);
  END;
END curve3b;

PROCEDURE (ps: path_storage_ptr) curve3b_rel*(dx_to, dy_to: bas.double);
BEGIN
  ps.rel_to_abs(dx_to, dy_to);
  ps.curve3b   (dx_to, dy_to);
END curve3b_rel;

PROCEDURE (ps: path_storage_ptr) curve4a*(x_ctrl1, y_ctrl1, x_ctrl2, y_ctrl2, x_to, y_to : bas.double);
BEGIN
  ps.add_vertex(x_ctrl1, y_ctrl1, bas.path_cmd_curve4);
  ps.add_vertex(x_ctrl2, y_ctrl2, bas.path_cmd_curve4);
  ps.add_vertex(x_to   , y_to   , bas.path_cmd_curve4);
END curve4a;

PROCEDURE (ps: path_storage_ptr) curve4a_rel*(dx_ctrl1, dy_ctrl1, dx_ctrl2, dy_ctrl2, dx_to, dy_to : bas.double);
BEGIN
  ps.rel_to_abs(dx_ctrl1, dy_ctrl1);
  ps.rel_to_abs(dx_ctrl2, dy_ctrl2);
  ps.rel_to_abs(dx_to   , dy_to);
  ps.add_vertex(dx_ctrl1, dy_ctrl1, bas.path_cmd_curve4);
  ps.add_vertex(dx_ctrl2, dy_ctrl2, bas.path_cmd_curve4);
  ps.add_vertex(dx_to   , dy_to   , bas.path_cmd_curve4);
END curve4a_rel;

PROCEDURE (ps: path_storage_ptr) curve4b*(x_ctrl2, y_ctrl2, x_to, y_to : bas.double);
VAR
  cmd: SET;
  x0, y0, x_ctrl1, y_ctrl1 : bas.double;
BEGIN
  IF bas.is_vertex(ps.last_vertex(x0, y0)) THEN
    cmd := ps.prev_vertex(x_ctrl1, y_ctrl1);

    IF bas.is_curve(cmd) THEN
      x_ctrl1 := x0 + x0 - x_ctrl1;
      y_ctrl1 := y0 + y0 - y_ctrl1;
    ELSE
      x_ctrl1 := x0;
      y_ctrl1 := y0;
    END;

    ps.curve4a(x_ctrl1, y_ctrl1, x_ctrl2, y_ctrl2, x_to, y_to);
  END;
END curve4b;

PROCEDURE (ps: path_storage_ptr) curve4b_rel*(dx_ctrl2, dy_ctrl2, dx_to, dy_to : bas.double);
BEGIN
  ps.rel_to_abs(dx_ctrl2, dy_ctrl2);
  ps.rel_to_abs(dx_to   , dy_to);
  ps.curve4b(dx_ctrl2, dy_ctrl2, dx_to, dy_to);
END curve4b_rel;

PROCEDURE (ps: path_storage_ptr) end_poly*(flags: SET);
BEGIN
  (*flags := bas.path_flags_close;*)
  IF ps.total_vertices # 0 THEN
    IF bas.is_vertex(ps.command(ps.total_vertices - 1)) THEN
      ps.add_vertex(0.0, 0.0, bas.path_cmd_end_poly + flags);
    END;
  END;
END end_poly;

PROCEDURE (ps: path_storage_ptr) close_polygon*(flags: SET);
BEGIN
 (*flags := bas.path_flags_none*)
  ps.end_poly(bas.path_flags_close + flags);
END close_polygon;

PROCEDURE (ps: path_storage_ptr) add_poly*(vertices: bas.double_array_ptr; num: bas.int32; solid_path: BOOLEAN; end_flags: SET);
VAR
  i: bas.int32;
BEGIN
  (*end_flags := path_flags_none*)
  IF num # 0 THEN
    IF ~solid_path THEN
      ps.move_to(vertices[0], vertices[1]);
      DEC(num);
    END;

    i := 2;
    WHILE num > 0 DO
      ps.line_to(vertices[i], vertices[i + 1]);

      INC(i, 2);
      DEC(num);
    END;

    IF end_flags # {} THEN
      ps.end_poly(end_flags);
    END
  END;
END add_poly;

PROCEDURE (ps: path_storage_ptr) start_new_path*(): bas.int32;
BEGIN
  IF ps.total_vertices # 0 THEN
    IF ~bas.is_stop(ps.command(ps.total_vertices - 1)) THEN
      ps.add_vertex(0.0, 0.0, bas.path_cmd_stop);
    END;
  END;
  RETURN ps.total_vertices;
END start_new_path;

PROCEDURE (ps: path_storage_ptr) rewind*(path_id : bas.int32u);
BEGIN
  ps.iterator := path_id;
END rewind;

PROCEDURE (ps: path_storage_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  r: SET;
BEGIN
  IF ps.iterator >= ps.total_vertices THEN
    r := bas.path_cmd_stop
  ELSE
    r := ps.get_vertex(ps.iterator, x, y);
    INC(ps.iterator);
  END;
  RETURN r;
END vertex;

PROCEDURE (ps: path_storage_ptr) perceive_polygon_orientation(start, end: bas.int32): SET;
VAR
 np, i: bas.int32;
 area, x1, y1, x2, y2: bas.double;

BEGIN
  (* Calculate signed area (double area to be exact) *)
  np   := end - start;
  area := 0.0;

  IF np > 0 THEN
    FOR i := 0 TO np - 1 DO
      IF ps.get_vertex(start + i, x1, y1) = {} THEN END;
      IF ps.get_vertex(start + (i + 1) MOD np, x2, y2) = {} THEN END;

      area := area + (x1 * y2 - y1 * x2);
    END;
  END;
  IF area < 0.0 THEN
    RETURN bas.path_flags_cw
  ELSE
    RETURN bas.path_flags_ccw;
  END;
END perceive_polygon_orientation;

(** Allows you to modify vertex command. The caller must know the index of the vertex. *)
PROCEDURE (ps: path_storage_ptr) modify_command*(idx: bas.int32; cmd: SET);
VAR
  list, item: bas.int32;
BEGIN
  list := bas.shr_int32(idx, list_shift);
  item := bit.and32    (idx, list_mask );

  ps.items^[list][item].cmd := cmd;
END modify_command;

(** Allows you to modify vertex coordinates. The caller must know the index of the vertex. *)
PROCEDURE (ps: path_storage_ptr) modify_vertex*(idx: bas.int32; x, y : bas.double);
VAR
  list, item: bas.int32;
BEGIN
  list := bas.shr_int32(idx, list_shift);
  item := bit.and32    (idx, list_mask );

  ps.items^[list][item].x := x;
  ps.items^[list][item].y := y;
END modify_vertex;

PROCEDURE (ps: path_storage_ptr) swap_vertices(v1, v2: bas.int32);
VAR
  l1, i1: bas.int32;
  l2, i2: bas.int32;
  vi: vertex_item;
BEGIN
  l1 := bas.shr_int32(v1, list_shift);
  i1 := bit.and32    (v1, list_mask );

  l2 := bas.shr_int32(v2, list_shift);
  i2 := bit.and32    (v2, list_mask );

  vi := ps.items^[l1][i1];
  ps.items^[l1][i1] := ps.items^[l2][i2];
  ps.items^[l2][i2] := vi;
END swap_vertices;

PROCEDURE (ps: path_storage_ptr) invert_polygon(start, end: bas.int32);
VAR
  tmp_cmd: SET;
  i: bas.int32;
BEGIN
  tmp_cmd := ps.command(start);
  DEC(end); (* Make "end" inclusive *)

  (* Shift all commands to one position *)
  i := start;

  WHILE i < end DO
    ps.modify_command(i, ps.command(i + 1));
    INC(i);
  END;

  (* Assign starting command to the ending command *)
  ps.modify_command(end, tmp_cmd);

  (* Reverse the polygon *)
  WHILE (end > start) DO
    ps.swap_vertices(start, end);
    INC(start);
    DEC(end);
  END;
END invert_polygon;

(**
  Arrange the orientation of a polygon, all polygons in a path,
  or in all paths. After calling arrange_orientations() or
  arrange_orientations_all_paths(), all the polygons will have
  the same orientation, i.e. path_flags_cw or path_flags_ccw
*)
PROCEDURE (ps: path_storage_ptr) arrange_polygon_orientation*(start: bas.int32; orientation: SET): bas.int32;
VAR
  cmd: SET;
  end: bas.int32;
BEGIN
  IF orientation = bas.path_flags_none THEN
    RETURN start;
  END;

  (* Skip all non-vertices at the beginning *)
  WHILE (start < ps.total_vertices) & ~bas.is_vertex(ps.command(start)) DO
    INC(start);
  END;

  (* Skip all insignificant move_to *)
  WHILE (start + 1 < ps.total_vertices) & bas.is_move_to(ps.command(start)) & bas.is_move_to(ps.command(start + 1)) DO
    INC(start);
  END;

  (* Find the last vertex *)
  end := start + 1;

  WHILE (end < ps.total_vertices) & ~bas.is_next_poly(ps.command(end)) DO
    INC(end);
  END;

  IF end - start > 2 THEN
    IF ps.perceive_polygon_orientation(start, end) # orientation THEN
      (* Invert polygon, set orientation flag, and skip all end_poly *)
      ps.invert_polygon(start, end);

      cmd := ps.command(end);

      WHILE (end < ps.total_vertices) & bas.is_end_poly(cmd) DO
        ps.modify_command(end, bas.set_orientation(cmd, orientation));
        INC(end);
        cmd := ps.command(end);
      END;
    END;
  END;
  RETURN end;
END arrange_polygon_orientation;

PROCEDURE (ps: path_storage_ptr) arrange_orientations*(start: bas.int32; orientation: SET): bas.int32;
BEGIN
  IF orientation # bas.path_flags_none THEN
    LOOP
    IF ~(start < ps.total_vertices) THEN EXIT END;
      start := ps.arrange_polygon_orientation(start, orientation);
      IF bas.is_stop(ps.command(start)) THEN
        INC(start);
        EXIT;
      END;
    END;
  END;
  RETURN start;
END arrange_orientations;

PROCEDURE (ps: path_storage_ptr) arrange_orientations_all_paths*(orientation: SET);
VAR
  start: bas.int32;
BEGIN
  IF orientation # bas.path_flags_none THEN
    start := 0;
    WHILE start < ps.total_vertices DO
      start := ps.arrange_orientations(start, orientation);
    END;
  END;
END arrange_orientations_all_paths;

(** Flip all the vertices horizontally or vertically *)
PROCEDURE (ps: path_storage_ptr) flip_x*(x1, x2: bas.double);
VAR
  i: bas.int32;
  cmd: SET;
  x, y: bas.double;
BEGIN
  IF ps.total_vertices > 0 THEN
    FOR i := 0 TO ps.total_vertices - 1 DO
      cmd := ps.get_vertex(i, x, y);
      IF bas.is_vertex(cmd) THEN
        ps.modify_vertex(i, x2 - x + x1, y);
      END;
    END;
  END;
END flip_x;

PROCEDURE (ps: path_storage_ptr) flip_y*(y1, y2: bas.double);
VAR
  i: bas.int32;
  cmd: SET;
  x, y: bas.double;
BEGIN
  IF ps.total_vertices > 0 THEN
    FOR i := 0 TO ps.total_vertices - 1 DO
      cmd := ps.get_vertex(i, x, y);
      IF bas.is_vertex(cmd) THEN
        ps.modify_vertex(i, x, y2 - y + y1);
      END;
    END;
  END;
END flip_y;

END AggPathStorage.
PROCEDURE (ps: path_storage_ptr)