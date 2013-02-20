MODULE AggVcgenVertexSequence;

IMPORT
  bas := AggBasics,
  vsq := AggVertexSequence,
  asp := AggShortenPath,
  avs := AggVertexSource;

TYPE
  vcgen_vertex_sequence_ptr* = POINTER TO vcgen_vertex_sequence;
  vcgen_vertex_sequence* = RECORD(avs.vertex_source)
    src_vertices : vsq.vertex_sequence_ptr;
    flags: SET;
    cur_vertex: bas.int32;

    ashorten: bas.double;
    ready: BOOLEAN;
  END;

PROCEDURE (vs: vcgen_vertex_sequence_ptr) Construct*();
BEGIN
  NEW(vs.src_vertices);
  vs.src_vertices.Construct();

  vs.flags      := {};
  vs.cur_vertex := 0;
  vs.ashorten    := 0.0;
  vs.ready      := FALSE;
END Construct;

PROCEDURE (vs: vcgen_vertex_sequence_ptr) Destruct*();
BEGIN
  vs.src_vertices.Destruct();
  vs.src_vertices := NIL;
END Destruct;

PROCEDURE (vs: vcgen_vertex_sequence_ptr) remove_all*();
BEGIN
  vs.ready := FALSE;
  vs.src_vertices.remove_all();
  vs.cur_vertex := 0;
  vs.flags      := {};
END remove_all;

PROCEDURE (vs: vcgen_vertex_sequence_ptr) add_vertex*(x, y: bas.double; cmd: SET);
VAR
  vc: vsq.vertex_dist_cmd_ptr;
BEGIN
  vs.ready := FALSE;

  NEW(vc);

  vc.x := x;
  vc.y := y;

  vc.dist := 0;
  vc.cmd  := cmd;

  IF bas.is_move_to(cmd) THEN
    vs.src_vertices.modify_last(vc)
  ELSE
    IF bas.is_vertex(cmd) THEN
      vs.src_vertices.addfunc(vc)
    ELSE
      vs.flags := cmd + bas.path_flags_mask;
    END;
  END;
END add_vertex;

<*+WOFF301*>

PROCEDURE (vs: vcgen_vertex_sequence_ptr) rewind*(path_id: bas.int32u);
BEGIN
  IF ~vs.ready THEN
    vs.src_vertices.close(bas.is_closed(vs.flags));
    asp.shorten_path(vs.src_vertices, vs.ashorten, bas.get_close_flag(vs.flags));
  END;

  vs.ready      := TRUE;
  vs.cur_vertex := 0;
END rewind;
<*-WOFF301*>

PROCEDURE (vs: vcgen_vertex_sequence_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  v: vsq.vertex_dist_cmd_ptr;

BEGIN
  IF ~vs.ready THEN
    vs.rewind(0);
  END;

  IF vs.cur_vertex = vs.src_vertices.size THEN
    INC(vs.cur_vertex);
    RETURN bas.path_cmd_end_poly + vs.flags;
  END;

  IF vs.cur_vertex > vs.src_vertices.size THEN
    RETURN bas.path_cmd_stop;
  END;

  v := vs.src_vertices.array[vs.cur_vertex](vsq.vertex_dist_cmd_ptr);

  INC(vs.cur_vertex);

  x := v.x;
  y := v.y;

  RETURN v.cmd;
END vertex;

PROCEDURE (vs: vcgen_vertex_sequence_ptr) shorten*(): bas.double;
BEGIN
  RETURN vs.ashorten;
END shorten;

PROCEDURE (vs: vcgen_vertex_sequence_ptr) set_shorten*(s: bas.double);
BEGIN
  vs.ashorten := s;
END set_shorten;

END AggVcgenVertexSequence.
PROCEDURE (vs: vcgen_vertex_sequence_ptr)