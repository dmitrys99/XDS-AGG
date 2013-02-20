MODULE AggConvAdaptorVcgen;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource,

  out := Out, SYSTEM;

TYPE
  null_markers_ptr* = POINTER TO null_markers;
  null_markers* = RECORD(avs.vertex_source)
    markers-: avs.vertex_source_ptr;
  END;

  conv_adaptor_vcgen_ptr* = POINTER TO conv_adaptor_vcgen;
  conv_adaptor_vcgen* = RECORD(avs.vertex_source)
    source,
    generator- : avs.vertex_source_ptr;
    markers    : null_markers_ptr;
    status     : bas.int32;
    last_cmd   : SET;
    start_x,
    start_y    : bas.double;
  END;

CONST
  initial    * = 0;
  accumulate * = 1;
  generate   * = 2;

(*==============*)
(* null_markers *)
(*==============*)

PROCEDURE (nm: null_markers_ptr) Construct*();
BEGIN
  nm.Construct^();
  nm.markers := NIL;
END Construct;

PROCEDURE (nm: null_markers_ptr) Destruct*();
BEGIN
  nm.markers := NIL;
  nm.Destruct^();
END Destruct;

PROCEDURE (nm: null_markers_ptr) remove_all*();
BEGIN
  IF nm.markers # NIL THEN
    nm.markers.remove_all();
  END;
END remove_all;

PROCEDURE (nm: null_markers_ptr) add_vertex*(x, y: bas.double; cmd: SET);
BEGIN
  IF nm.markers # NIL THEN
    nm.markers.add_vertex(x, y, cmd);
  END;
END add_vertex;

PROCEDURE (nm: null_markers_ptr) prepare_src*();
BEGIN
END prepare_src;

PROCEDURE (nm: null_markers_ptr) rewind*(path_id: bas.int32u);
BEGIN
  IF nm.markers # NIL THEN
    nm.markers.rewind(path_id);
  END;
END rewind;

PROCEDURE (nm: null_markers_ptr) vertex*(VAR x, y: bas.double): SET;
BEGIN
  IF nm.markers # NIL THEN
    RETURN nm.markers.vertex(x, y)
  ELSE
    RETURN bas.path_cmd_stop;
  END;
END vertex;

PROCEDURE (nm: null_markers_ptr) set_markers(VAR m: avs.vertex_source_ptr);
BEGIN
  nm.markers := m;
END set_markers;

(*====================*)
(* conv_adaptor_vcgen *)
(*====================*)
(*@@@*)
PROCEDURE (ca: conv_adaptor_vcgen_ptr) Construct*();
BEGIN
  ca.Construct^();
  NEW(ca.generator);
END Construct;

PROCEDURE (ca: conv_adaptor_vcgen_ptr) ConstructGen*(source, gen: avs.vertex_source_ptr);
BEGIN
 ca.Construct();

 ca.source := source;
 ca.status := initial;

 ca.generator := gen;
 NEW(ca.markers);
 ca.markers.Construct();

 ca.last_cmd := {};
 ca.start_x  := 0;
 ca.start_y  := 0;

END ConstructGen;

PROCEDURE (ca: conv_adaptor_vcgen_ptr) Destruct*();
BEGIN
  ca.markers.Destruct();
  ca.markers := NIL;
  ca.Destruct^();
END Destruct;

PROCEDURE (ca: conv_adaptor_vcgen_ptr) set_source*(source: avs.vertex_source_ptr);
BEGIN
  ca.source := source;
END set_source;

PROCEDURE (ca: conv_adaptor_vcgen_ptr) set_markers*(VAR m: avs.vertex_source_ptr);
BEGIN
  ASSERT(ca.markers # NIL);
  ca.markers.set_markers(m);
END set_markers;

PROCEDURE (ca: conv_adaptor_vcgen_ptr) get_markers*(): avs.vertex_source_ptr;
BEGIN
  ASSERT(ca.markers # NIL);
  IF ca.markers.markers # NIL THEN
    RETURN ca.markers.markers
  ELSE
    RETURN ca.markers;
  END;
END get_markers;

PROCEDURE (ca: conv_adaptor_vcgen_ptr) rewind*(path_id: bas.int32u);
BEGIN
 ca.source.rewind(path_id);
 ca.status:=initial;
END rewind;

PROCEDURE (ca: conv_adaptor_vcgen_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  cmd: SET;
  done: BOOLEAN;
BEGIN
  (*out.String('Enter (ca: conv_adaptor_vcgen_ptr) vertex'); out.Ln();*)
  ASSERT(ca.markers # NIL);
  cmd := bas.path_cmd_stop;
  done := FALSE;
  WHILE ~done DO
    IF ca.status = initial THEN
      ca.markers.remove_all();
      ca.last_cmd := ca.source.vertex(ca.start_x, ca.start_y);
      ca.status := accumulate;
    END;

    IF ca.status = accumulate THEN
      (*out.String("ca.status = accumulate"); out.Ln();*)
      IF bas.is_stop(ca.last_cmd) THEN RETURN bas.path_cmd_stop END;

      ca.generator.remove_all();
      ca.generator.add_vertex(ca.start_x, ca.start_y, bas.path_cmd_move_to);
      ca.markers.add_vertex(ca.start_x, ca.start_y, bas.path_cmd_move_to);
      LOOP

        cmd := ca.source.vertex(x, y);

(*    out.String('>cmd='); out.Int(SYSTEM.VAL(LONGINT, cmd), 8);
    out.String(' x=');   out.Int(ENTIER(x*100),5);
    out.String(' y=');   out.Int(ENTIER(y*100),5);
    out.Ln();*)

        IF (bas.is_vertex(cmd)) THEN
          ca.last_cmd := cmd;
          IF(bas.is_move_to(cmd)) THEN
            ca.start_x := x;
            ca.start_y := y;
            EXIT;
          END;
          ca.generator.add_vertex(x, y, cmd);
          ca.markers.add_vertex(x, y, bas.path_cmd_line_to);
        ELSE
          IF(bas.is_stop(cmd)) THEN
            ca.last_cmd := bas.path_cmd_stop;
            EXIT;
          END;
          IF(bas.is_end_poly(cmd)) THEN
            ca.generator.add_vertex(x, y, cmd);
            EXIT;
          END;
        END;
      END;
      ca.generator.rewind(0);
      ca.status := generate;
    END;

    IF ca.status = generate THEN
      (*out.String("ca.status = generate"); out.Ln();*)
      cmd := ca.generator.vertex(x, y);
      IF(bas.is_stop(cmd)) THEN
        (*out.String("bas.is_stop(cmd)"); out.Ln();*)
        ca.status := accumulate;
      ELSE
        (*out.String(" ~ bas.is_stop(cmd)"); out.Ln();*)
        done := TRUE;
      END;
    END;
  END;
  (*out.String('Exit (ca: conv_adaptor_vcgen_ptr) vertex'); out.Ln();*)
  RETURN cmd;
END vertex;

END AggConvAdaptorVcgen.