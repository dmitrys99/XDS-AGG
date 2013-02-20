MODULE AggVertexSequence;

IMPORT
  bas := AggBasics,
  arr := AggArray,
  agm := AggMath;

CONST
  vertex_dist_epsilon* = 1.0E-14;
  zero_dist = 1.0E14;

TYPE
(*-------------------------------------------------------------vertex_dist
  Vertex (x, y) with the distance to the next one. The last vertex has
  distance between the last and the first points if the polygon is closed
  and 0.0 if it's a polyline.*)

  vertex_dist_ptr* = POINTER TO vertex_dist;
  vertex_dist* = RECORD(bas.array_item)
    x*, y*, dist*: bas.double;
  END;

  vertex_dist_cmd_ptr* = POINTER TO vertex_dist_cmd;
  vertex_dist_cmd* = RECORD(vertex_dist)
    cmd*: SET;
  END;

  func_vertex_sequence* = PROCEDURE (this, val: vertex_dist_ptr): BOOLEAN;

  vertex_sequence_ptr* = POINTER TO vertex_sequence;
  vertex_sequence* = RECORD(arr.pod_deque)
    func-: func_vertex_sequence;
  END;

PROCEDURE vertex_dist_func_operator(this, val: vertex_dist_ptr): BOOLEAN;
VAR
  ret: BOOLEAN;
BEGIN
  this.dist := agm.calc_distance(this.x, this.y, val.x, val.y);

  ret := this.dist > vertex_dist_epsilon;

  IF ~ret THEN
    this.dist := zero_dist;
  END;

  RETURN ret;
END vertex_dist_func_operator;


PROCEDURE (vs: vertex_sequence_ptr) ConstructShiftFunc*(shift: bas.int32; func: func_vertex_sequence);
BEGIN
  vs.ConstructShift(shift);
  IF func = NIL THEN
    vs.func := vertex_dist_func_operator
  ELSE
    vs.func := func
  END;
END ConstructShiftFunc;

PROCEDURE (vs: vertex_sequence_ptr) Construct*();
BEGIN
  vs.ConstructShiftFunc(6, NIL);
END Construct;

PROCEDURE (vs: vertex_sequence_ptr) addfunc*(val: arr.array_item_ptr);
BEGIN
  (*ASSERT(val IS vertex_dist_ptr);*)

  IF vs.size > 1 THEN
    IF ~vs.func(vs.array[vs.size - 2](vertex_dist_ptr), vs.array[vs.size - 1](vertex_dist_ptr)) THEN
      vs.remove_last;
    END;
  END;

  vs.add(val);
END addfunc;

PROCEDURE (vs: vertex_sequence_ptr) modify_last*(val: arr.array_item_ptr);
BEGIN
  vs.remove_last();
  vs.addfunc(val);
END modify_last;

PROCEDURE (vs: vertex_sequence_ptr) close*(remove_flag: BOOLEAN);
VAR
  t: arr.array_item_ptr;
BEGIN
  WHILE (vs.size > 1) & ~(vs.func(vs.array[vs.size - 2](vertex_dist_ptr), vs.array[vs.size - 1](vertex_dist_ptr))) DO
    t := vs.array[vs.size - 1];
    vs.remove_last();
    vs.modify_last(t);
  END;

  IF remove_flag THEN
    WHILE (vs.size > 1) & ~(vs.func(vs.array[vs.size - 1](vertex_dist_ptr), vs.array[0](vertex_dist_ptr))) DO
      vs.remove_last();
    END;
  END;
END close;

END AggVertexSequence.