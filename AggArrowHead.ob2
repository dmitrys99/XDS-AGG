MODULE AggArrowHead;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource;

TYPE
  arrowhead_ptr* = POINTER TO arrowhead;
  arrowhead* = RECORD(avs.vertex_source)
    head_d1,
    head_d2,
    head_d3,
    head_d4,
    tail_d1,
    tail_d2,
    tail_d3,
    tail_d4: bas.double;

    head_flag,
    tail_flag: BOOLEAN;

    coord: bas.double_16;
    cmd:   ARRAY 8 OF SET;

    curr_id,
    curr_coord: bas.int32u;
  END;

PROCEDURE (ah: arrowhead_ptr) Construct*();
BEGIN
  ah.head_d1 := 1.0;
  ah.head_d2 := 1.0;
  ah.head_d3 := 1.0;
  ah.head_d4 := 0.0;
  ah.tail_d1 := 1.0;
  ah.tail_d2 := 1.0;
  ah.tail_d3 := 1.0;
  ah.tail_d4 := 0.0;

  ah.head_flag := FALSE;
  ah.tail_flag := FALSE;

  ah.curr_id    := 0;
  ah.curr_coord := 0;
END Construct;

PROCEDURE (ah: arrowhead_ptr) head*();
BEGIN
  ah.head_flag := TRUE;
END head;

PROCEDURE (ah: arrowhead_ptr) no_head*();
BEGIN
  ah.head_flag := FALSE;
END no_head;

PROCEDURE (ah: arrowhead_ptr) tail*();
BEGIN
  ah.tail_flag := TRUE;
END tail;
PROCEDURE (ah: arrowhead_ptr) no_tail*();
BEGIN
  ah.tail_flag := FALSE;
END no_tail;

PROCEDURE (ah: arrowhead_ptr) set_head*(d1, d2, d3, d4: bas.double);
BEGIN
  ah.head_d1 := d1;
  ah.head_d2 := d2;
  ah.head_d3 := d3;
  ah.head_d4 := d4;

  ah.head_flag := TRUE;
END set_head;

PROCEDURE (ah: arrowhead_ptr) set_tail*(d1, d2, d3, d4: bas.double);
BEGIN
  ah.tail_d1 := d1;
  ah.tail_d2 := d2;
  ah.tail_d3 := d3;
  ah.tail_d4 := d4;

  ah.tail_flag := TRUE;
END set_tail;

PROCEDURE (ah: arrowhead_ptr) rewind*(path_id: bas.int32u);
BEGIN
  ah.curr_id    := path_id;
  ah.curr_coord := 0;

  IF path_id = 0 THEN

    IF ~ah.tail_flag THEN
      ah.cmd[0] := bas.path_cmd_stop;
      RETURN;
    END;

    ah.coord[0]  :=  ah.tail_d1;
    ah.coord[1]  :=  0.0;
    ah.coord[2]  :=  ah.tail_d1 - ah.tail_d4;
    ah.coord[3]  :=  ah.tail_d3;
    ah.coord[4]  := -ah.tail_d2 - ah.tail_d4;
    ah.coord[5]  :=  ah.tail_d3;
    ah.coord[6]  := -ah.tail_d2;
    ah.coord[7]  :=  0.0;
    ah.coord[8]  := -ah.tail_d2 - ah.tail_d4;
    ah.coord[9]  := -ah.tail_d3;
    ah.coord[10] :=  ah.tail_d1 - ah.tail_d4;
    ah.coord[11] := -ah.tail_d3;

    ah.cmd[0] := bas.path_cmd_move_to;
    ah.cmd[1] := bas.path_cmd_line_to;
    ah.cmd[2] := bas.path_cmd_line_to;
    ah.cmd[3] := bas.path_cmd_line_to;
    ah.cmd[4] := bas.path_cmd_line_to;
    ah.cmd[5] := bas.path_cmd_line_to;
    ah.cmd[7] := bas.path_cmd_end_poly + bas.path_flags_close + bas.path_flags_ccw;
    ah.cmd[6] := bas.path_cmd_stop;

    RETURN;
  END;

  IF path_id = 1 THEN
    IF ~ah.head_flag THEN
       ah.cmd[0] := bas.path_cmd_stop;
       RETURN
    END;

    ah.coord[0] := -ah.head_d1;
    ah.coord[1] :=  0.0;
    ah.coord[2] :=  ah.head_d2 + ah.head_d4;
    ah.coord[3] := -ah.head_d3;
    ah.coord[4] :=  ah.head_d2;
    ah.coord[5] :=  0.0;
    ah.coord[6] :=  ah.head_d2 + ah.head_d4;
    ah.coord[7] :=  ah.head_d3;

    ah.cmd[0] := bas.path_cmd_move_to;
    ah.cmd[1] := bas.path_cmd_line_to;
    ah.cmd[2] := bas.path_cmd_line_to;
    ah.cmd[3] := bas.path_cmd_line_to;
    ah.cmd[4] := bas.path_cmd_end_poly + bas.path_flags_close + bas.path_flags_ccw;
    ah.cmd[5] := bas.path_cmd_stop;
  END;
END rewind;

PROCEDURE (ah: arrowhead_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  curr_idx: bas.int32u;
  c: SET;
BEGIN
  IF ah.curr_id < 2 THEN
    curr_idx := ah.curr_coord * 2;

    x := ah.coord[curr_idx];
    y := ah.coord[curr_idx + 1];

    c := ah.cmd[ah.curr_coord];
    ah.curr_coord := ah.curr_coord + 1;

    RETURN c;
  ELSE
    RETURN bas.path_cmd_stop;
  END;

END vertex;

END AggArrowHead.