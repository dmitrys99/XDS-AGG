<* +O2EXTENSIONS *>
MODULE GPC;

TYPE
  double* = LONGREAL;

CONST
  (* Increase GPC_EPSILON to encourage merging of near coincident edges    *)
  GPC_EPSILON * = 2.2204460492503131E-016;
  GPC_VERSION * = "2.32";

(*
===========================================================================
                           Public Data Types
===========================================================================
*)

                                        (* Set operation type                *)
  GPC_DIFF   * = 0;                     (* Difference                        *)
  GPC_INT    * = 1;                     (* Intersection                      *)
  GPC_XOR    * = 2;                     (* Exclusive or                      *)
  GPC_UNION  * = 3;                     (* Union                             *)

TYPE


  gpc_vertex* = RECORD                  (* Polygon vertex structure          *)
    x*: double;                         (* Vertex x component                *)
    y*: double;                         (* vertex y component                *)
  END;

  vertex_list* = POINTER TO ARRAY OF gpc_vertex;
  gpc_vertex_list* = RECORD             (* Vertex list structure             *)
    num_vertices*: LONGINT;             (* Number of vertices in list        *)
    vertex*: vertex_list;               (* Vertex array pointer              *)
  END;

  gpc_polygon_ptr* = POINTER TO gpc_polygon;
  gpc_polygon* = RECORD                 (* Polygon set structure             *)
    num_contours*: LONGINT;             (* Number of contours in polygon     *)
    hole*: POINTER TO ARRAY OF BOOLEAN; (* Hole / external contour flags     *)
                                        (* Contour array pointer             *)
    contour*: POINTER TO ARRAY OF gpc_vertex_list;
  END;

  gpc_tristrip* = RECORD                (* Tristrip set structure            *)
    num_strips*: LONGINT;               (* Number of tristrips               *)
                                        (* Tristrip array pointer            *)
    strip*: POINTER TO ARRAY OF gpc_vertex_list;
  END;


CONST
  LEFT  = 0;
  RIGHT = 1;

  ABOVE = 0;
  BELOW = 1;

  CLIP  = 0;
  SUBJ  = 1;

  INVERT_TRISTRIPS = FALSE;

(* Edge intersection classes *)

  NUL =  0;                             (* Empty non-intersection            *)
  EMX =  1;                             (* External maximum                  *)
  ELI =  2;                             (* External left intermediate        *)
  TED =  3;                             (* Top edge                          *)
  ERI =  4;                             (* External right intermediate       *)
  RED =  5;                             (* Right edge                        *)
  IMM =  6;                             (* Internal maximum and minimum      *)
  IMN =  7;                             (* Internal minimum                  *)
  EMN =  8;                             (* External minimum                  *)
  EMM =  9;                             (* External maximum and minimum      *)
  LED = 10;                             (* Left edge                         *)
  ILI = 11;                             (* Internal left intermediate        *)
  BED = 12;                             (* Bottom edge                       *)
  IRI = 13;                             (* Internal right intermediate       *)
  IMX = 14;                             (* Internal maximum                  *)
  FUL = 15;                             (* Full non-intersection             *)


(* Horizontal edge states *)

  NH = 0;                               (* No horizontal edge                *)
  BH = 1;                               (* Bottom horizontal edge            *)
  TH = 2;                               (* Top horizontal edge               *)


(* Edge bundle state *)

  UNBUNDLED   = 0;                      (* Isolated edge not within a bundle *)
  BUNDLE_HEAD = 1;                      (* Bundle head node                  *)
  BUNDLE_TAIL = 2;                      (* Passive bundle tail node          *)

TYPE
  vertex_node_ptr = POINTER TO vertex_node;
  vertex_node = RECORD                  (* Internal vertex list datatype     *)
    x: double;                          (* X coordinate component            *)
    y: double;                          (* Y coordinate component            *)
    next: vertex_node_ptr;              (* Pointer to next vertex in list    *)
  END;

  polygon_node_ptr = POINTER TO polygon_node;
  polygon_node = RECORD                 (* Internal contour / tristrip type  *)
    active: BOOLEAN;                    (* Active flag / vertex count        *)
    hole: BOOLEAN;                      (* Hole / external contour flag      *)
                                        (* Left and right vertex list ptrs   *)
    v: ARRAY 2 OF POINTER TO ARRAY OF vertex_node_ptr;
    next: polygon_node_ptr;             (* Pointer to next polygon contour   *)
    proxy: polygon_node_ptr;            (* Pointer to actual structure used  *)
  END;

  edge_node_ptr = POINTER TO edge_node;
  edge_node = RECORD
    vertex: gpc_vertex;                 (* Piggy-backed contour vertex data  *)
    bot: gpc_vertex;                    (* Edge lower (x, y) coordinate      *)
    top: gpc_vertex;                    (* Edge upper (x, y) coordinate      *)
    xb: double;                         (* Scanbeam bottom x coordinate      *)
    xt: double;                         (* Scanbeam top x coordinate         *)
    dx: double;                         (* Change in x for a unit y increase *)
    type: BOOLEAN;                      (* Clip / subject edge flag          *)
                                        (* Bundle edge flags                 *)
    bundle: ARRAY 2 OF ARRAY 2 OF LONGINT;
    bside: ARRAY 2 OF LONGINT;          (* Bundle left / right indicators    *)
    bstate: ARRAY 2 OF LONGINT;         (* Edge bundle state                 *)
                                        (* Output polygon / tristrip pointer *)
    outp: ARRAY 2 OF polygon_node_ptr;
    prev: edge_node_ptr;                (* Previous edge in the AET          *)
    next: edge_node_ptr;                (* Next edge in the AET              *)
    pred: edge_node_ptr;                (* Edge connected at the lower end   *)
    succ: edge_node_ptr;                (* Edge connected at the upper end   *)
    next_bound: edge_node_ptr;          (* Pointer to next bound in LMT      *)
  END;
  edge_node_array_ptr = POINTER TO ARRAY OF edge_node_ptr;

  lmt_node_ptr = POINTER TO lmt_node;
  lmt_node = RECORD                     (* Local minima table                *)
    y: double;                          (* Y coordinate at local minimum     *)
    first_bound: edge_node_ptr;         (* Pointer to bound list             *)
    next: lmt_node_ptr;                 (* Pointer to next local minimum     *)
  END ;

  sb_tree_ptr = POINTER TO sb_tree;
  sb_tree = RECORD                      (* Scanbeam tree                     *)
    y: double;                          (* Scanbeam node y value             *)
    less: sb_tree_ptr;                  (* Pointer to nodes with lower y     *)
    more: sb_tree_ptr;                  (* Pointer to nodes with higher y    *)
  END;

  it_node_ptr = POINTER TO it_node;
  it_node = RECORD                      (* Intersection table                *)
    ie: ARRAY 2 OF edge_node_ptr;       (* Intersecting edge (bundle) pair   *)
    point: gpc_vertex;                  (* Point of intersection             *)
    next: it_node_ptr;                  (* The next intersection table node  *)
  END;

  st_node_ptr = POINTER TO st_node;
  st_node = RECORD                      (* Sorted edge table                 *)
    edge: edge_node_ptr;                (* Pointer to AET edge               *)
    xb: double;                         (* Scanbeam bottom x coordinate      *)
    xt: double;                         (* Scanbeam top x coordinate         *)
    dx: double;                         (* Change in x for a unit y increase *)
    prev: st_node_ptr;                  (* Previous edge in sorted list      *)
  END;

  bbox = RECORD                         (* Contour axis-aligned bounding box *)
    xmin: double;                       (* Minimum x coordinate              *)
    ymin: double;                       (* Minimum y coordinate              *)
    xmax: double;                       (* Maximum x coordinate              *)
    ymax: double;                       (* Maximum y coordinate              *)
  END;

(*
===========================================================================
                               Global Data
===========================================================================
*)

(* Horizontal edge state transitions within scanbeam boundary *)

  next_h_state_array = ARRAY 3 OF ARRAY 6 OF LONGINT;

VAR
  next_h_state: next_h_state_array;

(*
===========================================================================
               C Macros, defined as procedures for Oberon
===========================================================================
*)

PROCEDURE EQ(a, b : double): BOOLEAN; BEGIN RETURN ABS(a - b) <= GPC_EPSILON END EQ;
PROCEDURE NE(a, b : double): BOOLEAN; BEGIN RETURN ABS(a - b) > GPC_EPSILON  END NE;
PROCEDURE GT(a, b : double): BOOLEAN; BEGIN RETURN (a - b) > GPC_EPSILON     END GT;
PROCEDURE LT(a, b : double): BOOLEAN; BEGIN RETURN (b - a) > GPC_EPSILON     END LT;
PROCEDURE GE(a, b : double): BOOLEAN; BEGIN RETURN ~LT(a, b)                 END GE;
PROCEDURE LE(a, b : double): BOOLEAN; BEGIN RETURN ~GT(a, b)                 END LE;


PROCEDURE PREV_INDEX(i, n: LONGINT): LONGINT; BEGIN RETURN ((i - 1 + n) MOD n); END PREV_INDEX;
PROCEDURE NEXT_INDEX(i, n: LONGINT): LONGINT; BEGIN RETURN ((i + 1) MOD n);     END NEXT_INDEX;
PROCEDURE OPTIMAL(v: vertex_list; i, n: LONGINT): BOOLEAN;
BEGIN
  RETURN NE(v[PREV_INDEX(i, n)].y, v[i].y) OR NE(v[NEXT_INDEX(i, n)].y, v[i].y);
END OPTIMAL;
PROCEDURE FWD_MIN(v : edge_node_array_ptr; i, n : LONGINT) : BOOLEAN;
BEGIN
  RETURN GE(v[PREV_INDEX(i, n)].vertex.y, v[i].vertex.y) & GT(v[NEXT_INDEX(i, n)].vertex.y, v[i].vertex.y);
END FWD_MIN;

PROCEDURE NOT_FMAX(v : edge_node_array_ptr; i, n : LONGINT) : BOOLEAN;
BEGIN
  RETURN GT(v[NEXT_INDEX(i, n)].vertex.y, v[i].vertex.y);
END NOT_FMAX;

PROCEDURE REV_MIN(v : edge_node_array_ptr; i, n : LONGINT) : BOOLEAN;
BEGIN
  RETURN GT(v[PREV_INDEX(i, n)].vertex.y, v[i].vertex.y) & GE(v[NEXT_INDEX(i, n)].vertex.y, v[i].vertex.y);
END REV_MIN;

PROCEDURE NOT_RMAX(v : edge_node_array_ptr; i, n : LONGINT) : BOOLEAN;
BEGIN
  RETURN GT(v[PREV_INDEX(i, n)].vertex.y, v[i].vertex.y);
END NOT_RMAX;


(*
===========================================================================
                             Private Functions
===========================================================================
*)

PROCEDURE reset_it(VAR it: it_node_ptr);
VAR
  itn: it_node_ptr;
BEGIN
  (* it := NIL?*)
  WHILE (it # NIL) DO
    itn := it.next;
    it  := NIL;
    it  := itn;
  END;
END reset_it;


PROCEDURE reset_lmt(VAR lmt: lmt_node_ptr);
VAR
  lmtn: lmt_node_ptr;
BEGIN
  (*lmt := NIL?*)
  WHILE (lmt # NIL) DO
    lmtn := lmt.next;
    lmt  := NIL;
    lmt  := lmtn;
  END;
END reset_lmt;

PROCEDURE insert_bound(VAR b: edge_node_ptr; e: edge_node_ptr);
VAR
  existing_bound: edge_node_ptr;
BEGIN
  IF b = NIL THEN

    (* Link node e to the tail of the list **)
    b := e;

  ELSE

    (* Do primary sort on the x field *)
    IF (e.bot.x < b.bot.x) THEN
      (* Insert a new node mid-list *)
      existing_bound := b;
      b := e;
      b.next_bound := existing_bound;
    ELSE

      IF (e.bot.x = b.bot.x) THEN
        (* Do secondary sort on the dx field *)

        IF (e.dx < b.dx) THEN
          (* Insert a new node mid-list *)
          existing_bound := b;
          b := e;
          b.next_bound := existing_bound;
        ELSE
          (* Head further down the list *)
          insert_bound(b.next_bound, e);
        END

      ELSE
        (* Head further down the list *)
        insert_bound(b.next_bound, e);
      END
    END
  END
END insert_bound;

PROCEDURE bound_list(VAR lmt: lmt_node_ptr; y: double; VAR ret: edge_node_ptr);
VAR
  existing_node: lmt_node_ptr;
BEGIN
  IF (lmt = NIL) THEN
    (* Add node onto the tail end of the LMT *)
    NEW(lmt);
    (*"LMT insertion"*)
    lmt.y := y;
    lmt.first_bound := NIL;
    lmt.next := NIL;
    ret := lmt.first_bound;
    RETURN;
  ELSE
    IF (y < lmt.y) THEN
      (* Insert a new LMT node before the current node *)
      existing_node := lmt;
      NEW(lmt);
      (*"LMT insertion"*)
      lmt.y := y;
      lmt.first_bound := NIL;
      lmt.next := existing_node;
      ret := lmt.first_bound;
      RETURN;
    ELSE
      IF (y > lmt.y) THEN
        (* Head further up the LMT *)
        bound_list(lmt.next, y, ret);
        RETURN;
      ELSE
        (* Use this existing LMT node *)
        ret := lmt.first_bound;
        RETURN;
      END;
    END;
  END;
END bound_list;

PROCEDURE add_to_sbtree(VAR entries: LONGINT; VAR sbtree: sb_tree_ptr; y: double);
BEGIN
  IF (sbtree = NIL) THEN
    (* Add a new tree node here *)
    NEW(sbtree);
    (* "scanbeam tree insertion"*)
    sbtree.y := y;
    sbtree.less := NIL;
    sbtree.more := NIL;
    INC(entries);
  ELSE
    IF (sbtree.y > y) THEN
    (* Head into the 'less' sub-tree *)
      add_to_sbtree(entries, sbtree.less, y);
    ELSE
      IF (sbtree.y < y) THEN
        (* Head into the 'more' sub-tree *)
        add_to_sbtree(entries, sbtree.more, y);
      END;
    END;
  END;
END add_to_sbtree;

PROCEDURE build_sbt(VAR entries: LONGINT; VAR sbt: ARRAY OF double; sbtree: sb_tree_ptr);
BEGIN
  IF (sbtree.less # NIL) THEN
    build_sbt(entries, sbt, sbtree.less);
  END;

  sbt[entries] := sbtree.y;
  INC(entries);

  IF (sbtree.more # NIL) THEN
    build_sbt(entries, sbt, sbtree.more);
  END;
END build_sbt;

PROCEDURE free_sbtree(VAR sbtree: sb_tree_ptr);
BEGIN
  (* sbtree := NIL?*)
  IF (sbtree # NIL) THEN
    free_sbtree(sbtree.less);
    free_sbtree(sbtree.more);
    sbtree := NIL;
  END;
END free_sbtree;

PROCEDURE count_optimal_vertices(c: gpc_vertex_list): LONGINT;
VAR
  i, result: LONGINT;
BEGIN
  result := 0;
  (* Ignore non-contributing contours *)
  IF (c.num_vertices > 0) THEN
    FOR i := 0 TO c.num_vertices - 1 DO
      (* Ignore superfluous vertices embedded in horizontal edges *)
      IF (OPTIMAL(c.vertex, i, c.num_vertices)) THEN
        INC(result);
      END;
    END;
  END;
  RETURN result;
END count_optimal_vertices;

PROCEDURE build_lmt(VAR lmt: lmt_node_ptr;
                    VAR sbtree: sb_tree_ptr;
                    VAR sbt_entries: LONGINT;
                    p: gpc_polygon_ptr;
                    type: BOOLEAN;
                    op: LONGINT): edge_node_array_ptr;
VAR
  e, c, i, min, max, num_edges, v, num_vertices, total_vertices, e_index: LONGINT;
  edge_table: edge_node_array_ptr;
  ret: edge_node_ptr;
BEGIN
  e_index := 0;
  total_vertices := 0;
  FOR c := 0 TO p.num_contours-1 DO
    INC(total_vertices, count_optimal_vertices(p.contour^[c]));
  END;

  (* Create the entire input polygon edge table in one go *)
  NEW(edge_table, total_vertices);
  FOR i := 0 TO total_vertices - 1 DO
    NEW(edge_table[i]);
  END;
  (*"edge table creation"*)

  FOR c := 0 TO p.num_contours-1 DO
    IF p.contour[c].num_vertices < 0 THEN
      (* Ignore the non-contributing contour and repair the vertex count *)
      p.contour[c].num_vertices := -p.contour[c].num_vertices;
    ELSE
      (* Perform contour optimisation *)
      num_vertices := 0;
      FOR i := 0 TO p.contour[c].num_vertices - 1 DO
        IF (OPTIMAL(p.contour[c].vertex, i, p.contour[c].num_vertices)) THEN
          edge_table[num_vertices].vertex.x := p.contour[c].vertex[i].x;
          edge_table[num_vertices].vertex.y := p.contour[c].vertex[i].y;

          (* Record vertex in the scanbeam table *)
          add_to_sbtree(sbt_entries, sbtree, edge_table[num_vertices].vertex.y);
          INC(num_vertices);
        END;
      END;


      (* Do the contour forward pass *)
      FOR min := 0 TO num_vertices-1 DO
        (* If a forward local minimum... *)
        IF (FWD_MIN(edge_table, min, num_vertices)) THEN
          (* Search for the next local maximum... *)
          num_edges := 1;
          max := NEXT_INDEX(min, num_vertices);
          WHILE (NOT_FMAX(edge_table, max, num_vertices)) DO
            INC(num_edges);
            max := NEXT_INDEX(max, num_vertices);
          END;

          (* Build the next edge list *)
          e := e_index;
          INC(e_index, num_edges);
          v := min;
          edge_table[e].bstate[BELOW] := UNBUNDLED;
          edge_table[e].bundle[BELOW][CLIP] := 0;
          edge_table[e].bundle[BELOW][SUBJ] := 0;
          FOR i := 0 TO num_edges - 1 DO
            edge_table[e + i].xb := edge_table[v].vertex.x;
            edge_table[e + i].bot.x := edge_table[v].vertex.x;
            edge_table[e + i].bot.y := edge_table[v].vertex.y;

            v := NEXT_INDEX(v, num_vertices);

            edge_table[e + i].top.x := edge_table[v].vertex.x;
            edge_table[e + i].top.y := edge_table[v].vertex.y;
            edge_table[e + i].dx := (edge_table[v].vertex.x - edge_table[e + i].bot.x) / (edge_table[e + i].top.y - edge_table[e + i].bot.y);
            edge_table[e + i].type := type;
            edge_table[e + i].outp[ABOVE] := NIL;
            edge_table[e + i].outp[BELOW] := NIL;
            edge_table[e + i].next := NIL;
            edge_table[e + i].prev := NIL;

            IF (num_edges > 1) & (i < (num_edges - 1)) THEN
              edge_table[e + i].succ := edge_table[e + i + 1];
            ELSE
              edge_table[e + i].succ := NIL;
            END;

            IF ((num_edges > 1) & (i > 0)) THEN
              edge_table[e + i].pred := (edge_table[e + i - 1])
            ELSE
              edge_table[e + i].pred := NIL;
            END;

            edge_table[e + i].next_bound := NIL;

            IF (op = GPC_DIFF) THEN
              edge_table[e + i].bside[CLIP] := RIGHT;
            ELSE
              edge_table[e + i].bside[CLIP] := LEFT;
            END;

            edge_table[e + i].bside[SUBJ] := LEFT;
          END;
          bound_list(lmt, edge_table[min].vertex.y, ret);
          insert_bound(ret, edge_table[e]);
        END;
      END;

      (* Do the contour reverse pass *)
      FOR min := 0 TO num_vertices-1 DO
      (* If a reverse local minimum... *)
        IF REV_MIN(edge_table, min, num_vertices) THEN
          (* Search for the previous local maximum... *)
          num_edges := 1;
          max := PREV_INDEX(min, num_vertices);
          WHILE (NOT_RMAX(edge_table, max, num_vertices)) DO
            INC(num_edges);
            max := PREV_INDEX(max, num_vertices);
          END;

          (* Build the previous edge list *)
          e := e_index;
          INC(e_index, num_edges);
          v := min;
          edge_table[e].bstate[BELOW] := UNBUNDLED;
          edge_table[e].bundle[BELOW][CLIP] := 0;
          edge_table[e].bundle[BELOW][SUBJ] := 0;
          FOR i := 0 TO num_edges-1 DO
            edge_table[e + i].xb := edge_table[v].vertex.x;
            edge_table[e + i].bot.x := edge_table[v].vertex.x;
            edge_table[e + i].bot.y := edge_table[v].vertex.y;

            v := PREV_INDEX(v, num_vertices);

            edge_table[e + i].top.x := edge_table[v].vertex.x;
            edge_table[e + i].top.y := edge_table[v].vertex.y;
            edge_table[e + i].dx := (edge_table[v].vertex.x - edge_table[e + i].bot.x) / (edge_table[e + i].top.y - edge_table[e + i].bot.y);
            edge_table[e + i].type := type;
            edge_table[e + i].outp[ABOVE] := NIL;
            edge_table[e + i].outp[BELOW] := NIL;
            edge_table[e + i].next := NIL;
            edge_table[e + i].prev := NIL;

            IF ((num_edges > 1) & (i < (num_edges - 1))) THEN
              edge_table[e + i].succ := edge_table[e + i + 1]
            ELSE
              edge_table[e + i].succ := NIL;
            END;

            IF ((num_edges > 1) & (i > 0)) THEN
              edge_table[e + i].pred := edge_table[e + i - 1]
            ELSE
              edge_table[e + i].pred := NIL;
            END;

            edge_table[e + i].next_bound := NIL;

            IF (op = GPC_DIFF) THEN
              edge_table[e + i].bside[CLIP] := RIGHT
            ELSE
              edge_table[e + i].bside[CLIP] := LEFT;
            END;

            edge_table[e + i].bside[SUBJ] := LEFT;
          END;
          bound_list(lmt, edge_table[min].vertex.y, ret);
          insert_bound(ret, edge_table[e]);
        END;
      END;
    END;
  END;
  RETURN edge_table;
END build_lmt;

PROCEDURE add_intersection(VAR it: it_node_ptr; edge0, edge1: edge_node_ptr;
                             x, y: double);
VAR
  existing_node: it_node_ptr;
BEGIN
  IF (it = NIL) THEN
    (* Append a new node to the tail of the list *)
    NEW(it);
    (*"IT insertion"*)
    it.ie[0] := edge0;
    it.ie[1] := edge1;
    it.point.x := x;
    it.point.y := y;
    it.next := NIL;
  ELSE
    IF (it.point.y > y) THEN
      (* Insert a new node mid-list *)
      existing_node := it;
      NEW(it);
      (*"IT insertion"*)
      it.ie[0] := edge0;
      it.ie[1] := edge1;
      it.point.x := x;
      it.point.y := y;
      it.next := existing_node;
    ELSE
      (* Head further down the list *)
      add_intersection(it.next, edge0, edge1, x, y);
    END;
  END;
END add_intersection;


BEGIN
  next_h_state := next_h_state_array{
  (*        ABOVE     BELOW     CROSS *)
  (*        L   R     L   R     L   R *)
  (* NH *) {BH, TH,   TH, BH,   NH, NH},
  (* BH *) {NH, NH,   NH, NH,   TH, TH},
  (* TH *) {NH, NH,   NH, NH,   BH, BH}
  };
END GPC.