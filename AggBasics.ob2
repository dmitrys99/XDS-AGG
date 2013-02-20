<* +O2EXTENSIONS *>
<* +O2NUMEXT *>
MODULE AggBasics;

IMPORT SYSTEM;
TYPE
  AGG_INT8*  = SYSTEM.INT8;
  AGG_INT8U* = SYSTEM.CARD8;

  AGG_INT16*  = SYSTEM.INT16;
  AGG_INT16U* = SYSTEM.CARD16;

  AGG_INT32*  = SYSTEM.INT32;
  AGG_INT32U* = SYSTEM.CARD32;

  int8*   = AGG_INT8;
  int8u*  = AGG_INT8U;
  int16*  = AGG_INT16;
  int16u* = AGG_INT16U;
  int32*  = AGG_INT32;
  int32u* = AGG_INT32U;

  int* = int32;
  byte* = int8;
  double* = LONGREAL;
  unsigned* = int32u;

  char* = CHAR;

  double_2 *  = ARRAY  2 OF double;
  double_4 *  = ARRAY  4 OF double;
  double_6 *  = ARRAY  6 OF double;
  double_8 *  = ARRAY  8 OF double;
  double_16*  = ARRAY 16 OF double;
  double_26*  = ARRAY 26 OF double;

  int_4*      = ARRAY  4 OF int32;

  int_array_ptr* = POINTER TO ARRAY OF int32;
  int8u_array_ptr* = POINTER TO ARRAY OF int8u;
  double_array_ptr* = POINTER TO ARRAY OF double;

  (** buffer - плоска€ матрица, состо€ща€ из байтов картинки. *)
  buffer* = POINTER TO ARRAY OF ARRAY OF int8u;

CONST
(* cover_scale_e *)
  cover_shift* = 8;
  cover_size*  = ASH(1, cover_shift);
  cover_mask*  = cover_size - 1;
  cover_none*  = 0;
  cover_full*  = cover_mask;

  pi* = 3.14159265358979323846;

(* path_commands_e *)
  path_cmd_stop*     = { }; (* 0 *)
  path_cmd_move_to*  = {0}; (* 1 *)
  path_cmd_line_to*  = {1}; (* 2 *)
  path_cmd_curve3*   = {2}; (* 3 *)
  path_cmd_curve4*   = {3}; (* 4 *)
  path_cmd_curveN*   = {4}; (* 5 *)
  path_cmd_catrom*   = {5}; (* 6 *)
  path_cmd_ubspline* = {6}; (* 7 *)

  _path_vertex  = {0..6};
  _path_drawing = {1..6};

  path_cmd_end_poly* = {0..3}; (* F *)
  path_cmd_mask*     = {0..3}; (* F *)

(* path_flags_e *)
  path_flags_none*  = {};
  path_flags_ccw*   = {8};         (*010H;*)
  path_flags_cw*    = {9};         (*020H;*)
  path_flags_close* = {10};        (*040H;*)
  path_flags_mask*  = {8..10};

  double_epsilon* = 1.0E-14;

TYPE
  (* «апись объ€вл€етс€ как начальна€ дл€ всех тех, которые могут быть размещены
     структурах из модул€ AggArray *)
  array_item_ptr* = POINTER TO array_item;
  array_item* = RECORD

  END;

  rect* = RECORD
    x1*, y1*, x2*, y2*: int;
  END;
  rect_ptr* = POINTER TO rect;

  rectd* = RECORD
    x1*, y1*, x2*, y2*: double;
  END;

  point_type_ptr* = POINTER TO point_type;
  point_type* = RECORD(array_item)
    x*, y* : double;
  END;

  vertex_type* = RECORD
    x*, y* : double;
    cmd*   : SET;
  END;

PROCEDURE is_equal_eps*(v1, v2, epsilon: double): BOOLEAN;
BEGIN
  RETURN ABS(v1 - v2) < epsilon;
END is_equal_eps;

PROCEDURE ai_ptr*(ai: array_item): array_item_ptr;
BEGIN
  RETURN SYSTEM.VAL(array_item_ptr, SYSTEM.ADR(ai));
END ai_ptr;

PROCEDURE agg_adr*(VAR a: ARRAY OF SYSTEM.BYTE): int32;
BEGIN
  RETURN SYSTEM.ADR(a);
END agg_adr;

PROCEDURE agg_move*(from, to: int32; len: int32);
BEGIN
  SYSTEM.MOVE(from, to, SHORT(len));
END agg_move;

PROCEDURE agg_fill*(address: int32; len: int32; value: int8u);
BEGIN
  SYSTEM.FILL(address, value, len);
END agg_fill;

PROCEDURE agg_min32*(a,b: int32): int32;
BEGIN
  IF a < b THEN
    RETURN a;
  ELSE
    RETURN b;
  END;
END agg_min32;

PROCEDURE agg_max32*(a,b: int32): int32;
BEGIN
  IF a > b THEN
    RETURN a;
  ELSE
    RETURN b;
  END;
END agg_max32;

PROCEDURE agg_getbyte*(source: int32): int8u;
BEGIN
  RETURN SYSTEM.VAL(int8u, source);
END agg_getbyte;

PROCEDURE agg_power*(base, exponent: double): double;
BEGIN
  RETURN base ** exponent;
END agg_power;

(* ---------------------------------------------------- *)
(* rect *)
(* ---------------------------------------------------- *)

PROCEDURE (VAR r: rect) ConstructI*;
BEGIN
  r.x1 := 0;
  r.x2 := 0;
  r.y1 := 0;
  r.y2 := 0;
END ConstructI;

PROCEDURE (VAR r: rect) Construct*(x1, y1, x2, y2 : int);
BEGIN
  r.x1 := x1;
  r.x2 := x2;
  r.y1 := y1;
  r.y2 := y2;
END Construct;

(* ≈сли координаты пр€моугольника идут не в правильном пор€дке, переставл€ем. *)
PROCEDURE (VAR r: rect) Normalize*;
VAR
  t: int;
BEGIN
  IF r.x1 > r.x2 THEN
    t :=r.x1;
    r.x1:=r.x2;
    r.x2:=t;
  END;

  IF r.y1 > r.y2 THEN
    t :=r.y1;
    r.y1:=r.y2;
    r.y2:=t;
  END;
END Normalize;

PROCEDURE (VAR r: rect) Clip*(VAR r1: rect): BOOLEAN;
BEGIN
  IF r.x2 > r1.x2 THEN
    r.x2:=r1.x2;
  END;

  IF r.y2 > r1.y2 THEN
    r.y2:=r1.y2;
  END;

  IF r.x1 < r1.x1 THEN
    r.x1:=r1.x1;
  END;

  IF r.y1 < r1.y1 THEN
   r.y1:=r1.y1;
  END;

  RETURN (r.x1 <= r.x2) & (r.y1 <= r.y2);
END Clip;

PROCEDURE (VAR r: rect) is_valid*(): BOOLEAN;
BEGIN
  RETURN (r.x1 <= r.x2) & (r.y1 <= r.y2);
END is_valid;

PROCEDURE (VAR r: rect) make_invalid*();
BEGIN
  r.x2 := 0;
  r.x1 := 1;
  r.y2 := 0;
  r.y1 := 1;
END make_invalid;

(* ---------------------------------------------------- *)
(* rectd *)
(* ---------------------------------------------------- *)

PROCEDURE (VAR r: rectd) ConstructI*;
BEGIN
  r.x1 := 0;
  r.x2 := 0;
  r.y1 := 0;
  r.y2 := 0;
END ConstructI;

PROCEDURE (VAR r: rectd) Construct*(x1, y1, x2, y2 : double);
BEGIN
  r.x1 := x1;
  r.x2 := x2;
  r.y1 := y1;
  r.y2 := y2;
END Construct;

(* ≈сли координаты пр€моугольника идут не в правильном пор€дке, переставл€ем. *)
PROCEDURE (VAR r: rectd) Normalize*;
VAR
  t: double;
BEGIN
  IF r.x1 > r.x2 THEN
    t :=r.x1;
    r.x1:=r.x2;
    r.x2:=t;
  END;

  IF r.y1 > r.y2 THEN
    t :=r.y1;
    r.y1:=r.y2;
    r.y2:=t;
  END;
END Normalize;

(* ---------------------------------------------------- *)
(* vertex *)
(* ---------------------------------------------------- *)
PROCEDURE (VAR v: vertex_type) ConstructI*;
BEGIN
  v.x := 0;
  v.y := 0;
  v.cmd := {};
END ConstructI;

PROCEDURE (VAR v: vertex_type) Construct*(x,y: double; cmd: SET);
BEGIN
  v.x := x;
  v.y := y;
  v.cmd := cmd;
END Construct;


(* ---------------------------------------------------- *)
(* —амосто€тельные процедуры *)
(* ---------------------------------------------------- *)

(* is_vertex *)

PROCEDURE is_vertex*(c: SET): BOOLEAN;
BEGIN
  RETURN (c * (-_path_vertex) = {}) & (c # {})
(*  RETURN (c * _path_vertex) # {}; *)
END is_vertex;

(* is_drawing *)

PROCEDURE is_drawing*(c: SET): BOOLEAN;
BEGIN
  RETURN ((c * _path_drawing) # {}) & (c # {});
END is_drawing;

(* is_stop *)

PROCEDURE is_stop*(c: SET): BOOLEAN;
BEGIN
  RETURN (c = path_cmd_stop);
END is_stop;

(* is_move *)
PROCEDURE is_move*(c: SET): BOOLEAN;
BEGIN
  RETURN(c = path_cmd_move_to);
END is_move;

(* is_line_to *)
PROCEDURE is_line_to*(c: SET): BOOLEAN;
BEGIN
  RETURN (c = path_cmd_line_to);
END is_line_to;

(* is_move_to *)
PROCEDURE is_move_to*(c: SET): BOOLEAN;
BEGIN
  RETURN(c = path_cmd_move_to );
END is_move_to;

(* is_curve *)
PROCEDURE is_curve*(c: SET): BOOLEAN;
BEGIN
  RETURN (c = path_cmd_curve3) OR (c = path_cmd_curve4);
END is_curve;

(* is_curve3 *)
PROCEDURE is_curve3*(c: SET): BOOLEAN;
BEGIN
  RETURN(c = path_cmd_curve3);
END is_curve3;

(* is_curve4 *)
PROCEDURE is_curve4*(c: SET): BOOLEAN;
BEGIN
  RETURN(c = path_cmd_curve4 );
END is_curve4;

(* is_end_poly *)
PROCEDURE is_end_poly*(c: SET): BOOLEAN;
BEGIN
  RETURN c * path_cmd_mask = path_cmd_end_poly;
END is_end_poly;

(* is_close *)
PROCEDURE is_close*(c: SET): BOOLEAN;
BEGIN
(*  RETURN (c * (path_flags_close + path_cmd_end_poly)) # {}; *)

  RETURN (c - (path_flags_cw + path_flags_ccw)) = (path_cmd_end_poly + path_flags_close);
END is_close;

(* is_next_poly *)
PROCEDURE is_next_poly*(c: SET): BOOLEAN;
BEGIN
  RETURN is_stop(c) OR is_move_to(c) OR is_end_poly(c);
END is_next_poly;

(* is_cw *)
PROCEDURE is_cw*(c: SET): BOOLEAN;
BEGIN
  RETURN (c * path_flags_cw) # {};
END is_cw;

(* is_ccw *)
PROCEDURE is_ccw*(c: SET): BOOLEAN;
BEGIN
  RETURN (c * path_flags_ccw) # {};
END is_ccw;

(* is_oriented *)
PROCEDURE is_oriented*(c: SET): BOOLEAN;
BEGIN
  RETURN (c * (path_flags_ccw + path_flags_cw)) # {};
END is_oriented;

(* is_closed *)
PROCEDURE is_closed*(c: SET): BOOLEAN;
BEGIN
  RETURN (c * path_flags_close) # {};
END is_closed;

(* get_close_flag *)
PROCEDURE get_close_flag*(c: SET): BOOLEAN;
BEGIN
  RETURN (c * path_flags_close) = path_flags_close;
END get_close_flag;

(* clear_orientation *)
PROCEDURE clear_orientation*(c: SET): SET;
BEGIN
  RETURN c - (path_flags_cw + path_flags_ccw);
END clear_orientation;

(* get_orientation *)
PROCEDURE get_orientation*(c: SET): SET;
BEGIN
  RETURN c * (path_flags_cw + path_flags_ccw);
END get_orientation;

(* set_orientation *)
PROCEDURE set_orientation*(c, o: SET): SET;
BEGIN
  RETURN clear_orientation(c) + o;
END set_orientation;

(* NoP *)
PROCEDURE NoP*;
BEGIN

END NoP;

(*
 SHR for signed integers is differently implemented in pascal compilers
 than in c++ compilers. On the assembler level, c++ is using the SAR and
 pascal is using SHR. That gives completely different result, when the
 number is negative. We have to be compatible with c++ implementation,
 thus instead of directly using SHR we emulate c++ solution.

 ’е, дальнейшее исследование показало, что можно смело убирать эти функции
 и использовать ASH, поскольку при по€влении в коде отрицательных чисел или
 смещений компил€тор вставл€ет sar.
*)


PROCEDURE shr_int8*(i, shift: int8): int8;
VAR
  result: int8;
BEGIN
  ASM
    mov al, byte ptr [i]
    mov cl, byte ptr [shift]
    sar al, cl
    mov byte ptr [result], al
  END;
  RETURN result;
END shr_int8;

PROCEDURE shr_int16*(i, shift: int16): int16;
VAR
  result: int16;
BEGIN
  ASM
    mov ax, word ptr [i]
    mov cx, word ptr [shift]
    sar ax, cl
    mov word ptr [result] ,ax
  END;
  RETURN result;
END shr_int16;

PROCEDURE shr_int32*(i, shift: int32): int32;
VAR
  result: int32;
BEGIN
  ASM
    mov eax, dword ptr [i]
    mov ecx, dword ptr [shift]
    sar eax, cl
    mov dword ptr [result], eax
  END;
  RETURN result;
END shr_int32;

END AggBasics.
