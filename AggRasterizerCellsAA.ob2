<*+O2EXTENSIONS*>
MODULE AggRasterizerCellsAA;

IMPORT
(* DEBUG 
  SeqFile,
  SWholeIO,
  STextIO,
  FormStr,
  RawIO,
  Out,
*)
  bas := AggBasics,
  scl := AggScanline,
  bit := AggBit;

(*
  Class outline_aa - implementation.

  Initially the rendering algorithm was designed by David Turner and the
  other authors of the FreeType library - see the above notice. I nearly
  created a similar renderer, but still I was far from David's work.
  I completely redesigned the original code and adapted it for Anti-Grain
  ideas. Two functions - render_line and render_hline are the core of
  the algorithm - they calculate the exact coverage of each pixel cell
  of the polygon. I left these functions almost as is, because there's
  no way to improve the perfection - hats off to David and his group!

  All other code is very different from the original.
*)

CONST
  cell_block_shift * = 12;
  cell_block_size  * = ASH(1, cell_block_shift);
  cell_block_mask  * = cell_block_size - 1;
  cell_block_pool  * = 256;
  cell_block_limit * = 1024;

(*
  These constants determine the subpixel accuracy, to be more precise,
  the number of bits of the fractional part of the coordinates.
  The possible coordinate capacity in bits can be calculated by formula:
  sizeof(int) * 8 - poly_base_shift * 2, i.e, for 32-bit integers and
  8-bits fractional part the capacity is 16 bits or [-32768...32767].
*)

  poly_base_shift * = 8;                       (* ----poly_base_shift *)
  poly_base_size  * = ASH(1, poly_base_shift); (* ----poly_base_size  *)
  poly_base_mask  * = poly_base_size - 1;      (* ----poly_base_mask  *)

  qsort_threshold = 9;

TYPE

(*
  -----------------------------------------------------------------cell_aa
  A pixel cell. There're no constructors defined and it was done
  intentionally in order to avoid extra overhead when allocating an
  array of cells.
*)
  cell_aa_ptr* = POINTER TO cell_aa;
  cell_aa* = RECORD(bas.array_item);
    x*, y*, cover*, area*: bas.int32;
  END;
  cell_aa_array_ptr* = POINTER TO ARRAY OF cell_aa;
  cells_aa* = POINTER TO ARRAY OF cell_aa_array_ptr;

(*
  --------------------------------------------------------------outline_aa
  An internal class that implements the main rasterization algorithm.
  Used in the rasterizer. Should not be used direcly.
*)

  sorted_y = RECORD
    start, num: bas.int32;
  END;


  (* "Указатель" на ячейку. *)
  cell_ptr* = RECORD
    A-, B-: bas.int32;
  END;


  rasterizer_cells_aa* = RECORD
    num_blocks*,
    max_blocks*,
    cur_block*,
    num_cells*: bas.int32;

    cur_x,
    cur_y,
    min_x *,
    min_y *,
    max_x *,
    max_y *: bas.int32;

    sorted*: BOOLEAN;

    cells-         : cells_aa;

    cur_cell_array_ptr  : cell_aa_array_ptr;
    cur_cell_array_idx  : bas.int32;

    cur_cell     * : cell_aa;

    sorted_cells- : POINTER TO ARRAY OF cell_ptr;
    sorted_y      : POINTER TO ARRAY OF sorted_y;
  END;


  scanline_hit_test_ptr* = POINTER TO scanline_hit_test;
  scanline_hit_test* = RECORD(scl.scanline)
    x: bas.int32;
    hit*: BOOLEAN;
  END;

(* reset *)

PROCEDURE (VAR rca: rasterizer_cells_aa) reset*();
BEGIN
  rca.num_cells := 0;
  rca.cur_block := 0;

  rca.cur_cell.x     := 7FFFH;
  rca.cur_cell.y     := 7FFFH;
  rca.cur_cell.cover := 0;
  rca.cur_cell.area  := 0;

  rca.sorted := FALSE;

  rca.min_x :=  7FFFFFFFH;
  rca.min_y :=  7FFFFFFFH;
  rca.max_x := -7FFFFFFFH;
  rca.max_y := -7FFFFFFFH;
END reset;

(* Construct *)

PROCEDURE (VAR rca: rasterizer_cells_aa) Construct*();
BEGIN
  rca.num_cells  := 0;
  rca.cur_block  := 0;

  rca.max_blocks := 0;
  rca.num_blocks := 0;

  rca.cur_x := 0;
  rca.cur_y := 0;
  rca.min_x := 7FFFFFFFH;
  rca.min_y := 7FFFFFFFH;
  rca.max_x := -7FFFFFFFH;
  rca.max_y := -7FFFFFFFH;

  rca.sorted := FALSE;

  rca.cells         := NIL;
  rca.sorted_y      := NIL;

(* rca.cur_cell_ptr := NIL;*)

  rca.cur_cell.x     := 7FFFH;
  rca.cur_cell.y     := 7FFFH;
  rca.cur_cell.cover := 0;
  rca.cur_cell.area  := 0;
END Construct;

(* Destruct *)

PROCEDURE (VAR rca: rasterizer_cells_aa) Destruct*();
BEGIN
  rca.sorted_y     := NIL;
  rca.cells        := NIL;
  rca.sorted_cells := NIL;
END Destruct;

(* allocate_block *)

PROCEDURE (VAR rca: rasterizer_cells_aa) allocate_block();
VAR
  new_cells: cells_aa;
  i: bas.int32;
BEGIN
  IF rca.cur_block >= rca.num_blocks THEN
    IF rca.num_blocks >= rca.max_blocks THEN

      NEW(new_cells, rca.max_blocks + cell_block_pool);

      IF rca.cells # NIL THEN
        i := rca.max_blocks;
        WHILE i >= 0 DO
          new_cells[i] := rca.cells[i];
          DEC(i);
        END;
      END;

      rca.cells := new_cells;
      INC(rca.max_blocks, cell_block_pool);
    END;

    NEW(rca.cells[rca.num_blocks], cell_block_size);
    INC(rca.num_blocks);
  END;

  rca.cur_cell_array_ptr := rca.cells[rca.cur_block];
  rca.cur_cell_array_idx := 0;
  INC(rca.cur_block);
END allocate_block;

(* add_cur_cell *)

PROCEDURE (VAR rca: rasterizer_cells_aa) add_cur_cell*();
BEGIN
  IF bit.or32(rca.cur_cell.area, rca.cur_cell.cover) # 0 THEN
    IF bit.and32(rca.num_cells, cell_block_mask) = 0 THEN

      IF rca.num_blocks >= cell_block_limit THEN
        RETURN;
      END;

      rca.allocate_block;
    END;

    rca.cur_cell_array_ptr^[rca.cur_cell_array_idx] := rca.cur_cell;

    INC(rca.cur_cell_array_idx);
    INC(rca.num_cells);

    IF rca.cur_cell.x < rca.min_x THEN
      rca.min_x := rca.cur_cell.x;
    END;

    IF rca.cur_cell.x > rca.max_x THEN
      rca.max_x := rca.cur_cell.x;
    END;

    IF rca.cur_cell.y < rca.min_y THEN
      rca.min_y := rca.cur_cell.y;
    END;

    IF rca.cur_cell.y > rca.max_y THEN
      rca.max_y := rca.cur_cell.y;
    END;
  END;
END add_cur_cell;

(* set_cur_cell *)

PROCEDURE (VAR rca: rasterizer_cells_aa) set_cur_cell*(x, y: bas.int32);
BEGIN
  IF (rca.cur_cell.x # x) OR (rca.cur_cell.y # y) THEN
    rca.add_cur_cell();

    rca.cur_cell.x := x;
    rca.cur_cell.y := y;

    rca.cur_cell.cover := 0;
    rca.cur_cell.area  := 0;
  END;
END set_cur_cell;

(* DEBUG 

PROCEDURE (VAR rca: rasterizer_cells_aa) cells_output*(name: ARRAY OF CHAR);
VAR
  f: SeqFile.ChanId;
  res: SeqFile.OpenResults;
  i, j: bas.int32;
  ic: cell_ptr;
  x: ARRAY 50 OF CHAR;
BEGIN
  SeqFile.OpenWrite(f, name, SeqFile.raw + SeqFile.old, res);
  IF res = SeqFile.fileExists THEN
    SeqFile.Rewrite(f);
  END;
  FOR i := 0 TO rca.num_cells-1 DO
    FOR j := 0 TO LEN(x)-1 DO
      x[j] := ' ';
    END;
    ic := rca.sorted_cells^[i];
    FormStr.print(x, "%d %d %d %d",
      rca.cells^[ic.A][ic.B].x,
      rca.cells^[ic.A][ic.B].y,
      rca.cells^[ic.A][ic.B].cover,
      rca.cells^[ic.A][ic.B].area);
    x[48] := 15C;
    x[49] := 12C;
    RawIO.Write(f, x);
  END;
  SeqFile.Close(f);
END cells_output; 
*)

(* swap_cell *)

PROCEDURE (VAR rca: rasterizer_cells_aa) swap_cells(i, j: bas.int32);
VAR
  t: cell_ptr;
BEGIN
  t := rca.sorted_cells^[i];
  rca.sorted_cells^[i] := rca.sorted_cells^[j];
  rca.sorted_cells^[j] := t;
END swap_cells;

PROCEDURE (VAR rca: rasterizer_cells_aa) total_cells*(): bas.int32;
BEGIN
  RETURN rca.num_cells;
END total_cells;

PROCEDURE (VAR rca: rasterizer_cells_aa) qsort_cells(start, num: bas.int32);
VAR
  low, high: ARRAY 40 OF bas.int32;
  i, j, x, pivot, s, L, R: bas.int32;
  t, jc, ic, pc: cell_ptr;
BEGIN
  s := 0;
  low[0]  := start;
  high[0] := start + num - 1;
  REPEAT (*take top request from stack*)
    L := low[s];
    R := high[s];

    DEC(s);
    REPEAT (*partition a[L] ... a[R]*)
      i := L;
      j := R;
      pivot := (L+R) DIV 2;

      jc := rca.sorted_cells^[j];
      ic := rca.sorted_cells^[i];
      pc := rca.sorted_cells^[pivot];

      x := rca.cells^[pc.A][pc.B].x;

      REPEAT

        WHILE rca.cells^[ic.A][ic.B].x < x DO
          INC(i);
          ic := rca.sorted_cells^[i];
        END;

        WHILE x < rca.cells^[jc.A][jc.B].x DO
          DEC(j);
          jc := rca.sorted_cells^[j];
        END;

        IF i <= j THEN
          t := rca.sorted_cells^[i];
          rca.sorted_cells^[i] := rca.sorted_cells^[j];
          rca.sorted_cells^[j] := t;

          INC(i);
          DEC(j);

      	  (*
            Здесь была ошибка, связанная с непоследовательностью действий по обработке.
            В рамках всей процедуры qsort производилась манипуляция с индексами, но не массива,
            а манипуляции с индексами в дополнительном массиве.
            во всем коде вслед за изменением i и j производилось обновление "индексов" 
            jc и ic. А ниже код был пропущен. Поэтому и не получалось отсортировать массив как полагается.
            Печально, но на поиск этой ошибки ушел почти месяц.
          *)

          IF j >= 0            THEN jc := rca.sorted_cells^[j]; END;
          IF i < rca.num_cells THEN ic := rca.sorted_cells^[i]; END;
        END
      UNTIL i > j;
      IF i < R THEN (*stack request to sort right partition*)
        INC(s);
        low[s] := i;
        high[s] := R
      END;
      R := j (*now L and R delimit the left partition*)
    UNTIL L >= R
  UNTIL s = -1;
END qsort_cells;

PROCEDURE (VAR rca: rasterizer_cells_aa) sort_cells*();
VAR
  nb, i,
  v, yidx, scidx,
  start: bas.int32;

  cell_array : cell_aa_array_ptr;
  cell_idx   : bas.int32;
BEGIN

(* Perform sort only the first time *)
  IF rca.sorted THEN
    RETURN;
  END;

  rca.add_cur_cell();

  IF rca.num_cells = 0 THEN
    RETURN;
  END;

(* Allocate the array of cell pointers *)
  NEW(rca.sorted_cells, rca.num_cells);

(* Allocate and zero the Y array *)
  NEW(rca.sorted_y, rca.max_y - rca.min_y + 1);

(* Create the Y-histogram (count the numbers of cells for each Y) *)

  (* Получаем количество блоков (количество массивов cell) *)
  nb := bas.shr_int32(rca.num_cells, cell_block_shift);
  i := 0;
  (* Если nb получается больше 0, это означает, что первые nb-1 элементов массива
     заполнены полностью, и можно не оглядываясь на индексы перебрать их полностью. *)

  (* Цикл по количеству блоков *)
  WHILE i < nb DO
    cell_array := rca.cells^[i];

    cell_idx := cell_block_size;

    (* Перебираем все cells в массиве *)
    WHILE cell_idx > 0 DO
      DEC(cell_idx);
      INC(rca.sorted_y[cell_array^[cell_idx].y - rca.min_y].start);
    END;
    INC(i);
  END;

  (* Если же оказывается, что nb = 0 мы пробегаем по самому первому массиву *)
  cell_array := rca.cells^[nb];

  cell_idx := bit.and32(rca.num_cells, cell_block_mask);

  WHILE cell_idx > 0 DO
    DEC(cell_idx);
    INC(rca.sorted_y[cell_array^[cell_idx].y - rca.min_y].start);
  END;

(* Convert the Y-histogram into the array of starting indexes *)
  start := 0;

  FOR i := 0 TO LEN(rca.sorted_y^) - 1 DO
   v := rca.sorted_y[i].start;
   rca.sorted_y[i].start := start;
   INC(start, v);
  END;

(* Fill the cell pointer array sorted by Y *)

  nb := bas.shr_int32(rca.num_cells, cell_block_shift);
  i := 0;

  WHILE i < nb DO
    cell_array := rca.cells^[i];

    cell_idx := cell_block_size;

    WHILE cell_idx > 0 DO
      DEC(cell_idx);
      (* Индекс в массиве sorted_y (количество ячеек на данный Y) *)
      yidx := cell_array^[cell_idx].y - rca.min_y;
      (* Индекс в массиве сортированных ячеек *)
      scidx := rca.sorted_y[yidx].start + rca.sorted_y[yidx].num;
      (* Указатель на ячейку (массив (А) и ячейка (B)) *)
      rca.sorted_cells^[scidx].A := i;
      rca.sorted_cells^[scidx].B := cell_idx;

      INC(rca.sorted_y[yidx].num);
    END;
    (* следующий массив *)
    INC(i);
  END;

  cell_array := rca.cells^[nb];

  cell_idx := bit.and32(rca.num_cells, cell_block_mask);

  WHILE cell_idx > 0 DO
    DEC(cell_idx);
    (* Индекс в массиве sorted_y (количество ячеек на данный Y) *)
    yidx := cell_array^[cell_idx].y - rca.min_y;
    (* Индекс в массиве сортированных ячеек *)
    scidx := rca.sorted_y[yidx].start + rca.sorted_y[yidx].num;
    (* Указатель на ячейку (массив (А) и ячейка (B)) *)
    rca.sorted_cells^[scidx].A := nb;
    rca.sorted_cells^[scidx].B := cell_idx;

    INC(rca.sorted_y[yidx].num);
  END;

(* DEBUG 
  rca.cells_output("sorted.no");
*)
(* Finally arrange the X-arrays *)
  FOR i := 0 TO LEN(rca.sorted_y^) - 1 DO
    (*STextIO.WriteString("array: ");
    SWholeIO.WriteInt(i, 4);
    STextIO.WriteLn(); *)
    IF rca.sorted_y^[i].num > 0 THEN
      rca.qsort_cells(rca.sorted_y^[i].start, rca.sorted_y^[i].num);
    END;
  END;
(* DEBUG 
  rca.cells_output("sorted.yes");
*)
  rca.sorted := TRUE;
END sort_cells;

(* scanline_num_cells *)

PROCEDURE (VAR rca: rasterizer_cells_aa) scanline_num_cells*(y: bas.int32): bas.int32;
BEGIN
  RETURN rca.sorted_y[y - rca.min_y].num;
END scanline_num_cells;

(* scanline_start_cells *)

PROCEDURE (VAR rca: rasterizer_cells_aa) scanline_start_cells*(y: bas.int32): bas.int32;
BEGIN
  RETURN rca.sorted_y[y - rca.min_y].start;
END scanline_start_cells;

PROCEDURE (VAR rca: rasterizer_cells_aa) move_to*(x, y: bas.int32);
BEGIN
  IF rca.sorted THEN
    rca.reset;
  END;

  rca.set_cur_cell(bas.shr_int32(x, poly_base_shift), bas.shr_int32(y, poly_base_shift));

  rca.cur_x := x;
  rca.cur_y := y;
END move_to;

(* render_hline *)

PROCEDURE (VAR rca: rasterizer_cells_aa) render_hline*(ey, x1, y1, x2, y2: bas.int32);
VAR
  p, dx,
  ex1,
  ex2,
  fx1,
  fx2,

  delta,
  first,
  incr,
  lift,
  m_d,
  rem: bas.int32;
BEGIN
  ex1 := bas.shr_int32(x1, poly_base_shift);
  ex2 := bas.shr_int32(x2, poly_base_shift);

  fx1 := bit.and32(x1, poly_base_mask);
  fx2 := bit.and32(x2, poly_base_mask);

 (* trivial case. Happens often *)
  IF y1 = y2 THEN
    rca.set_cur_cell(ex2, ey);
    RETURN;
  END;

(* everything is located in a single cell. That is easy! *)
  IF ex1 = ex2 THEN
    delta := y2 - y1;

    INC(rca.cur_cell.cover, delta);
    INC(rca.cur_cell.area, (fx1 + fx2) * delta);

    RETURN;

  END;

  (* ok, we'll have to render a run of adjacent cells on the same
     hline... *)
  p     := (poly_base_size - fx1) * (y2 - y1);
  first := poly_base_size;
  incr  := 1;
  dx    := x2 - x1;

  IF dx < 0 THEN
    p     := fx1 * (y2 - y1);
    first := 0;
    incr  := -1;
    dx    := -dx;
  END;

  delta := p DIV dx;
  m_d   := p MOD dx;

  IF m_d < 0 THEN
    DEC(delta);
    INC(m_d, dx);
  END;

  INC(rca.cur_cell.cover, delta);
  INC(rca.cur_cell.area, (fx1 + first) * delta);

  INC(ex1, incr);

  rca.set_cur_cell(ex1, ey);

  INC(y1, delta);

  IF ex1 # ex2 THEN
    p    := poly_base_size * (y2 - y1 + delta);
    lift := p DIV dx;
    rem  := p MOD dx;

    IF rem < 0 THEN
      DEC(lift);
      INC(rem, dx);
    END;

    DEC(m_d, dx);

    WHILE ex1 # ex2 DO
      delta := lift;

      INC(m_d, rem);

      IF m_d >= 0 THEN

       DEC(m_d, dx);
       INC(delta);

      END;

      INC(rca.cur_cell.cover, delta);
      INC(rca.cur_cell.area, (poly_base_size) * delta);
      INC(y1, delta);
      INC(ex1, incr);

      rca.set_cur_cell(ex1, ey);
    END;
  END;

 delta := y2 - y1;

 INC(rca.cur_cell.cover, delta);
 INC(rca.cur_cell.area, (fx2 + poly_base_size - first) * delta);
END render_hline;

PROCEDURE (VAR rca: rasterizer_cells_aa) render_line*(x1, y1, x2, y2: bas.int32);
VAR
  p ,
  cx, cy, dx, dy, ex,
  ey1, ey2, fy1, fy2, rem, m_d,
  x_from, x_to, lift, delta, first, incr, two_fx, area: bas.int32;

CONST
  dx_limit = ASH(16384, poly_base_shift);

BEGIN
  dx := x2 - x1;

  IF (dx >= dx_limit ) OR
     (dx <= -dx_limit ) THEN
    cx := bas.shr_int32((x1 + x2), 1);
    cy := bas.shr_int32((y1 + y2), 1);

    rca.render_line(x1, y1, cx, cy);
    rca.render_line(cx, cy, x2, y2);
  END;

  dy := y2 - y1;

  ey1 := bas.shr_int32(y1, poly_base_shift);
  ey2 := bas.shr_int32(y2, poly_base_shift);

  fy1 := bit.and32(y1, poly_base_mask);
  fy2 := bit.and32(y2, poly_base_mask);

 (* everything is on a single hline *)
  IF ey1 = ey2 THEN
    rca.render_hline(ey1, x1, fy1, x2, fy2);
    RETURN;
  END;

(*
  Vertical line - we have to calculate start and end cells,
  and then - the common values of the area and coverage for
  all cells of the line. We know exactly there's only one
  cell, so, we don't have to call render_hline().
*)

  incr := 1;

  IF dx = 0 THEN

    ex := bas.shr_int32(x1, poly_base_shift);

    two_fx := ASH((x1 - ASH(ex, poly_base_shift)), 1);
    first := poly_base_size;

    IF dy < 0 THEN
      first := 0;
      incr  := -1;
    END;

    delta := first - fy1;

    INC(rca.cur_cell.cover, delta);
    INC(rca.cur_cell.area, two_fx * delta);
    INC(ey1, incr);

    rca.set_cur_cell(ex, ey1);

    delta := first + first - poly_base_size;
    area := two_fx * delta;

    WHILE ey1 <> ey2 DO

      rca.cur_cell.cover := delta;
      rca.cur_cell.area  := area;

      INC(ey1 ,incr );

      rca.set_cur_cell(ex, ey1);

    END;

    delta := fy2 - poly_base_size + first;

    INC(rca.cur_cell.cover, delta);
    INC(rca.cur_cell.area, two_fx * delta);

    RETURN;
  END;

(* ok, we have to render several hlines *)
  p     := (poly_base_size - fy1) * dx;
  first := poly_base_size;

  IF dy < 0 THEN
    p     := fy1 * dx;
    first := 0;
    incr  := -1;
    dy    := -dy;
  END;

  delta := p DIV dy;
  m_d   := p MOD dy;

  IF m_d < 0 THEN
    DEC(delta);
    INC(m_d, dy);
  END;

  x_from := x1 + delta;

  rca.render_hline(ey1, x1, fy1, x_from, first);

  INC(ey1, incr);

  rca.set_cur_cell(bas.shr_int32(x_from, poly_base_shift), ey1);

  IF ey1 # ey2 THEN

    p    := poly_base_size * dx;
    lift := p DIV dy;
    rem  := p MOD dy;

    IF rem < 0 THEN
      DEC(lift);
      INC(rem, dy);
    END;

    DEC(m_d, dy);

    WHILE ey1 # ey2 DO
      delta := lift;
      INC(m_d, rem);

      IF m_d >= 0 THEN
        DEC(m_d, dy);
        INC(delta);
      END;

     x_to := x_from + delta;

     rca.render_hline(ey1, x_from, poly_base_size - first, x_to, first);

     x_from := x_to;

     INC(ey1, incr);
     rca.set_cur_cell(bas.shr_int32(x_from, poly_base_shift), ey1);
    END;
  END;

  rca.render_hline(ey1, x_from, poly_base_size - first, x2, fy2);
END render_line;

PROCEDURE (VAR rca: rasterizer_cells_aa) line_to*(x, y: bas.int32);
BEGIN
  rca.render_line(rca.cur_x, rca.cur_y, x, y);
  rca.cur_x := x;
  rca.cur_y := y;
  rca.sorted := FALSE;
END line_to;


(*===================*)
(* scanline_hit_test *)

PROCEDURE (VAR sht: scanline_hit_test) ConstructX*(x: bas.int32);
BEGIN
  sht.x := x;
  sht.hit := FALSE;
END ConstructX;

<*+WOFF301*>
PROCEDURE (VAR sht: scanline_hit_test) add_cell*(x: bas.int32; cover: bas.int8u);
BEGIN
  IF sht.x = x THEN
    sht.hit := TRUE;
  END;
END add_cell;

PROCEDURE (VAR sht: scanline_hit_test) add_span*(x, len: bas.int32; cover: bas.int8u);
BEGIN
  IF (sht.x >= x ) & (sht.x < x + len ) THEN
    sht.hit := TRUE;
  END;
END add_span;
<*-WOFF301*>

PROCEDURE (VAR sht: scanline_hit_test) num_spans*(): bas.int32;
BEGIN
  RETURN 1;
END num_spans;

END AggRasterizerCellsAA.
