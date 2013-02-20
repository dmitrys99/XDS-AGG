<* +MAIN *>
<* +O2EXTENSIONS *>
MODULE qsort;

IMPORT
  bas := AggBasics,
  Out, STextIO, SWholeIO;

TYPE
  tarray = ARRAY 36 OF bas.int32;

VAR
  array: tarray;

CONST
  qsort_threshold = 9;

(*PROCEDURE swap_cells(i, j: bas.int32);
VAR
  t: bas.int32;
BEGIN
  t := array[i];
  array[i] := array[j];
  array[j] := t;
END swap_cells;

PROCEDURE qsort_cells(start, num: bas.int32);
VAR
  stack: ARRAY 80 OF bas.int32;
  len, i, j, x, pivot,
  top, limit, base: bas.int32;
BEGIN
  limit := start + num;
  base := start;
  top := 0;
  LOOP
    len := limit - base;

    IF len > qsort_threshold THEN
      pivot := base + len DIV 2;
      swap_cells(base, pivot);

      i := base + 1;
      j := limit - 1;

      IF array[j] < array[i] THEN
        swap_cells(i, j);
      END;

      IF array[base] < array[i] THEN
        swap_cells(base, i);
      END;

      IF array[j] < array[base] THEN
        swap_cells(base, j);
      END;
      LOOP
        x := array[base];

        REPEAT
          INC(i);
        UNTIL array[i] >= x;

        REPEAT
          DEC(j);
        UNTIL x >= array[j];

        IF i > j THEN
          EXIT;
        END;

        swap_cells(i, j);

      END;

      swap_cells(base, j);

      IF (j - base > limit - i) THEN
        stack[top] := base;
        stack[top + 1] := j;
        base := i;
      ELSE
        stack[top] := i;
        stack[top + 1] := limit;
        limit := j;
      END;
      INC(top, 2);
    ELSE
      j := base;
      i := j + 1;

      WHILE i < limit-1 DO
        LOOP
          IF array[j+1] >= array[j] THEN
            EXIT;
          END;

          swap_cells(j + 1, j);

          IF j = base THEN
            EXIT;
          END;

          DEC(j);
        END;
        j := i;
        INC(i);
      END;

      IF (top > 0) THEN
        DEC(top, 2);
        base := stack[top];
        limit := stack[top + 1];
      ELSE
        EXIT;
      END;

    END;
  END;
END qsort_cells;*)
PROCEDURE outarray();
VAR
  i: INTEGER;
BEGIN
  i := 0;
  WHILE i < LEN(array) DO
    SWholeIO.WriteInt(i, 4);
    SWholeIO.WriteInt(array[i], 4);
    STextIO.WriteLn();
    INC(i);
  END;
  STextIO.WriteLn();
END outarray;


PROCEDURE NonRecursiveQuickSort(start, num: bas.int32);
CONST
  M = 40;
VAR
  i, j, L, R, s: bas.int32;
  x, w: bas.int32;
  low, high: ARRAY M OF bas.int32; (*index stack*)
BEGIN
  Out.String("Enter qsort"); Out.Ln();
  Out.Ln();

  s := 0;
  low[0]  := start;
  high[0] := start + num - 1;
  REPEAT (*take top request from stack*)
    L := low[s];
    R := high[s]; 
    Out.String("L="); Out.Int(L,4); Out.Ln();
    Out.String("R="); Out.Int(R,4); Out.Ln();
    DEC(s);

    REPEAT (*partition a[L] ... a[R]*)
      i := L;
      j := R;







      Out.String("i="); Out.Int(i,4); Out.String(" j="); Out.Int(j, 4); Out.String(" pivot="); Out.Int((L+R) DIV 2, 4);Out.Ln();
      x := array[(L+R) DIV 2];

      Out.String("x="); Out.Int(x,4); Out.Ln();

      REPEAT
        WHILE array[i] < x DO
          Out.String("A[i] < x;. A[i]="); Out.Int(array[i], 4); Out.Ln();
          INC(i);

	  Out.String("i="); Out.Int(i,4); Out.Ln();
        END;

        WHILE x < array[j] DO
          Out.String("x < A[j];. A[j]="); Out.Int(array[j], 4); Out.Ln();
          DEC(j);

	  Out.String("j="); Out.Int(j,4); Out.Ln();
        END;

        IF i <= j THEN
          Out.Ln();
          Out.String("i <= j. swap A[i], A[j]"); Out.Ln();
          w := array[i];
          array[i] := array[j];
          array[j] := w;

          INC(i);
          DEC(j);

          Out.String("INC(i); DEC(j);"); Out.Ln(); 
          Out.String("i="); Out.Int(i,4); Out.String(" j="); Out.Int(j, 4); Out.Ln();
        END
      UNTIL i > j;
      IF i < R THEN (*stack request to sort right partition*)
        INC(s);
        low[s] := i;
        high[s] := R
      END;
      R := j (*now L and R delimit the left partition*)
    UNTIL L >= R
  UNTIL s = -1
END NonRecursiveQuickSort;

BEGIN
  array := tarray{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,

 156,
 157,
 158,
 159,
 160,
 189,
 190,
 191,
 192,
 193,
 197,
 196,
 153,
 152,

 0,0,0,0,0,0};
  outarray();
  (*qsort_cells(10, 10);*)
  NonRecursiveQuickSort(16, 14);
  outarray();
END qsort.
