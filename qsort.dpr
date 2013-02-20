program qsort;
{$APPTYPE Console}

VAR
  a: array [0..19] of integer = (888, 104, 213, 212, 211, 210, 109, 108, 107, 106, 210, 209, 208, 207, 206, 113, 112, 111, 110, 999);

PROCEDURE outarray();
VAR
  i: integer;
BEGIN
  i := 0;
  WHILE i < length(a) DO
    BEGIN
      Writeln(a[i]);
      inc(i)
    END;
  Writeln;
END;

PROCEDURE NonRecursiveQuickSort(start, num: integer);
CONST
  M = 80;
VAR
  i, j, L, R, s: integer;
  x, w: integer;
  low, high: ARRAY [0..M-1] OF integer; (*index stack*)
BEGIN
  s := 0; low[0] := start;
  high[0] := start + num - 1;

  REPEAT (*take top request from stack*)
    L := low[s];
    R := high[s]; DEC(s);

    REPEAT (*partition a[L] ... a[R]*)
      i := L;
      j := R;
      x := a[(L+R) DIV 2];

      REPEAT
        WHILE a[i] < x DO
          INC(i);

        WHILE x < a[j] DO
          DEC(j);
        IF i <= j THEN
        BEGIN
          w := a[i];
          a[i] := a[j];
          a[j] := w;
          INC(i);
          DEC(j);
        END;
      UNTIL i > j;

      IF i < R THEN (*stack request to sort right partition*)
      BEGIN
        INC(s);
        low[s] := i;
        high[s] := R
      END;
      R := j (*now L and R delimit the left partition*)
    UNTIL L >= R
  UNTIL s = -1
END;

BEGIN
  outarray();
  NonRecursiveQuickSort(10, 10);
  outarray();
END.
