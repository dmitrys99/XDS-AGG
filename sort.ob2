PROCEDURE NonRecursiveQuickSort;
CONST
  M = 12;
VAR
  i, j, L, R, s: INTEGER;
  x, w: Item;
  low, high: ARRAY M OF INTEGER; (*index stack*)
BEGIN
  s := 0;
  low[0] := 0;
  high[0] := n-1;
  REPEAT
    (*take top request from stack*)
    L := low[s];
    R := high[s];
    DEC(s);
    REPEAT (*partition a[L] ... a[R]*)

      i := L;
      j := R;
      x := a[(L+R) DIV 2];
      REPEAT
        WHILE a[i] < x DO
          INC(i)
        END;
        WHILE x < a[j] DO
          DEC(j)
        END;
        IF i <= j THEN
          w := a[i];
          a[i] := a[j];
          a[j] := w;
          i := i+1;
          j := j-1
        END
      UNTIL i > j;
      IF i < R THEN (*stack request to sort right partition*)
        INC(s);
        low[s] := i;
        high[s] := R
      END;
      R := j (*now L and R delimit the left partition*)
    UNTIL L >= R
  UNTIL s = 0
END NonRecursiveQuickSort