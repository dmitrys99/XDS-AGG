<*+O2EXTENSIONS*>
<*+WOFF311*>
(* ETH Oberon, Copyright 2001 ETH Zuerich Institut fuer Computersysteme, ETH Zentrum, CH-8092 Zuerich.
Refer to the "General ETH Oberon System Source License" contract available at: http://www.oberon.ethz.ch/ *)

MODULE AggBit; (** portable *) (* tk 12.2.96 *)
(** AUTHOR "tk"; PURPOSE "Bit manipulation"; *)

IMPORT
  S := SYSTEM,
  bas := AggBasics;

(*  TYPE
    SHORTCARD* = SHORTINT;
    CARDINAL* = INTEGER;
    LONGCARD* = LONGINT;*)
  CONST
    rbo = FALSE;  (* reverse bit ordering, e.g. PowerPC*)
    risc = FALSE; (* risc architecture - no support for 8 and 16-bit rotations *)

  (** bitwise exclusive or: x XOR y *)
  PROCEDURE xor8*(x, y: bas.int8): bas.int8;
  BEGIN
    RETURN S.VAL(bas.int8, S.VAL(SET, x) / S.VAL(SET, y));
  END xor8;

  PROCEDURE xor8u*(x, y: bas.int8u): bas.int8u;
  BEGIN
    RETURN S.VAL(bas.int8u, S.VAL(SET, x) / S.VAL(SET, y));
  END xor8u;

  PROCEDURE xor16*(x, y: bas.int16): bas.int16;
  BEGIN
    RETURN S.VAL(bas.int16, S.VAL(SET, x) / S.VAL(SET, y));
  END xor16;

  PROCEDURE xor16u*(x, y: bas.int16u): bas.int16u;
  BEGIN
    RETURN S.VAL(bas.int16u, S.VAL(SET, x) / S.VAL(SET, y));
  END xor16u;

  PROCEDURE xor32*(x, y: bas.int32): bas.int32;
  BEGIN
    RETURN S.VAL(bas.int32, S.VAL(SET, x) / S.VAL(SET, y));
  END xor32;

  PROCEDURE xor32u*(x, y: bas.int32u): bas.int32u;
  BEGIN
    RETURN S.VAL(bas.int32u, S.VAL(SET, x) / S.VAL(SET, y));
  END xor32u;


  (** bitwise or: x OR y *)
  PROCEDURE or8*(x, y: bas.int8): bas.int8;
  BEGIN
    RETURN S.VAL(bas.int8, S.VAL(SET, x) + S.VAL(SET, y));
  END or8;

  PROCEDURE or8u*(x, y: bas.int8u): bas.int8u;
  BEGIN
    RETURN S.VAL(bas.int8u, S.VAL(SET, x) + S.VAL(SET, y));
  END or8u;

  PROCEDURE or16*(x, y: bas.int16): bas.int16;
  BEGIN
    RETURN S.VAL(bas.int16, S.VAL(SET, x) + S.VAL(SET, y));
  END or16;

  PROCEDURE or16u*(x, y: bas.int16u): bas.int16u;
  BEGIN
    RETURN S.VAL(bas.int16u, S.VAL(SET, x) + S.VAL(SET, y));
  END or16u;

  PROCEDURE or32*(x, y: bas.int32): bas.int32;
  BEGIN
    RETURN S.VAL(bas.int32, S.VAL(SET, x) + S.VAL(SET, y));
  END or32;

  PROCEDURE or32u*(x, y: bas.int32u): bas.int32u;
  BEGIN
    RETURN S.VAL(bas.int32u, S.VAL(SET, x) + S.VAL(SET, y));
  END or32u;


  (** bitwise and: x AND y *)
  PROCEDURE and8*(x, y: bas.int8): bas.int8;
  BEGIN
    RETURN S.VAL(bas.int8, S.VAL(SET, x) * S.VAL(SET, y));
  END and8;

  PROCEDURE and8u*(x, y: bas.int8u): bas.int8u;
  BEGIN
    RETURN S.VAL(bas.int8u, S.VAL(SET, x) * S.VAL(SET, y));
  END and8u;

  PROCEDURE and16*(x, y: bas.int16): bas.int16;
  BEGIN
    RETURN S.VAL(bas.int16, S.VAL(SET, x) * S.VAL(SET, y));
  END and16;

  PROCEDURE and16u*(x, y: bas.int16u): bas.int16u;
  BEGIN
    RETURN S.VAL(bas.int16u, S.VAL(SET, x) * S.VAL(SET, y));
  END and16u;

  PROCEDURE and32*(x, y: bas.int32): bas.int32;
  BEGIN
    RETURN S.VAL(bas.int32, S.VAL(SET, x) * S.VAL(SET, y));
  END and32;

  PROCEDURE and32u*(x, y: bas.int32u): bas.int32u;
  BEGIN
    RETURN S.VAL(bas.int32u, S.VAL(SET, x) * S.VAL(SET, y));
  END and32u;

  (** bitwise not: NOT x *)
  PROCEDURE not8*(x: bas.int8): bas.int8;
  BEGIN
    RETURN S.VAL(bas.int8, {0..31} - S.VAL(SET, x));
  END not8;

  PROCEDURE not8u*(x: bas.int8u): bas.int8u;
  BEGIN
    RETURN S.VAL(bas.int8u, {0..31} - S.VAL(SET, x));
  END not8u;

  PROCEDURE not16*(x: bas.int16): bas.int16;
  BEGIN
    RETURN S.VAL(bas.int16, {0..31} - S.VAL(SET, x));
  END not16;

  PROCEDURE not16u*(x: bas.int16u): bas.int16u;
  BEGIN
    RETURN S.VAL(bas.int16u, {0..31} - S.VAL(SET, x));
  END not16u;

  PROCEDURE not32*(x: bas.int32): bas.int32;
  BEGIN
    RETURN S.VAL(bas.int32, {0..31} - S.VAL(SET, x));
  END not32;

  PROCEDURE not32u*(x: bas.int32u): bas.int32u;
  BEGIN
    RETURN S.VAL(bas.int32u, {0..31} - S.VAL(SET, x));
  END not32u;



  PROCEDURE lsh8*(x: bas.int8; n: bas.int32): bas.int8;
  BEGIN
    RETURN S.LSH(x, n);
  END lsh8;

  PROCEDURE lsh8u*(x: bas.int8u; n: bas.int32): bas.int8u;
  BEGIN
    RETURN S.VAL(bas.int8u, S.LSH(S.VAL(S.BYTE, x), n));
  END lsh8u;

  PROCEDURE lsh32u*(x: bas.int32u; n: bas.int32): bas.int32u;
  BEGIN
    RETURN S.VAL(bas.int32u, S.LSH(S.VAL(bas.int32, S.VAL(SET, x)), n));
  END lsh32u;


  (** bitwise logical left-shift: x shifted n *)
(*  PROCEDURE CLSH*(x: CHAR; n: SHORTINT): CHAR;
  BEGIN
    IF risc THEN RETURN CHR(S.LSH(S.VAL(LONGINT, S.VAL(SET, ORD(x)) * S.VAL(SET, 0FFH)), n))
    ELSE RETURN S.LSH(x, n) END
  END CLSH;

  PROCEDURE SLSH*(x: SHORTINT; n: SHORTINT): SHORTINT;
  BEGIN
    IF risc THEN RETURN SHORT(SHORT(S.LSH(S.VAL(LONGINT, S.VAL(SET, LONG(LONG(x))) * S.VAL(SET, 0FFH)), n)))
    ELSE RETURN S.LSH(x, n) END
  END SLSH;

  PROCEDURE ILSH*(x: INTEGER; n: SHORTINT): INTEGER;
  BEGIN
    IF risc THEN RETURN SHORT(S.LSH(S.VAL(LONGINT, S.VAL(SET, LONG(x)) * S.VAL(SET, 0FFFFH)), n))
    ELSE RETURN S.LSH(x, n) END
  END ILSH;

  PROCEDURE LLSH*(x: LONGINT; n: SHORTINT): LONGINT;
  BEGIN RETURN S.LSH(x, n)
  END LLSH;
*)
(*
  ** bitwise rotation: x rotatated by n bits *
  PROCEDURE CROT*(x: CHAR; n: SHORTINT): CHAR;
    VAR s0, s1: SET; i: INTEGER;
  BEGIN
    IF risc THEN
      s0 := S.VAL(SET, ORD(x)); s1 := {};
      IF rbo THEN
        i := 0; WHILE i < 8 DO
          IF 31-i IN s0 THEN INCL(s1, 31 - ((i+n) MOD 8)) END;
          INC(i)
        END;
      ELSE
        i := 0; WHILE i < 8 DO
          IF i IN s0 THEN INCL(s1, (i+n) MOD 8) END;
          INC(i)
        END;
      END;
      RETURN CHR(S.VAL(LONGINT, s1))
    ELSE RETURN S.ROT(x, n) END;
  END CROT;

  PROCEDURE SROT*(x: SHORTINT; n: SHORTINT): SHORTINT;
    VAR s0, s1: SET; i: INTEGER;
  BEGIN
    IF risc THEN
      s0 := S.VAL(SET, LONG(LONG(x))); s1 := {};
      IF rbo THEN
        i := 0; WHILE i < 8 DO
          IF 31-i IN s0 THEN INCL(s1, 31 - ((i+n) MOD 8)) END;
          INC(i)
        END;
      ELSE
        i := 0; WHILE i < 8 DO
          IF i IN s0 THEN INCL(s1, (i+n) MOD 8) END;
          INC(i)
        END;
      END;
      RETURN SHORT(SHORT(S.VAL(LONGINT, s1)))
    ELSE RETURN S.ROT(x, n) END;
  END SROT;

  PROCEDURE IROT*(x: INTEGER; n: SHORTINT): INTEGER;
    VAR s0, s1: SET; i: INTEGER;
  BEGIN
    IF risc THEN
      s0 := S.VAL(SET, LONG(x)); s1 := {};
      IF rbo THEN
        i := 0; WHILE i < 16 DO
          IF 31-i IN s0 THEN INCL(s1, 31 - ((i+n) MOD 16)) END;
          INC(i)
        END;
      ELSE
        i := 0; WHILE i < 16 DO
          IF i IN s0 THEN INCL(s1, (i+n) MOD 16) END;
          INC(i)
        END;
      END;
      RETURN SHORT(S.VAL(LONGINT, s1))
    ELSE RETURN S.ROT(x, n) END;
  END IROT;

  PROCEDURE LROT*(x: LONGINT; n: SHORTINT): LONGINT;
  BEGIN RETURN S.ROT(x, n)
  END LROT;

*)
  (** swap bytes to change byteordering *)
(*  PROCEDURE ISWAP*(x: INTEGER): INTEGER;
    TYPE integer = ARRAY 2 OF CHAR; VAR a, b: integer;
  BEGIN a := S.VAL(integer, x); b[0] := a[1]; b[1] := a[0]; RETURN S.VAL(INTEGER, b)
  END ISWAP;

  PROCEDURE LSWAP*(x: LONGINT): LONGINT;
    TYPE longint = ARRAY 4 OF CHAR; VAR a, b: longint;
  BEGIN a := S.VAL(longint, x); b[0] := a[3]; b[1] := a[2]; b[2] := a[1]; b[3] := a[0]; RETURN S.VAL(LONGINT, b)
  END LSWAP;
*)

  (** test bit n in x*)
(*  PROCEDURE CBIT*(x: CHAR; n: SHORTINT): BOOLEAN;
  BEGIN ASSERT((n >= 0) & (n <= 7));
    IF rbo THEN RETURN (31-n) IN S.VAL(SET, ORD(x)) ELSE RETURN n IN S.VAL(SET, LONG(ORD(x))) END
  END CBIT;

  PROCEDURE BIT*(x: LONGINT; n: SHORTINT): BOOLEAN;
  BEGIN ASSERT((n >= 0) & (n <= 31));
    IF rbo THEN RETURN (31-n) IN S.VAL(SET, x) ELSE RETURN n IN S.VAL(SET, x) END
  END BIT;
*)

  (** set bit n in x*)
(*  PROCEDURE CSETBIT*(VAR x: CHAR; n: SHORTINT);
    VAR i: LONGINT;
  BEGIN ASSERT((n >= 0) & (n <= 7));
    i := ORD(x); IF rbo THEN INCL(S.VAL(SET, i), 31-n) ELSE INCL(S.VAL(SET, i), n) END; x := CHR(i)
  END CSETBIT;

  PROCEDURE SSETBIT*(VAR x: SHORTINT; n: SHORTINT);
    VAR i: LONGINT;
  BEGIN ASSERT((n >= 0) & (n <= 7));
    i := LONG(LONG(x)); IF rbo THEN INCL(S.VAL(SET, i), 31-n) ELSE INCL(S.VAL(SET, i), n) END; x := SHORT(SHORT(i))
  END SSETBIT;

  PROCEDURE ISETBIT*(VAR x: INTEGER; n: SHORTINT);
    VAR i: LONGINT;
  BEGIN ASSERT((n >= 0) & (n <= 15));
    i := LONG(x); IF rbo THEN INCL(S.VAL(SET, i), 31-n) ELSE INCL(S.VAL(SET, i), n) END; x := SHORT(i)
  END ISETBIT;

  PROCEDURE LSETBIT*(VAR x: LONGINT; n: SHORTINT);
  BEGIN ASSERT((n >= 0) & (n <= 31));
    IF rbo THEN INCL(S.VAL(SET, x), 31-n) ELSE INCL(S.VAL(SET, x), n) END
  END LSETBIT;
*)

  (** clear bit n in x*)
(*  PROCEDURE CCLRBIT*(VAR x: CHAR; n: SHORTINT);
    VAR i: LONGINT;
  BEGIN ASSERT(ABS(n) < 8);
    i := ORD(x); IF rbo THEN EXCL(S.VAL(SET, i), 31-n) ELSE EXCL(S.VAL(SET, i), n) END; x := CHR(i)
  END CCLRBIT;

  PROCEDURE SCLRBIT*(VAR x: SHORTINT; n: SHORTINT);
    VAR i: LONGINT;
  BEGIN ASSERT(ABS(n) < 8);
    i := LONG(LONG(x)); IF rbo THEN EXCL(S.VAL(SET, i), 31-n) ELSE EXCL(S.VAL(SET, i), n) END; x := SHORT(SHORT(i))
  END SCLRBIT;

  PROCEDURE ICLRBIT*(VAR x: INTEGER; n: SHORTINT);
    VAR i: LONGINT;
  BEGIN ASSERT(ABS(n) < 16);
    i := LONG(x); IF rbo THEN EXCL(S.VAL(SET, i), 31-n) ELSE EXCL(S.VAL(SET, i), n) END; x := SHORT(i)
  END ICLRBIT;

  PROCEDURE LCLRBIT*(VAR x: LONGINT; n: SHORTINT);
  BEGIN IF rbo THEN EXCL(S.VAL(SET, x), 31-n) ELSE EXCL(S.VAL(SET, x), n) END
  END LCLRBIT;
*)

  (** unsigned comparison: x < y *)
(*  PROCEDURE SLESS*(x, y: SHORTCARD): BOOLEAN;
  BEGIN
    RETURN
      S.VAL(LONGINT, S.VAL(SET, LONG(LONG(x))) * S.VAL(SET, 0FFH))
      <
     S.VAL(LONGINT, S.VAL(SET, LONG(LONG(y))) * S.VAL(SET, 0FFH));
  END SLESS;

  PROCEDURE ILESS*(x, y: CARDINAL): BOOLEAN;
  BEGIN
    RETURN
      S.VAL(LONGINT, S.VAL(SET,LONG(x)) * S.VAL(SET, 0FFFFH))
    <
      S.VAL(LONGINT, S.VAL(SET, LONG(y)) * S.VAL(SET, 0FFFFH))
  END ILESS;

  PROCEDURE LLESS*(x, y: LONGCARD): BOOLEAN;
    VAR x0, y0: LONGINT;
  BEGIN x0 := S.LSH(x, -1); y0 := S.LSH(y, -1);
    IF x0 - y0 = 0 THEN RETURN x0 MOD 2 < y0 MOD 2 ELSE RETURN x0 < y0 END
  END LLESS;
*)

  (** unsigned comparison: x <= y *)
(*  PROCEDURE SLESSEQ*(x, y: SHORTCARD): BOOLEAN;
  BEGIN
    RETURN
      S.VAL(LONGINT, S.VAL(SET, LONG(LONG(x))) * S.VAL(SET, 0FFH))
    <=
      S.VAL(LONGINT, S.VAL(SET, LONG(LONG(y))) * S.VAL(SET, 0FFH))
  END SLESSEQ;

  PROCEDURE ILESSEQ*(x, y: CARDINAL): BOOLEAN;
  BEGIN
    RETURN
      S.VAL(LONGINT, S.VAL(SET,LONG(x)) * S.VAL(SET, 0FFFFH))
    <=
      S.VAL(LONGINT, S.VAL(SET, LONG(y)) * S.VAL(SET, 0FFFFH))
  END ILESSEQ;

  PROCEDURE LLESSEQ*(x, y: LONGCARD): BOOLEAN;
    VAR x0, y0: LONGINT;
  BEGIN x0 := S.LSH(x, -1); y0 := S.LSH(y, -1);
    IF x0 - y0 = 0 THEN RETURN x0 MOD 2 <= y0 MOD 2 ELSE RETURN x0 <= y0 END
  END LLESSEQ;
*)

  (** unsigned division: x DIV y *)
(*  PROCEDURE SDIV*(x, y: SHORTCARD): SHORTCARD;
  BEGIN RETURN SHORT(SHORT(S.VAL(LONGINT, S.VAL(SET, LONG(LONG(x))) * S.VAL(SET, 0FFH)) DIV y))
  END SDIV;

  PROCEDURE IDIV*(x, y: CARDINAL): CARDINAL;
  BEGIN RETURN SHORT(S.VAL(LONGINT, S.VAL(SET, LONG(x)) * S.VAL(SET, 0FFFFH))) DIV y;
  END IDIV;

  PROCEDURE LDIV*(x, y: LONGCARD): LONGCARD;
    CONST m = 4.294967296D9;
    VAR x0, y0: LONGREAL;
  BEGIN IF x < 0 THEN x0 := m - x ELSE x0 := x END;
    IF y < 0 THEN y0 := m - y ELSE y0 := y END;
    RETURN ENTIER(x0 / y0)
  END LDIV;
*)
END AggBit.
<*-WOFF311*>
