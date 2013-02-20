MODULE AggColor;

IMPORT
  bas := AggBasics,
  bit := AggBit;

TYPE
  (** Цвет пикселя *)
  (** Цвет пикселя1 **)
  (** Pixel color *)
  (** Pixel color1 **)
  aggclr_ptr* = POINTER TO aggclr;
  aggclr* = RECORD
    v*, r*, g*, b*, a*: bas.int8u;
  END;

  rgba8* = RECORD
    r*, g*, b*, a*: bas.int8u;
  END;

  colors_array_ptr* = POINTER TO colors_array;
  colors_array*     = ARRAY OF aggclr;

  covers_array_ptr* = POINTER TO covers_array;
  covers_array*     = ARRAY OF bas.int8u;

CONST
 base_shift* = 8;
 base_size*  = ASH(1, base_shift);
 base_mask*  = base_size - 1;

PROCEDURE (VAR r: rgba8) no_color*();
BEGIN
  r.r := 0;
  r.g := 0;
  r.b := 0;
  r.a := 0;
END no_color;


PROCEDURE (VAR c: aggclr) ConstructI*();
BEGIN
  c.v := 0;
  c.r := 0;
  c.g := 0;
  c.b := 0;
  c.a := 0;
END ConstructI;

PROCEDURE (VAR c: aggclr) ConstructRGBA*(VAR rgba: rgba8);
BEGIN
 c.v := bas.agg_getbyte(ASH((LONG(rgba.r) * 77 + LONG(rgba.g) * 150 + LONG(rgba.b) * 29), -base_shift));
 c.r := rgba.r;
 c.g := rgba.g;
 c.b := rgba.b;
 c.a := rgba.a;
END ConstructRGBA;

PROCEDURE (VAR c: aggclr) ConstructFrom*(VAR color: aggclr);
BEGIN
  c.v := color.v;
  c.r := color.r;
  c.g := color.g;
  c.b := color.b;
  c.a := color.a;
END ConstructFrom;

PROCEDURE (VAR c: aggclr) ConstructV*(v, a: bas.int8u);
BEGIN
  c.v := v;
  c.r := 0;
  c.g := 0;
  c.b := 0;
  c.a := a;
END ConstructV;

PROCEDURE (VAR c: aggclr) Constructrgba*(r,g,b,a: bas.int8u);
BEGIN
  c.v := bas.agg_getbyte(ASH((LONG(r) * 77 + LONG(g) * 150 + LONG(b) * 29), -base_shift));
  c.r := r;
  c.g := g;
  c.b := b;
  c.a := a;
END Constructrgba;

PROCEDURE (VAR c: aggclr) ConstructDbl*(r, g, b, a: bas.double);
BEGIN
  ASSERT((0 <= r) & (r <= 1.0));
  ASSERT((0 <= g) & (g <= 1.0));
  ASSERT((0 <= b) & (b <= 1.0));
  ASSERT((0 <= a) & (a <= 1.0));
  c.v := bas.agg_getbyte(ENTIER((0.299 * r + 0.587 * g + 0.114 * b ) * base_mask));
  c.r := bas.agg_getbyte(ENTIER(r * base_mask));
  c.g := bas.agg_getbyte(ENTIER(g * base_mask));
  c.b := bas.agg_getbyte(ENTIER(b * base_mask));
  c.a := bas.agg_getbyte(ENTIER(a * base_mask));
END ConstructDbl;

PROCEDURE (VAR c: aggclr) ConstructPre*(r, g, b, a: bas.double);
BEGIN
  ASSERT((0 <= r) & (r <= 1.0));
  ASSERT((0 <= g) & (g <= 1.0));
  ASSERT((0 <= b) & (b <= 1.0));
  ASSERT((0 <= a) & (a <= 1.0));

  c.ConstructDbl(r * a, g * a, b * a, a);
END ConstructPre;

PROCEDURE (VAR c: aggclr) ConstructMix*(r, g, b: bas.int8u; a: bas.double);
BEGIN
  ASSERT((0 <= a) & (a <= 1.0));
  c.v := bas.agg_getbyte(ASH((LONG(r) * 77 + LONG(g) * 150 + LONG(b) * 29), -base_shift));
  c.r := r;
  c.g := g;
  c.b := b;
  c.a := bas.agg_getbyte(ENTIER(a * base_mask));
END ConstructMix;

PROCEDURE (VAR c: aggclr) Opacity*(): bas.double;
BEGIN
  RETURN c.a / base_mask;
END Opacity;

PROCEDURE (VAR c: aggclr) SetOpacity*(o: bas.double);
BEGIN
  c.a := bas.agg_getbyte(ENTIER(o * base_mask));
END SetOpacity;

PROCEDURE (VAR c: aggclr) Clear*();
BEGIN
  c.v := 0;
  c.r := 0;
  c.g := 0;
  c.b := 0;
  c.a := 0;
END Clear;

PROCEDURE (VAR c: aggclr) ConstructWave*(wl, gamma: bas.double);
VAR
  r,g,b,s: bas.double;
BEGIN
  r := 0;
  g := 0;
  b := 0;
  IF (wl >= 380.0) & (wl <= 440.0) THEN

    r := -1.0 * (wl - 440.0) / (440.0 - 380.0);
    b := 1.0;

  ELSIF (wl >= 440.0) & (wl <= 490.0) THEN

    g := (wl - 440.0) / (490.0 - 440.0);
    b := 1.0;

  ELSIF (wl >= 490.0) & (wl <= 510.0) THEN

    g := 1.0;
    b := -1.0 * (wl - 510.0) / (510.0 - 490.0);

  ELSIF (wl >= 510.0) & (wl <= 580.0) THEN

    r := (wl - 510.0) / (580.0 - 510.0);
    g := 1.0;

  ELSIF (wl >= 580.0) & (wl <= 645.0) THEN

    r := 1.0;
    g := -1.0 * (wl - 645.0) / (645.0 - 580.0);

  ELSIF  (wl >= 645.0) & (wl <= 780.0) THEN

    r := 1.0;

  END;

  s := 1.0;

  IF (wl > 700.0) THEN
    s := 0.3 + 0.7 * (780.0 - wl) / (780.0 - 700.0)
  ELSIF (wl < 420.0) THEN
    s := 0.3 + 0.7 * (wl - 380.0) / (420.0 - 380.0);
  END;

  r := bas.agg_power((r * s), gamma);
  g := bas.agg_power((g * s), gamma);
  b := bas.agg_power((b * s), gamma);

  c.ConstructDbl(r, g, b, 1.0);
END ConstructWave;

PROCEDURE (VAR c: aggclr) Gradient*(color: aggclr; k: bas.double);
VAR
  ik: bas.int32u;
BEGIN
  ik  := ENTIER(k * base_size);

  c.r := SHORT(c.r + SHORT(SHORT(ASH(((color.r - c.r) * ik ), -base_shift))));
  c.g := SHORT(c.g + SHORT(SHORT(ASH(((color.g - c.g) * ik ), -base_shift))));
  c.b := SHORT(c.b + SHORT(SHORT(ASH(((color.b - c.b) * ik ), -base_shift))));
  c.a := SHORT(c.a + SHORT(SHORT(ASH(((color.a - c.a) * ik ), -base_shift))));
END Gradient;

PROCEDURE (VAR c: aggclr) Gradient8*(color: aggclr; k: bas.double; VAR rgba: rgba8);
VAR
  ik: bas.int32u;
BEGIN
  ik  := ENTIER(k * base_size);

  rgba.r := SHORT(c.r + SHORT(SHORT(ASH(((color.r - c.r) * ik ), -base_shift))));
  rgba.g := SHORT(c.g + SHORT(SHORT(ASH(((color.g - c.g) * ik ), -base_shift))));
  rgba.b := SHORT(c.b + SHORT(SHORT(ASH(((color.b - c.b) * ik ), -base_shift))));
  rgba.a := SHORT(c.a + SHORT(SHORT(ASH(((color.a - c.a) * ik ), -base_shift))));
END Gradient8;

PROCEDURE rgba8_packed*(packed: bas.int32u; VAR rgba: rgba8);
BEGIN
  rgba.r := SHORT(SHORT(bit.and32u(bit.lsh32u(packed, -16), 0FFH)));
  rgba.g := SHORT(SHORT(bit.and32u(bit.lsh32u(packed,  -8), 0FFH)));
  rgba.b := SHORT(SHORT(bit.and32u(packed, 0FFH)));
  rgba.a := base_mask;
END rgba8_packed;

END AggColor.
