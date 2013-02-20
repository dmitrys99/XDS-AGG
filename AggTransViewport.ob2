MODULE AggTransViewport;

IMPORT
  bas := AggBasics,
  ata := AggTransAffine;

CONST
  aspect_ratio_stretch* = 0;
  aspect_ratio_meet*    = 1;
  aspect_ratio_slice*   = 2;

TYPE
  trans_viewport_ptr* = POINTER TO trans_viewport;
  trans_viewport* = RECORD(ata.trans_affine)
    world_x1,
    world_y1,
    world_x2,
    world_y2,

    device_x1,
    device_y1,
    device_x2,
    device_y2: bas.double;

    aspect-: bas.int32;
    is_valid-: BOOLEAN;

    align_x-,
    align_y-,

    wx1,
    wy1,
    wx2,
    wy2,
    dx1,
    dy1,

    kx,
    ky: bas.double;
  END;

PROCEDURE transform(this: ata.trans_affine_ptr; VAR x, y: bas.double);
BEGIN
  WITH this: trans_viewport_ptr DO
    x := x * this.kx;
    y := y * this.ky;
  END;
END transform;


PROCEDURE inverse_transform(this: ata.trans_affine_ptr; VAR x, y: bas.double);
BEGIN
  WITH this: trans_viewport_ptr DO
    x := (x - this.dx1) / this.kx + this.wx1;
    y := (y - this.dy1) / this.ky + this.wy1;
  END;
END inverse_transform;

PROCEDURE (tv: trans_viewport_ptr) Construct*();
BEGIN
  ASSERT(tv # NIL);
  tv.Construct^();

  tv.transform         := transform;
  tv.inverse_transform := inverse_transform;

  tv.world_x1 := 0;
  tv.world_y1 := 0;
  tv.world_x2 := 1;
  tv.world_y2 := 1;

  tv.device_x1 := 0;
  tv.device_y1 := 0;
  tv.device_x2 := 1;
  tv.device_y2 := 1;

  tv.aspect   := aspect_ratio_stretch;
  tv.is_valid := TRUE;

  tv.align_x := 0.5;
  tv.align_y := 0.5;

  tv.wx1 := 0;
  tv.wy1 := 0;
  tv.wx2 := 1;
  tv.wy2 := 1;
  tv.dx1 := 0;
  tv.dy1 := 0;

  tv.kx := 1;
  tv.ky := 1;
END Construct;

PROCEDURE (tv: trans_viewport_ptr) update*();
CONST
  epsilon = 1.0E-30;
VAR
  d,
  world_x1,
  world_y1,
  world_x2,
  world_y2,
  device_x1,
  device_y1,
  device_x2,
  device_y2: bas.double;
BEGIN
  ASSERT(tv # NIL);
  IF bas.is_equal_eps(tv.world_x1,  tv.world_x2, epsilon) OR
     bas.is_equal_eps(tv.world_y1,  tv.world_y2, epsilon) OR
     bas.is_equal_eps(tv.device_x1, tv.device_x2, epsilon) OR
     bas.is_equal_eps(tv.device_y1, tv.device_y2, epsilon) THEN
    tv.wx1 := tv.world_x1;
    tv.wy1 := tv.world_y1;
    tv.wx2 := tv.world_x1 + 1;
    tv.wy2 := tv.world_y2 + 1;
    tv.dx1 := tv.device_x1;
    tv.dy1 := tv.device_y1;
    tv.kx  := 1;
    tv.ky  := 1;

    tv.is_valid := FALSE;
  ELSE
    world_x1  := tv.world_x1;
    world_y1  := tv.world_y1;
    world_x2  := tv.world_x2;
    world_y2  := tv.world_y2;
    device_x1 := tv.device_x1;
    device_y1 := tv.device_y1;
    device_x2 := tv.device_x2;
    device_y2 := tv.device_y2;

    IF ~(tv.aspect = aspect_ratio_stretch) THEN
      tv.kx := (device_x2 - device_x1) / (world_x2 - world_x1);
      tv.ky := (device_y2 - device_y1) / (world_y2 - world_y1);

      IF (tv.aspect = aspect_ratio_meet) = (tv.kx < tv.ky) THEN
        d := (world_y2 - world_y1) * tv.ky / tv.kx;
        world_y1 := world_y1 + ((world_y2 - world_y1 - d) * tv.align_y);
        world_y2 := world_y1 + d;
      ELSE
        d:=(world_x2 - world_x1 ) * tv.kx / tv.ky;
        world_x1:=world_x1 + ((world_x2 - world_x1 - d) * tv.align_x);
        world_x2:=world_x1 + d;
      END;
    END;

   tv.wx1 := world_x1;
   tv.wy1 := world_y1;
   tv.wx2 := world_x2;
   tv.wy2 := world_y2;
   tv.dx1 := device_x1;
   tv.dy1 := device_y1;
   tv.kx  := (device_x2 - device_x1) / (world_x2 - world_x1);
   tv.ky  := (device_y2 - device_y1) / (world_y2 - world_y1);

   tv.is_valid:=TRUE;
  END;
END update;

PROCEDURE (tv: trans_viewport_ptr) preserve_aspect_ratio*(alignx, aligny: bas.double; aspect: bas.int32);
BEGIN
  ASSERT(tv # NIL);
  tv.align_x := alignx;
  tv.align_y := aligny;
  tv.aspect  := aspect;

  tv.update();
END preserve_aspect_ratio;


PROCEDURE (tv: trans_viewport_ptr) device_viewport*(x1, y1, x2, y2: bas.double);
BEGIN
  ASSERT(tv # NIL);
  tv.device_x1 := x1;
  tv.device_y1 := y1;
  tv.device_x2 := x2;
  tv.device_y2 := y2;

  tv.update();
END device_viewport;

PROCEDURE (tv: trans_viewport_ptr) device_viewport_ex*(VAR x1, y1, x2, y2: bas.double);
BEGIN
  ASSERT(tv # NIL);
  x1 := tv.device_x1;
  y1 := tv.device_y1;
  x2 := tv.device_x2;
  y2 := tv.device_y2;
END device_viewport_ex;

PROCEDURE (tv: trans_viewport_ptr) world_viewport*(x1, y1, x2, y2: bas.double);
BEGIN
  ASSERT(tv # NIL);
  tv.world_x1 := x1;
  tv.world_y1 := y1;
  tv.world_x2 := x2;
  tv.world_y2 := y2;

  tv.update();
END world_viewport;

PROCEDURE (tv: trans_viewport_ptr) world_viewport_ex*(VAR x1, y1, x2, y2: bas.double);
BEGIN
  ASSERT(tv # NIL);
  x1 := tv.world_x1;
  y1 := tv.world_y1;
  x2 := tv.world_x2;
  y2 := tv.world_y2;
END world_viewport_ex;

PROCEDURE (tv: trans_viewport_ptr) world_viewport_actual*(VAR x1, y1, x2, y2: bas.double);
BEGIN
  ASSERT(tv # NIL);
  x1 := tv.wx1;
  y1 := tv.wy1;
  x2 := tv.wx2;
  y2 := tv.wy2;
END world_viewport_actual;

PROCEDURE (tv: trans_viewport_ptr) inverse_transform_scale_only(VAR x, y: bas.double);
BEGIN
  ASSERT(tv # NIL);
  x := x / tv.kx;
  y := y / tv.ky;
END inverse_transform_scale_only;

PROCEDURE (tv: trans_viewport_ptr) device_dx*(): bas.double;
BEGIN
  ASSERT(tv # NIL);
  RETURN tv.dx1 - tv.wx1 * tv.kx;
END device_dx;


PROCEDURE (tv: trans_viewport_ptr) device_dy*(): bas.double;
BEGIN
  ASSERT(tv # NIL);
  RETURN tv.dy1 - tv.wy1 * tv.ky;
END device_dy;


PROCEDURE (tv: trans_viewport_ptr) scale_x*(): bas.double;
BEGIN
  ASSERT(tv # NIL);
  RETURN tv.kx;
END scale_x;


PROCEDURE (tv: trans_viewport_ptr) scale_y*(): bas.double;
BEGIN
  ASSERT(tv # NIL);
  RETURN tv.ky;
END scale_y;

PROCEDURE (tv: trans_viewport_ptr) scale*(): bas.double;
BEGIN
  ASSERT(tv # NIL);
  RETURN (tv.kx + tv.ky ) * 0.5;
END scale;

PROCEDURE (tv: trans_viewport_ptr) to_affine*(mtx: ata.trans_affine_ptr);
VAR
  m, t: ata.trans_affine_translation_ptr;
  s: ata.trans_affine_scaling_ptr;
BEGIN
  NEW(m);
  m.ConstructT(-tv.wx1, -tv.wy1);

  NEW(s);
  s.ConstructS(tv.kx, tv.ky);
  m.multiply(s);

  NEW(t);
  t.ConstructT(tv.dx1, tv.dy1);
  m.multiply(t);

  mtx.assign(m);
END to_affine;

PROCEDURE (tv: trans_viewport_ptr) to_affine_scale_only*(mtx: ata.trans_affine_ptr);
VAR
  s: ata.trans_affine_scaling_ptr;
BEGIN
  NEW(s);
  s.ConstructS(tv.kx, tv.ky);

  mtx.assign(s);
END to_affine_scale_only;


END AggTransViewport.