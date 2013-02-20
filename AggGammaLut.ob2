<* +O2EXTENSIONS *>
<* +O2NUMEXT *>
MODULE AggGammaLut;

IMPORT
  bas := AggBasics,
  apb := AggPixfmtBase;

TYPE
  gamma_lut_ptr* = POINTER TO gamma_lut;
  gamma_lut* = RECORD(apb.Gamma)
    gamma_shift,
    gamma_size,
    gamma_mask,

    hi_res_shift,
    hi_res_size,
    hi_res_mask,

    HiResT,
    LoResT: bas.int32;

    gamma: bas.double;

    dir_gamma,
    inv_gamma: POINTER TO ARRAY OF bas.int32u;
  END;

PROCEDURE (g: gamma_lut_ptr) Construct*(GammaShift, HiResShift: bas.int32);
VAR
  i: bas.int32;
BEGIN
  g.gamma_shift := GammaShift;
  g.gamma_size  := ASH(1, g.gamma_shift);
  g.gamma_mask  := g.gamma_size - 1;

  g.hi_res_shift := HiResShift;
  g.hi_res_size  := ASH(1, g.hi_res_shift);
  g.hi_res_mask  := g.hi_res_size - 1;

  g.HiResT := g.hi_res_shift DIV 8;
  g.LoResT := g.gamma_shift DIV 8;

  NEW(g.dir_gamma, g.gamma_size);
  NEW(g.inv_gamma, g.hi_res_size);

 (* dir_gamma*)
  FOR i := 0 TO g.gamma_size - 1 DO
    g.dir_gamma^[i] := ASH(i, (g.hi_res_shift - g.gamma_shift));
  END;

  (* inv_gamma *)
  FOR i := 0 TO g.hi_res_size - 1 DO
    g.inv_gamma^[i] := bas.shr_int32(i, g.hi_res_shift - g.gamma_shift);
  END;
END Construct;

PROCEDURE (g: gamma_lut_ptr) set_gamma*(gm: bas.double);
VAR
  i: bas.int32;

  inv_g: bas.double;
BEGIN
  g.gamma := gm;

  (* dir_gamma *)
  FOR i := 0 TO g.gamma_size - 1 DO
    g.dir_gamma^[i] :=ENTIER(((i / g.gamma_mask) ** g.gamma) * g.hi_res_mask);
  END;

  (* inv_gamma *)
  IF gm = 0 THEN
    FOR i := 0 TO g.hi_res_size-1 DO
      g.inv_gamma^[0] := 0;
    END;
  ELSE
    inv_g := 1 / gm;
    FOR i := 0 TO g.hi_res_size - 1 DO
      g.inv_gamma^[i] := ENTIER(((i / g.hi_res_mask) ** inv_g) * g.gamma_mask);
    END;
  END;
END set_gamma;

PROCEDURE (g: gamma_lut_ptr) Construct1*(gm: bas.double; GammaShift, HiResShift: bas.int32);
BEGIN
  g.gamma_shift:= GammaShift;
  g.gamma_size := ASH(1, g.gamma_shift);
  g.gamma_mask := g.gamma_size - 1;

  g.hi_res_shift:= HiResShift;
  g.hi_res_size := ASH(1, g.hi_res_shift);
  g.hi_res_mask := g.hi_res_size - 1;

  g.HiResT := g.hi_res_shift DIV 8;
  g.LoResT := g.gamma_shift DIV 8;

  g.gamma := 1;

  NEW(g.dir_gamma, g.gamma_size);
  NEW(g.inv_gamma, g.hi_res_size);

  g.set_gamma(gm);

END Construct1;

PROCEDURE (g: gamma_lut_ptr) Destruct*();
BEGIN
  g.dir_gamma := NIL;
  g.inv_gamma := NIL;
END Destruct;

PROCEDURE (g: gamma_lut_ptr) dir*(v: bas.int32): bas.int32u;
BEGIN
  RETURN g.dir_gamma^[v];
END dir;

PROCEDURE (g: gamma_lut_ptr) inv*(v: bas.int32): bas.int32u;
BEGIN
  RETURN g.inv_gamma^[v];
END inv;

END AggGammaLut.
PROCEDURE (g: gamma_lut_ptr)