MODULE AggGammaFunctions;

IMPORT
  bas  := AggBasics,
  avs  := AggVertexSource;


TYPE
  gamma_ptr* = POINTER TO gamma;
  gamma* = RECORD(avs.vertex_source)
  END;

  gamma_none_ptr* = POINTER TO gamma_none;
  gamma_none* = RECORD(gamma)
  END;

  gamma_power_ptr* = POINTER TO gamma_power;
  gamma_power* = RECORD(gamma)
    gamma*: bas.double;
  END;

  gamma_threshold_ptr* = POINTER TO gamma_threshold;
  gamma_threshold* = RECORD(gamma)
    threshold*: bas.double;
  END;

  gamma_linear_ptr* = POINTER TO gamma_linear;
  gamma_linear* = RECORD(gamma)
    start* : bas.double;
    end*   : bas.double;
  END;

  gamma_multiply_ptr* = POINTER TO gamma_multiply;
  gamma_multiply* = RECORD(gamma)
    mul* : bas.double;
  END;

(*=============*)
(* gamma_power *)
(*=============*)

PROCEDURE (gp: gamma_power_ptr) ConstructI*(g: bas.double);
BEGIN
  gp.gamma := g;
END ConstructI;

PROCEDURE (gp: gamma_power_ptr) Construct*();
BEGIN
  gp.gamma := 1.0;
END Construct;

PROCEDURE (gp: gamma_power_ptr) func_operator_gamma*(x: bas.double): bas.double;
BEGIN
  IF gp.gamma > 0 THEN
    RETURN bas.agg_power(x, gp.gamma)
  ELSE
    RETURN 1.0
  END;
END func_operator_gamma;


(*=================*)
(* gamma_threshold *)
(*=================*)

PROCEDURE (gt: gamma_threshold_ptr) ConstructI*(t: bas.double);
BEGIN
  gt.threshold := t;
END ConstructI;

PROCEDURE (gt: gamma_threshold_ptr) Construct*();
BEGIN
  gt.threshold := 0.5;
END Construct;

PROCEDURE (gt: gamma_threshold_ptr) func_operator_gamma*(x: bas.double): bas.double;
BEGIN
  IF x < gt.threshold THEN
    RETURN 0.0
  ELSE
    RETURN 1.0
  END;
END func_operator_gamma;

(*==============*)
(* gamma_linear *)
(*==============*)

PROCEDURE (gl: gamma_linear_ptr) ConstructI*(s, e: bas.double);
BEGIN
  gl.start := s;
  gl.end   := e;
END ConstructI;

PROCEDURE (gl: gamma_linear_ptr) Construct*();
BEGIN
  gl.start := 0.0;
  gl.end   := 1.0;
END Construct;

PROCEDURE (gl: gamma_linear_ptr) func_operator_gamma*(x: bas.double): bas.double;
BEGIN
  IF x < gl.start THEN
    RETURN 0.0
  ELSE
    IF x > gl.end THEN
      RETURN 1.0
    ELSE
      IF gl.end - gl.start # 0 THEN
        RETURN (x - gl.start ) / (gl.end - gl.start)
      ELSE
        RETURN 0
      END;
    END;
  END
END func_operator_gamma;

(*================*)
(* gamma_multiply *)
(*================*)

PROCEDURE (gm: gamma_multiply_ptr) ConstructI*(mul: bas.double);
BEGIN
  gm.mul := mul;
END ConstructI;

PROCEDURE (gm: gamma_multiply_ptr) Construct*();
BEGIN
  gm.mul := 1.0;
END Construct;

PROCEDURE (gm: gamma_multiply_ptr) func_operator_gamma*(x: bas.double): bas.double;
VAR
  y: bas.double;
BEGIN
  y := x * gm.mul;

  IF y > 1.0 THEN
    y := 1.0;
  END;

  RETURN y;
END func_operator_gamma;

END AggGammaFunctions.