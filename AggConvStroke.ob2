MODULE AggConvStroke;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource,
  vgs := AggVcgenStroke,
  cav := AggConvAdaptorVcgen;

TYPE

  conv_stroke_ptr* = POINTER TO conv_stroke;
  conv_stroke* = RECORD(cav.conv_adaptor_vcgen)
    the_generator: vgs.vcgen_stroke_ptr;
  END;

PROCEDURE (cs: conv_stroke_ptr) ConstructVS*(vs: avs.vertex_source_ptr);
BEGIN
  NEW(cs.the_generator);
  cs.the_generator.Construct();
  cs.ConstructGen(vs, cs.the_generator);
END ConstructVS;

PROCEDURE (cs: conv_stroke_ptr) Destruct*();
BEGIN
  cs.the_generator := NIL;
  cs.Destruct^();
END Destruct;

PROCEDURE (cs: conv_stroke_ptr) set_line_cap*(line_cap: bas.int32);
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    cs.generator(vgs.vcgen_stroke_ptr).set_line_cap(line_cap);
  END;
END set_line_cap;

PROCEDURE (cs: conv_stroke_ptr) set_line_join*(line_join: bas.int32);
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    cs.generator(vgs.vcgen_stroke_ptr).set_line_join(line_join);
  END;
END set_line_join;

PROCEDURE (cs: conv_stroke_ptr) set_inner_join*(inner_join: bas.int32);
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    cs.generator(vgs.vcgen_stroke_ptr).set_inner_join(inner_join);
  END;
END set_inner_join;

PROCEDURE (cs: conv_stroke_ptr) line_cap*(): bas.int32;
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    RETURN cs.generator(vgs.vcgen_stroke_ptr).line_cap();
  ELSE
    RETURN -1;
  END;
END line_cap;

PROCEDURE (cs: conv_stroke_ptr) line_join*(): bas.int32;
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    RETURN cs.generator(vgs.vcgen_stroke_ptr).line_join();
  ELSE
    RETURN -1;
  END;
END line_join;

PROCEDURE (cs: conv_stroke_ptr) inner_join*(): bas.int32;
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    RETURN cs.generator(vgs.vcgen_stroke_ptr).inner_join();
  ELSE
    RETURN -1;
  END;
END inner_join;

PROCEDURE (VAR cs:conv_stroke) set_width*(width: bas.double);
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    cs.generator(vgs.vcgen_stroke_ptr).set_width(width);
  END;
END set_width;

PROCEDURE (VAR cs:conv_stroke) set_miter_limit*(miter_limit: bas.double);
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    cs.generator(vgs.vcgen_stroke_ptr).set_miter_limit(miter_limit);
  END;
END set_miter_limit;

PROCEDURE (VAR cs:conv_stroke) set_inner_miter_limit*(inner_miter_limit: bas.double);
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    cs.generator(vgs.vcgen_stroke_ptr).set_inner_miter_limit(inner_miter_limit);
  END;
END set_inner_miter_limit;

PROCEDURE (VAR cs:conv_stroke) set_miter_limit_theta*(miter_limit_theta: bas.double);
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    cs.generator(vgs.vcgen_stroke_ptr).set_miter_limit_theta(miter_limit_theta);
  END;
END set_miter_limit_theta;

PROCEDURE (cs: conv_stroke_ptr) set_approximation_scale*(approximation_scale: bas.double);
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    cs.generator(vgs.vcgen_stroke_ptr).set_approximation_scale(approximation_scale);
  END;
END set_approximation_scale;

PROCEDURE (cs: conv_stroke_ptr) width*(): bas.double;
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    RETURN cs.generator(vgs.vcgen_stroke_ptr).width();
  ELSE
    RETURN 0;
  END;
END width;

PROCEDURE (cs: conv_stroke_ptr) miter_limit*(): bas.double;
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    RETURN cs.generator(vgs.vcgen_stroke_ptr).miter_limit();
  ELSE
    RETURN 0;
  END;
END miter_limit;

PROCEDURE (cs: conv_stroke_ptr) inner_miter_limit*(): bas.double;
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    RETURN cs.generator(vgs.vcgen_stroke_ptr).inner_miter_limit();
  ELSE
    RETURN 0;
  END;
END inner_miter_limit;

PROCEDURE (cs: conv_stroke_ptr) approximation_scale*(): bas.double;
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    RETURN cs.generator(vgs.vcgen_stroke_ptr).approximation_scale();
  ELSE
    RETURN 0;
  END;
END approximation_scale;

PROCEDURE (VAR cs:conv_stroke) set_shorten*(shorten: bas.double);
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    cs.generator(vgs.vcgen_stroke_ptr).set_shorten(shorten);
  END;
END set_shorten;

PROCEDURE (cs: conv_stroke_ptr) shorten*(): bas.double;
BEGIN
  IF cs.generator IS vgs.vcgen_stroke_ptr THEN
    RETURN cs.generator(vgs.vcgen_stroke_ptr).shorten();
  ELSE
    RETURN 0;
  END;
END shorten;


END AggConvStroke.