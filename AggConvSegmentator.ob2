MODULE AggConvSegmentator;
(*IMPORT
  bas := AggBasics,
  avs := AggVertexSource,
  aca := AggConvAdaptorVpgen,
  vps := AggVpgenSegmentator;

TYPE
  conv_segmentator_ptr* = POINTER TO conv_segmentator;
  conv_segmentator* = RECORD(aca.conv_adaptor_vpgen)
    the_generator: vps.vpgen_segmentator_ptr;
  END;

PROCEDURE (cs: conv_segmentator_ptr) ConstructVS*(vs: avs.vertex_source_ptr);
BEGIN
 NEW(sc.the_generator);
 sc.the_generator.Construct();
 sc.ConstructGen(vs, sc.the_generator);
END ConstructVS;

PROCEDURE (cs: conv_segmentator_ptr) Destruct*();
BEGIN
  sc.Destruct^();
  sc.the_generator := NIL;
END Destruct;

PROCEDURE (cs: conv_segmentator_ptr) approximation_scale*(): bas.double;
BEGIN
  RETURN sc.the_generator.approximation_scale();
END shorten;

PROCEDURE (cs: conv_segmentator_ptr) set_approximation_scale*(s: bas.double);
BEGIN
  sc.the_generator.set_approximation_scale(s);
END set_shorten;

                       *)
END AggConvSegmentator.