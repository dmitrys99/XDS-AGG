MODULE AggConvShortenPath;
IMPORT
  bas := AggBasics,
  cav := AggConvAdaptorVcgen,
  vvs := AggVcgenVertexSequence,
  avs := AggVertexSource ;

TYPE
  conv_shorten_path_ptr* = POINTER TO conv_shorten_path;
  conv_shorten_path* = RECORD(cav.conv_adaptor_vcgen)
    the_generator : vvs.vcgen_vertex_sequence_ptr;
  END;

PROCEDURE (sc: conv_shorten_path_ptr) ConstructVS*(vs: avs.vertex_source_ptr);
BEGIN
 NEW(sc.the_generator);
 sc.the_generator.Construct();
 sc.ConstructGen(vs, sc.the_generator);
END ConstructVS;

PROCEDURE (sc: conv_shorten_path_ptr) Destruct*();
BEGIN
  sc.Destruct^();
  sc.the_generator := NIL;
END Destruct;

PROCEDURE (sc: conv_shorten_path_ptr) shorten*(): bas.double;
BEGIN
  RETURN sc.the_generator.shorten();
END shorten;

PROCEDURE (sc: conv_shorten_path_ptr) set_shorten*(s: bas.double);
BEGIN
  sc.the_generator.set_shorten(s);
END set_shorten;

END AggConvShortenPath.
PROCEDURE (sc: conv_shorten_path_ptr)