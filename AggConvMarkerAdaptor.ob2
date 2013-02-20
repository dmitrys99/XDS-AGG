MODULE AggConvMarkerAdaptor;

IMPORT
  bas := AggBasics,
  cav := AggConvAdaptorVcgen,
  vvs := AggVcgenVertexSequence,
  avs := AggVertexSource;

TYPE
  conv_marker_adaptor_ptr* = POINTER TO conv_marker_adaptor;
  conv_marker_adaptor* = RECORD(cav.conv_adaptor_vcgen)
    the_generator: vvs.vcgen_vertex_sequence_ptr;
  END;

PROCEDURE (cm: conv_marker_adaptor_ptr) ConstructVS*(vs: avs.vertex_source_ptr);
BEGIN
  NEW(cm.the_generator);
  cm.the_generator.Construct();
  cm.ConstructGen(vs, cm.the_generator);
END ConstructVS;

PROCEDURE (cm: conv_marker_adaptor_ptr) Destruct*();
BEGIN
  cm.Destruct^();
  cm.the_generator := NIL;
END Destruct;

PROCEDURE (cm: conv_marker_adaptor_ptr) shorten*(): bas.double;
BEGIN
  RETURN cm.the_generator.shorten();
END shorten;

PROCEDURE (cm: conv_marker_adaptor_ptr) set_shorten*(s: bas.double);
BEGIN
  cm.the_generator.set_shorten(s);
END set_shorten;


END AggConvMarkerAdaptor.