MODULE AggConvTransform;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource,
  ata := AggTransAffine;

TYPE
  conv_transform_ptr* = POINTER TO conv_transform;
  conv_transform* = RECORD(avs.vertex_source)
    source: avs.vertex_source_ptr;
    trans : ata.trans_affine_ptr;
  END;

PROCEDURE (ct: conv_transform_ptr) rewind*(path_id: bas.int32u);
BEGIN
  ct.source.rewind(path_id);
END rewind;

PROCEDURE (ct: conv_transform_ptr) vertex*(VAR x, y: bas.double): SET;
VAR
  cmd: SET;
BEGIN
  cmd := ct.source.vertex(x, y);

  IF bas.is_vertex(cmd) THEN
    ct.trans.transform(ct.trans, x, y);
  END;

  RETURN cmd;
END vertex;

PROCEDURE (ct: conv_transform_ptr) set_transformer*(tr: ata.trans_affine_ptr);
BEGIN
  ct.trans := tr;
END set_transformer;

PROCEDURE (ct: conv_transform_ptr) set_source*(source: avs.vertex_source_ptr);
BEGIN
  ct.source := source;
END set_source;

PROCEDURE (ct: conv_transform_ptr) ConstructCT*(source: avs.vertex_source_ptr; tr: ata.trans_affine_ptr);
BEGIN
  ct.Construct();
  ct.trans := tr;
  ct.source := source;
END ConstructCT;

END AggConvTransform.