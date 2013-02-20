MODULE AggVertexSource;

IMPORT
  bas := AggBasics;

TYPE
  vertex_source_ptr* = POINTER TO vertex_source;
  vertex_source* = RECORD
  END;

PROCEDURE (vs: vertex_source_ptr) Construct*();
BEGIN
END Construct;

PROCEDURE (vs: vertex_source_ptr) Destruct*();
BEGIN
END Destruct;

PROCEDURE (vs: vertex_source_ptr) remove_all*();
BEGIN
END remove_all;

PROCEDURE (vs: vertex_source_ptr) add_vertex*(x, y: bas.double; cmd: SET);
BEGIN
END add_vertex;

PROCEDURE (vs: vertex_source_ptr) num_paths*(): bas.int32;
BEGIN
  RETURN 0;
END num_paths;

PROCEDURE (vs: vertex_source_ptr) rewind*(path_id: bas.int32u);
BEGIN

END rewind;

<*+WOFF301*>
PROCEDURE (vs: vertex_source_ptr) vertex*(VAR x, y: bas.double): SET;
BEGIN
  RETURN {};
END vertex;
<*-WOFF301*>

PROCEDURE (vs: vertex_source_ptr) func_operator_gamma*(x: bas.double): bas.double;
BEGIN
  RETURN x;
END func_operator_gamma;

<*+WOFF301*>
PROCEDURE (vs: vertex_source_ptr) set_approximation_scale*(scale: bas.double);
BEGIN
END set_approximation_scale;
<*-WOFF301*>

PROCEDURE (vs: vertex_source_ptr) approximation_scale*(): bas.double;
BEGIN
  RETURN 0;
END approximation_scale;

END AggVertexSource.