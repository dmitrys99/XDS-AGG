<*+MAIN*>
MODULE agg_build;
IMPORT
  AggMath,
  SWholeIO;
BEGIN
  SWholeIO.WriteInt(AggMath.fast_sqrt(99226), 5);
END agg_build.