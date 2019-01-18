SELECT [fldPrimeStation]
,[Yr]
,[LonDec]
,[LatDec]
,[ARG]
,[BLR]
,[BOF]
,[COD]
,[CUR]
,[DAB]
,[DFL]
,[DGS]
,[DII]
,[ESB]
,[GSS]
,[GUG]
,[HAD]
,[HER]
,[HKE]
,[HOM]
,[JOD]
,[LBI]
,[LSD]
,[MAC]
,[MEG]
,[MON]
,[NOP]
,[PLE]
,[POD]
,[POK]
,[POL]
,[PTR]
,[SAR]
,[SDR]
,[SHR]
,[SKT]
,[SOL]
,[SPR]
,[THR]
,[UNR]
,[WAF]
,[WHB]
,[WHG]
,y.CatchKg
FROM
(SELECT x.fldPrimeStation
,x.Yr
,x.LonDec
,x.LatDec
,x.Species
,x.CatchKg
FROM InformaticsLoad.dbo.SurveyAbundanceAndDistribution x) AS SourceTable
PIVOT
(
  SUM([CatchKg])
  FOR Species IN ([ARG],[BLR],[BOF],[COD],[CUR],[DAB],[DFL],[DGS],[DII],[ESB],[GSS],
[GUG],[HAD],[HER],[HKE],[HOM],[JOD],[LBI],[LSD],[MAC],[MEG],[MON],[NOP],[PLE],[POD],
[POK],[POL],[PTR],[SAR],[SDR],[SHR],[SKT],[SOL],[SPR],[THR],[UNR],[WAF],[WHB],[WHG])
) AS PivotTable
LEFT JOIN InformaticsLoad.dbo.SurveySummary y ON fldPrimeStation = y.PrimeStation AND Yr = y.Year;