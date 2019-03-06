library(RODBC)
library(reshape)
library(dplyr)
Q<- "
SELECT *
FROM [InformaticsLoad].[dbo].[SurveyAbundanceAndDistribution]
"
channel <- odbcDriverConnect("Driver=SQL Server; 
                             Server=VMINFORMDEV01; Database=InformaticsLoad")
IGFSdata<- sqlQuery(channel,Q)
close(channel)

  
Q2 <- "
SELECT x.[fldPrimeStation]
,x.[Yr]
,x.[LonDec]
,x.[LatDec]
,x.[Species]
,x.[CatchKg]
,y.[CatchKg]
FROM
(SELECT [fldPrimeStation]
,[Yr]
,[LonDec]
,[LatDec]
,[Species]
,[CatchKg]
FROM [InformaticsLoad].[dbo].[SurveyAbundanceAndDistribution] x) AS SourceTable
PIVOT
(
  SUM([CatchKg])
  FOR Species IN ([ARG],[BLR],[BOF],[COD],[CUR],[DAB],[DFL],[DGS],[DII],[ESB],[GSS],
[GUG],[HAD],[HER],[HKE],[HOM],[JOD],[LBI],[LSD],[MAC],[MEG],[MON],[NOP],[PLE],[POD],
[POK],[POL],[PTR],[SAR],[SDR],[SHR],[SKT],[SOL],[SPR],[THR],[UNR],[WAF],[WHB],[WHG])
) AS PivotTable;
LEFT JOIN [InformaticsLoad].[dbo].[SurveySummary] y ON x.[fldPrimeStation] = y.[fldPrimeStation]
  "

channel <- odbcDriverConnect("Driver=SQL Server; 
                             Server=VMINFORMDEV01; Database=InformaticsLoad")
IGFSdata_2<- sqlQuery(channel,Q2)
close(channel)

#IGFSdata <- read.csv(file= "SpDataAgg.csv")
  
Q3<- "
SELECT *
FROM [InformaticsLoad].[dbo].[SurveySummaryBySpecies]
"
channel <- odbcDriverConnect("Driver=SQL Server; 
                             Server=VMINFORMDEV01; Database=InformaticsLoad")
sp_data<- sqlQuery(channel,Q3)
close(channel)
saveRDS(sp_data, file = "sp_data20180504.rds")

sp_data_gp <- sp_data %>% 
  mutate(Group =
           recode(Species,
                  'ARG' = "Small pelagic",
                  'BLR' = "Elasmobranch",
                  'BOF' = "Small pelagic",
                  'COD' = "Large demersal",
                  'CUR' = "Elasmobranch",
                  'DAB' = "Flat fish",
                  'DFL' = "Elasmobranch",
                  'DGS' = "Elasmobranch",
                  'DII' = "Elasmobranch",
                  'ESB' = "Large demersal",
                  'GSS' = "Small pelagic",
                  'GUG' = "Large demersal",
                  'HAD' = "Large demersal",
                  'HER' = "Small pelagic",
                  'HKE' = "Large demersal",
                  'HOM' = "Small pelagic",
                  'JOD' = "Large demersal",
                  'LBI' = "Flat fish",
                  'LSD' = "Elasmobranch",
                  'MAC' = "Small pelagic",
                  'MEG' = "Flat fish",
                  'MON' = "Monkfish",
                  'NOP' = "Small demersal",
                  'PLE' = "Flat fish",
                  'POD' = "Small demersal",
                  'POK' = "Large demersal",
                  'POL' = "Large demersal",
                  'PTR' = "Elasmobranch",
                  'SAR' = "Elasmobranch",
                  'SDR' = "Elasmobranch",
                  'SHR' = "Elasmobranch",
                  'SKT' = "Elasmobranch",
                  'SOL' = "Flat fish",
                  'SPR' = "Small pelagic",
                  'THR' = "Elasmobranch",
                  'UNR' = "Elasmobranch",
                  'WAF' = "Monkfish",
                  'WHB' = "Small pelagic",
                  'WHG' = "Large demersal"
           ))

saveRDS(sp_data_gp,  "sp_data_gp_20180611.RDS")
