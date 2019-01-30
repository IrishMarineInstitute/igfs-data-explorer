use InformaticsLoad

EXEC sp_execute_external_script
  @language =N'R',
  @script=N'
       library(RODBC);
       
       #Length/Weight/Age data
       channel <- odbcDriverConnect("Driver=SQL Server; Server=MIINFORMPROD01; Database=InformaticsLoad; Uid=INFO_READ; Pwd=V1G437sd@");
       LWA<- sqlQuery(channel,"SELECT * FROM SurveyLenghtWeightAge");
       close(channel);
       
       #Station data
       channel <- odbcDriverConnect("Driver=SQL Server; Server=MIINFORMPROD01; Database=InformaticsLoad; Uid=INFO_READ; Pwd=V1G437sd@");
       stn<- sqlQuery(channel,"SELECT * FROM SurveyMI_Survey_Station_Data");
       close(channel);
       
       #Abundance and Distribution data
       channel <- odbcDriverConnect("Driver=SQL Server; Server=MIINFORMPROD01; Database=InformaticsLoad; Uid=INFO_READ; Pwd=V1G437sd@");
       data1<- sqlQuery(channel,"SELECT * FROM SurveyAbundanceAndDistribution");
       close(channel);
       
       #Length Frequency data
       channel <- odbcDriverConnect("Driver=SQL Server; Server=MIINFORMPROD01; Database=InformaticsLoad; Uid=INFO_READ; Pwd=V1G437sd@");
       LengthData<- sqlQuery(channel,"SELECT * FROM SurveyLenghtData");
       close(channel);
       
       #dat data
       channel <- odbcDriverConnect("Driver=SQL Server; Server=MIINFORMPROD01; Database=InformaticsLoad; Uid=INFO_READ; Pwd=V1G437sd@");
       dat<- sqlQuery(channel,"SELECT * FROM SurveyCatchRate");
       close(channel);

       species=c("COD", "HAD", "WHG", "HER", "HKE", "HOM", "MAC", "WHB",
				"NOP", "SPR", "BOF", "POD", "GUG", "DAB", "LSD", "PLE", 
				"MEG", "SDR", "JOD", "DGS", "POK", "MON", "THR", "SOL",
				"CUR", "POL", "ESB", "SKT");

       # Split RDS files by species
       for(i in species){
              #Station data - DO NOT filter
              saveRDS(stn, paste0("C:\\Rfiles\\IGFS\\",i,"\\stn.RDS"));

              #Dat filtering 
              dat_fish=subset(dat, Species==i);
              saveRDS(dat_fish, paste0("C:\\Rfiles\\IGFS\\",i,"\\dat.RDS"));
       
              #Length, Weight, Age data filtering
              LWA_fish=subset(LWA, fldMainSpeciesCode==i);
              saveRDS(LWA_fish, paste0("C:\\Rfiles\\IGFS\\",i,"\\LengthWeightAge.RDS"));

              #Length frequency filtering
              LD_fish=subset(LengthData, Species==i);
              saveRDS(LD_fish, paste0("C:\\Rfiles\\IGFS\\",i,"\\LengthData.RDS"));

              #data1 filtering
              data1_fish=subset(data1, Species==i);
              saveRDS(data1_fish, paste0("C:\\Rfiles\\IGFS\\",i,"\\data1.RDS"));


  };


  ';

GO
