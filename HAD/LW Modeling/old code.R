

LengthWeightAgeSp=LengthWeightAgeSp()
LengthWeightAgeSp$logW=log10(LengthWeightAgeSp$fldFishWholeWeight)
LengthWeightAgeSp$logL=log10(LengthWeightAgeSp$fldFishLength/10+.5)
fit2=lm(logW~logL*fldFishSex, data=LengthWeightAgeSp)
tmp= LengthWeightAgeSp %>% group_by(fldFishSex) %>%
  summarize(min=min(fldFishLength/10+.5, na.rm=TRUE),
            max=max(fldFishLength/10+.5, na.rm=TRUE))

#Plot line for females
tmpx= seq(tmp$min[1], tmp$max[1], length.out = dim(LengthWeightAgeSp)[1])
tmpy= 10^(predict(fit2, data.frame(logL=log10(tmpx), fldFishSex=factor("F"))))
tmpfemale=as.data.frame(cbind(tmpx, tmpy))

#Plot line for males
tmpx= seq(tmp$min[2], tmp$max[2], length.out = dim(LengthWeightAgeSp)[1])
tmpy= 10^(predict(fit2, data.frame(logL=log10(tmpx), fldFishSex=factor("M"))))
tmpmale=as.data.frame(cbind(tmpx, tmpy))
