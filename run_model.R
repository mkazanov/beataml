DATA_DIR <- "/Users/mar/BIO/PROJECTS/DREAM/BEATAML/Data/"
MODEL_DIR <- "/Users/mar/BIO/PROJECTS/DREAM/BEATAML/mydata/"
OUTPUT_DIR <- "/Users/mar/BIO/PROJECTS/DREAM/BEATAML/mydata/"

library(data.table)
library(xgboost)

data1 <- read.csv(paste0(DATA_DIR,"clinical_categorical.csv"))
data1 <- data.table(data1)

data1[,priorMalignancyType_Unknown := 0]
data1[,priorMalignancyType_BreastCancer := 0]
data1[,priorMalignancyType_LungCancer := 0]
data1[,priorMalignancyType_NonHodgkinsLymphoma := 0]
data1[,priorMalignancyType_ProstateCancer := 0]
data1[,priorMalignancyType_Other := 0]

data1[priorMalignancyType == 0 & priorMalignancyNonMyeloid == 1, priorMalignancyType_Unknown := 1]
data1[priorMalignancyType == 1, priorMalignancyType_BreastCancer := 1]
data1[priorMalignancyType == 2, priorMalignancyType_LungCancer := 1]
data1[priorMalignancyType == 3, priorMalignancyType_NonHodgkinsLymphoma := 1]
data1[priorMalignancyType == 4, priorMalignancyType_ProstateCancer := 1]
data1[priorMalignancyType == 5, priorMalignancyType_Other := 1]

data <- data1[,.(lab_id,
                 priorMalignancyType_Unknown,
                 priorMalignancyType_BreastCancer,
                 priorMalignancyType_LungCancer,
                 priorMalignancyType_NonHodgkinsLymphoma,
                 priorMalignancyType_ProstateCancer,
                 priorMalignancyType_Other)]

data <- cbind(data,data1[,.(priorMalignancyRadiationTx,
                            priorMDS,
                            priorMDSMoreThanTwoMths,
                            priorMDSMPN,
                            priorMDSMPNMoreThanTwoMths,
                            priorMPN,
                            priorMPNMoreThanTwoMths)])

data1[,dxAtInclusion_AML := 0]
data1[,dxAtInclusion_MDS := 0]
data1[,dxAtInclusion_other := 0]

data1[dxAtInclusion == 0, dxAtInclusion_AML := 1]
data1[dxAtInclusion == 1, dxAtInclusion_MDS := 1]
data1[dxAtInclusion == 2, dxAtInclusion_other := 1]

data1[, specificDxAtInclusion_AMLinv16 := 0]
data1[, specificDxAtInclusion_AMLinv3 := 0]
data1[, specificDxAtInclusion_AMLmindif := 0]
data1[, specificDxAtInclusion_AMLCEBPA := 0]
data1[, specificDxAtInclusion_AMLNPM1 := 0]
data1[, specificDxAtInclusion_AMLmdsrel := 0]
data1[, specificDxAtInclusion_AMLt821 := 0]
data1[, specificDxAtInclusion_AMLt921 := 0]
data1[, specificDxAtInclusion_AMLmono := 0]
data1[, specificDxAtInclusion_AMLNOS := 0]
data1[, specificDxAtInclusion_AMLmyelmono := 0]
data1[, specificDxAtInclusion_AproL := 0]
data1[, specificDxAtInclusion_CML := 0]
data1[, specificDxAtInclusion_TMN := 0]
data1[, specificDxAtInclusion_other := 0]

data1[specificDxAtInclusion == 0, specificDxAtInclusion_AMLinv16 := 1]
data1[specificDxAtInclusion == 1, specificDxAtInclusion_AMLinv3 := 1]
data1[specificDxAtInclusion == 2, specificDxAtInclusion_AMLmindif := 1]
data1[specificDxAtInclusion == 3, specificDxAtInclusion_AMLCEBPA := 1]
data1[specificDxAtInclusion == 4, specificDxAtInclusion_AMLNPM1 := 1]
data1[specificDxAtInclusion == 5, specificDxAtInclusion_AMLmdsrel := 1]
data1[specificDxAtInclusion == 6, specificDxAtInclusion_AMLt821 := 1]
data1[specificDxAtInclusion == 7, specificDxAtInclusion_AMLt921 := 1]
data1[specificDxAtInclusion == 8, specificDxAtInclusion_AMLmono := 1]
data1[specificDxAtInclusion == 9, specificDxAtInclusion_AMLNOS := 1]
data1[specificDxAtInclusion == 10, specificDxAtInclusion_AMLmyelmono := 1]
data1[specificDxAtInclusion == 11, specificDxAtInclusion_AproL := 1]
data1[specificDxAtInclusion == 12, specificDxAtInclusion_CML := 1]
data1[specificDxAtInclusion == 13, specificDxAtInclusion_TMN := 1]
data1[specificDxAtInclusion == 14, specificDxAtInclusion_other := 1]

data1[,dxAtSpecimenAcquisition_AML := 0]
data1[,dxAtSpecimenAcquisition_MDS := 0]
data1[,dxAtSpecimenAcquisition_other := 0]

data1[dxAtSpecimenAcquisition == 0, dxAtSpecimenAcquisition_AML := 1]
data1[dxAtSpecimenAcquisition == 1, dxAtSpecimenAcquisition_MDS := 1]
data1[dxAtSpecimenAcquisition == 2, dxAtSpecimenAcquisition := 1]

data1[, specificDxAtAcquisition_AMLinv16 := 0]
data1[, specificDxAtAcquisition_AMLinv3 := 0]
data1[, specificDxAtAcquisition_AMLmindif := 0]
data1[, specificDxAtAcquisition_AMLCEBPA := 0]
data1[, specificDxAtAcquisition_AMLNPM1 := 0]
data1[, specificDxAtAcquisition_AMLmdsrel := 0]
data1[, specificDxAtAcquisition_AMLt821 := 0]
data1[, specificDxAtAcquisition_AMLt921 := 0]
data1[, specificDxAtAcquisition_AMLmono := 0]
data1[, specificDxAtAcquisition_AMLNOS := 0]
data1[, specificDxAtAcquisition_AMLmyelmono := 0]
data1[, specificDxAtAcquisition_AproL := 0]
data1[, specificDxAtAcquisition_TMN := 0]
data1[, specificDxAtAcquisition_other := 0]

data1[specificDxAtAcquisition == 0, specificDxAtAcquisition_AMLinv16 := 1]
data1[specificDxAtAcquisition == 1, specificDxAtAcquisition_AMLinv3 := 1]
data1[specificDxAtAcquisition == 2, specificDxAtInclusion_AMLmindif := 1]
data1[specificDxAtAcquisition == 3, specificDxAtInclusion_AMLCEBPA := 1]
data1[specificDxAtAcquisition == 4, specificDxAtInclusion_AMLNPM1 := 1]
data1[specificDxAtAcquisition == 5, specificDxAtInclusion_AMLmdsrel := 1]
data1[specificDxAtAcquisition == 6, specificDxAtInclusion_AMLt821 := 1]
data1[specificDxAtAcquisition == 7, specificDxAtInclusion_AMLt921 := 1]
data1[specificDxAtAcquisition == 8, specificDxAtInclusion_AMLmono := 1]
data1[specificDxAtAcquisition == 9, specificDxAtInclusion_AMLNOS := 1]
data1[specificDxAtAcquisition == 10, specificDxAtInclusion_AMLmyelmono := 1]
data1[specificDxAtAcquisition == 11, specificDxAtInclusion_AproL := 1]
data1[specificDxAtAcquisition == 12, specificDxAtInclusion_TMN := 1]
data1[specificDxAtAcquisition == 13, specificDxAtInclusion_other := 1]

data <- cbind(data, data1[,.(dxAtInclusion_AML,
                             dxAtInclusion_MDS,
                             dxAtInclusion_other,
                             specificDxAtInclusion_AMLinv16,
                             specificDxAtInclusion_AMLinv3,
                             specificDxAtInclusion_AMLmindif,
                             specificDxAtInclusion_AMLCEBPA,
                             specificDxAtInclusion_AMLNPM1,
                             specificDxAtInclusion_AMLmdsrel,
                             specificDxAtInclusion_AMLt821,
                             specificDxAtInclusion_AMLt921,
                             specificDxAtInclusion_AMLmono,
                             specificDxAtInclusion_AMLNOS,
                             specificDxAtInclusion_AMLmyelmono,
                             specificDxAtInclusion_AproL,
                             specificDxAtInclusion_CML,
                             specificDxAtInclusion_TMN,
                             specificDxAtInclusion_other,
                             dxAtSpecimenAcquisition_AML,
                             dxAtSpecimenAcquisition_MDS,
                             dxAtSpecimenAcquisition_other,
                             specificDxAtAcquisition_AMLinv16,
                             specificDxAtAcquisition_AMLinv3,
                             specificDxAtAcquisition_AMLmindif,
                             specificDxAtAcquisition_AMLCEBPA,
                             specificDxAtAcquisition_AMLNPM1,
                             specificDxAtAcquisition_AMLmdsrel,
                             specificDxAtAcquisition_AMLt821,
                             specificDxAtAcquisition_AMLt921,
                             specificDxAtAcquisition_AMLmono,
                             specificDxAtAcquisition_AMLNOS,
                             specificDxAtAcquisition_AMLmyelmono,
                             specificDxAtAcquisition_AproL,
                             specificDxAtAcquisition_TMN,
                             specificDxAtAcquisition_other)])

data1[, specimenType_BoneMarrow := 0]
data1[, specimenType_Leuk := 0]
data1[, specimenType_Blood := 0]

data1[specimenType == 0, specimenType_BoneMarrow := 1]
data1[specimenType == 1, specimenType_Leuk := 1]
data1[specimenType == 2, specimenType_Blood := 1]

data1[, FAB_Unknown := 0]
data1[, FAB_M0 := 0]
data1[, FAB_M1 := 0]
data1[, FAB_M3 := 0]
data1[, FAB_M4 := 0]
data1[, FAB_M5 := 0]
data1[, FAB_Other := 0]

data1[FAB.Blast.Morphology == 0, FAB_Unknown := 1]
data1[FAB.Blast.Morphology == 1, FAB_M0 := 1]
data1[FAB.Blast.Morphology == 2, FAB_M1 := 1]
data1[FAB.Blast.Morphology == 3, FAB_M3 := 1]
data1[FAB.Blast.Morphology == 4, FAB_M4 := 1]
data1[FAB.Blast.Morphology == 5, FAB_M5 := 1]
data1[FAB.Blast.Morphology == 6, FAB_Other := 1]               

data1[, Karyotype_Unknown := 0]
data1[, Karyotype_46XX20 := 0]
data1[, Karyotype_46XY := 0]
data1[, Karyotype_46XY19 := 0]
data1[, Karyotype_46XY20 := 0]
data1[, Karyotype_Other := 0]

data1[ Karyotype == 0, Karyotype_Unknown := 1]
data1[ Karyotype == 1, Karyotype_46XX20 := 1]
data1[ Karyotype == 2, Karyotype_46XY := 1]
data1[ Karyotype == 3, Karyotype_46XY19 := 1]
data1[ Karyotype == 4, Karyotype_46XY20 := 1]
data1[ Karyotype == 5, Karyotype_Other := 1]

data1[, finalFusion_Unknown := 0]
data1[, finalFusion_CBFB := 0]
data1[, finalFusion_GATA2 := 0]
data1[, finalFusion_MLLT3 := 0]
data1[, finalFusion_None := 0]
data1[, finalFusion_PML := 0]
data1[, finalFusion_RUNX1 := 0]
data1[, finalFusion_Other := 0]

data1[finalFusion == 0, finalFusion_Unknown := 1]
data1[finalFusion == 1, finalFusion_CBFB := 1]
data1[finalFusion == 2, finalFusion_GATA2 := 1]
data1[finalFusion == 3, finalFusion_MLLT3 := 1]
data1[finalFusion == 4, finalFusion_None := 1]
data1[finalFusion == 5, finalFusion_PML := 1]
data1[finalFusion == 6, finalFusion_RUNX1 := 1]
data1[finalFusion == 7, finalFusion_Other := 1]

data <- cbind(data, data1[,.(specimenType_BoneMarrow,
                             specimenType_Leuk,
                             specimenType_Blood,
                             consensus_sex,
                             FAB_Unknown,
                             FAB_M0,
                             FAB_M1,
                             FAB_M3,
                             FAB_M4,
                             FAB_M5,
                             FAB_Other,
                             Karyotype_Unknown,
                             Karyotype_46XX20,
                             Karyotype_46XY,
                             Karyotype_46XY19,
                             Karyotype_46XY20,
                             Karyotype_Other,
                             FLT3.ITD,
                             NPM1,
                             finalFusion_Unknown,
                             finalFusion_CBFB,
                             finalFusion_GATA2,
                             finalFusion_MLLT3,
                             finalFusion_None,
                             finalFusion_PML,
                             finalFusion_RUNX1,
                             finalFusion_Other)])

data2 <- read.csv(paste0(DATA_DIR,"clinical_numerical.csv"))
data2 <- data.table(data2)

mean1 <- data2[,mean(X..Blasts.in.PB,na.rm = TRUE)]
data2[is.na(X..Blasts.in.PB), X..Blasts.in.PB:=mean1]
mean2 <- data2[,mean(WBC.Count,na.rm=TRUE)]
data2[is.na(WBC.Count), WBC.Count:=mean2]
mean3 <- data2[,mean(ageAtDiagnosis,na.rm=TRUE)]
data2[is.na(ageAtDiagnosis), ageAtDiagnosis:=mean3]

data <- merge(data,data2,by="lab_id")

data3 <- read.csv(paste0(DATA_DIR,"dnaseq.csv"))
data3 <- data.table(data3)

data4 <- data[,.(lab_id)]  
genes <- read.csv(paste0(MODEL_DIR,"model_genes.csv"))
genes <- data.table(genes)
for(i in 1:nrow(genes)){
  g <- as.character(genes[i,gene])
  data4[,(g):=0]
}
for(i in 1:nrow(data3)){
  labid <- as.character(data3[i,lab_id])
  gene <- as.character(data3[i,Hugo_Symbol])
  if(gene %in% names(data4)){
   data4[lab_id == labid,(gene):=1]
  }
}

data <- merge(data,data4,by="lab_id")

data5 <- read.csv(paste0(DATA_DIR,"rnaseq.csv"))
data5$Symbol <- NULL
data6 <- setNames(data.frame(t(data5[,-1])), data5[,1])
data6 <- setDT(data6, keep.rownames = TRUE)
data6[, rn := gsub("X","",rn)] 
data6[, lab_id := gsub("\\.","-",rn)]
data6[, rn := NULL]

dataExp <- merge(data,data6,by="lab_id")

## Load and apply model

models <- read.csv(paste0(MODEL_DIR,"model_inhs.csv"))
models <- data.table(models)

predTemplate <- dataExp[, .(lab_id)]
dataExp[, lab_id:=NULL]
XX <- as.matrix(dataExp)

allPreds <- data.table()

for(i in 1:nrow(models)){
  inh <- models[i,inhibitor]
  fname <- models[i,filename]
  model <- xgb.load(paste0(MODEL_DIR,fname))
  
  aucs <- predict(model,XX)
  pred <- copy(predTemplate)
  pred[, inhibitor := inh]
  pred <- cbind(pred,aucs)
  setnames(pred,c("lab_id","inhibitor","auc"))
           
  allPreds <- rbind(allPreds,pred)
}


# ## Load model
# 
# modelCoefs <- read.csv(paste0(MODEL_DIR,"model_coefs.csv"))
# modelCoefs <- data.table(modelCoefs)
# 
# ## Apply
# 
# allPreds <- data.table()
# 
# for(inh in unique(modelCoefs[,inhibitor])) 
# {
#   pred <- dataExp[,.(lab_id)]
#   pred[, inhibitor := inh]
#   pred[, auc := 0]
#   coefs <- modelCoefs[inhibitor == inh]
#   
#   for(i in 1:nrow(coefs)){
#    varname <- as.character(coefs[i,parname])
#    cf <- coefs[i,coef]
#    if(varname == "Intercept"){
#      pred[, auc := cf]
#    } else {
#     pred <- cbind(pred,cf * dataExp[,varname,with=FALSE])
#     setnames(pred,c("lab_id","inhibitor","auc","tmp"))
#     pred[, auc := auc + tmp]
#     pred[,tmp:=NULL]
#    }
#   }
#   
#   allPreds <- rbind(allPreds,pred)
# }  
 
write.csv(allPreds,paste0(OUTPUT_DIR,"predictions.csv"),row.names = FALSE,quote = FALSE)

