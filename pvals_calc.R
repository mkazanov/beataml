is <- c(1,2,3,4,5,6,7,8,9,10)

ROOT_DIR <- "/Users/mar/BIO/PROJECTS/DREAM/BEATAML/test/"

dataExp <- read.csv(paste0(ROOT_DIR,"trainingExp.csv"))
dataExp <- data.table(dataExp)
inhs <- read.csv(paste0(ROOT_DIR,"model_inhs.csv"))
inhs <- data.table(inhs)
ys <- read.csv(paste0(ROOT_DIR,"aucs.csv"))
ys <- data.table(ys)

for(i in is){
  inh <- inhs[i,inhibitor]
  fname <- inhs[i, filename]
  
  y <- ys[inhibitor == inh]
  y <- y[lab_id != "14-00800"]
  
  dt <- merge(y, dataExp, by="lab_id", all.x=TRUE)
  dt[, lab_id:=NULL]
  dt[, inhibitor:=NULL]
  
  #####
  pvals <- data.table("var"=rep("",63995),"pvalue"=rep(1,63995))
  j <- 1
  for(variable in names(dt)){
    if(variable == "auc"){
      next
    }
    formula <- paste0("auc ~ ",variable)
    lmmodel <- lm(formula, data=dt)
    pvals[j, var := variable]
    
    if(nrow(summary(lmmodel)$coefficients) == 2){
      pval <- summary(lmmodel)$coefficients[2,4]
      pvals[j, pvalue := pval]
    }
    j <- j + 1
    if(j %% 1000 == 0){
      print(j)
    }
  }
  
  pvals <- pvals[order(pvalue)]
  filename <- paste0(fname,"_feature_pvals.csv")
  write.csv(pvals,paste0(ROOT_DIR,filename),row.names = FALSE,quote = FALSE)
  
}


