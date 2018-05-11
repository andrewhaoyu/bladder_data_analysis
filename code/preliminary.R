library(sas7bdat)
data <- read.sas7bdat("../data/bladderdiesel.sas7bdat")
library(devtools)
install_github("andrewhaoyu/bc2")
library(bc2)
n <- nrow(data)
# STAGE1 <- rep(0,n)
# STAGE2 <- rep(0,n)
# STAGE1[data$STAGE==1] = 1
# STAGE1[is.nan(data$STAGE)] = NA
# STAGE2[data$STAGE==2] = 1
data$GRADE[is.nan(data$GRADE)]=NA
data$STAGE[is.nan(data$STAGE)]= NA
STAGE <- data$STAGE
idx <- which(is.nan(STAGE)&data$CACO==1)
STAGE[idx] = 888
idx <- which(is.nan(STAGE)&data$CACO==0)
STAGE[idx] = NA
table(STAGE,data$CACO,exclude = NULL)
GRADE <- data$GRADE
idx <- which(is.nan(GRADE)&data$CACO==1)
GRADE[idx] = 888
idx <- which(is.nan(GRADE)&data$CACO==0)
GRADE[idx] = NA
table(GRADE,data$CACO,exclude = NULL)
p53 <- data$IHC2P53_CACO
idx <- which(p53==0)
p53[idx] <- NA
idx <- which(p53==1)
p53[idx] <- 0
idx <- which(p53==2)
p53[idx] <- 1
idx <- which(is.nan(p53))
p53[idx] <- 888
table(p53,data$CACO,exclude=NULL)

y <- as.matrix(cbind(data$CACO,p53,GRADE,STAGE))
table(data$CACO,p53,exclude = NULL)
table(data$CACO,STAGE,exclude = NULL)
table(data$CACO,GRADE,exclude = NULL)
colnames(y)[1] = "CACO"

#table(data$RACE_GRP2)
table(data$AGE)
table(data$SITE)
table(data$HIGHRISK)
#table(data$SMOKING)
table(data$POSSIBLE)
table(data$cumAM_ge)
cumAM_ge <- data$cumAM_ge
cumamge1 <- rep(0,n)
cumamge1[cumAM_ge==1] = 1
cumamge2 <- rep(0,n)
cumamge2[cumAM_ge==2] = 1
x <- cbind(cumamge1,cumamge2,data$AGE,data$SITE,
           data$HIGHRISK,data$POSSIBLE)
colnames(x)[3:6] <- c("age","site","highrisk","possible") 
GenerateZstandard(y)
model <- TwoStageModel(y = y,additive = x,
                       missingTumorIndicator = 888)
#cumamge_du <- model.matrix(~cumAM_ge)
