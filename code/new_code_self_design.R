##FINAL MODEL (currently in paper) (stage2, grade3 (three groups each))
setwd('/Users/zhangh24/GoogleDrive/Bladder')
#library(bc2)
library(devtools)
#install_github("andrewhaoyu/bc2")
library(bc2)
dieselbladder <- read.csv("./data/dieselbladder.csv")
y <- as.matrix(cbind(dieselbladder$CACO,dieselbladder$stage2,dieselbladder$grade3))

colnames(y)[1:3] <- c("CACO", "stage2", "grade3")

x <- cbind(dieselbladder$dieselscore2,dieselbladder$age6574,dieselbladder$age75,dieselbladder$SITE,dieselbladder$HIGHRISK,dieselbladder$POSSIBLE,dieselbladder$smoke_c,dieselbladder$smoke_f,dieselbladder$smoke_o)
colnames(x)[1:9] <- c("dieselscore",  "covar1", "covar2", "covar3", "covar4", "covar5", "covar6", "covar7", "covar8")
z.standard <- GenerateZstandard(y)
model <- TwoStageModel(y = y,additive = x,
                       missingTumorIndicator = 888)
model[[7]]
##ALTERNATE MODEL (stage2, grade2 (three groups for stage, two groups for grade)


y <- as.matrix(cbind(dieselbladder$CACO,dieselbladder$stage2,dieselbladder$grade2))
colnames(y)[1:3] <- c("CACO", "stage2", "grade2")


x <- cbind(dieselbladder$dieselscore2,dieselbladder$age6574,dieselbladder$age75,dieselbladder$SITE,dieselbladder$HIGHRISK,dieselbladder$POSSIBLE,dieselbladder$smoke_c,dieselbladder$smoke_f,dieselbladder$smoke_o)
colnames(x)[1:9] <- c("dieselscore",  "covar1", "covar2", "covar3", "covar4", "covar5", "covar6", "covar7", "covar8")
z.standard <- GenerateZstandard(y)
model <- TwoStageModel(y = y,additive = x,
                       missingTumorIndicator = 888)




################self design model
################code stage: 0, 1, 2
################code grade: 0, 1, 2
################group1: stage 0 grade 0
################group2: stage 0 grade 1
################group3: stage 0 grade 2; stage 1 grade 0; stage1 grade 1; stage1 grade 2
################group4: stage 2 grade 1, stage 2 grade 2
################ the subtype stage 2 grade 0 will be removed due to limited sample size (ps. I could also include them, but the function default was set to remove the small sample size subtypes at the beginning. The odds ratio shouldn't change much.)
y <- as.matrix(cbind(dieselbladder$CACO,dieselbladder$stage2,dieselbladder$grade3))
colnames(y)[1:3] <- c("CACO", "stage2", "grade3")
z.standard <- GenerateZstandard(y)
M <- nrow(z.standard)
n.group <- 4
z.design <- matrix(0,M,n.group)
colnames(z.design)
####group1
idx.1 <- which(z.standard[,1]==0&z.standard[,2]==0)
z.design[idx.1,1] <- 1
####group2
idx.2 <- which(z.standard[,1]==0&z.standard[,2]==1)
z.design[idx.2,2] <- 1
####group3
idx.3 <- which((z.standard[,1]==0&z.standard[,2]==2)|
                 z.standard[,1]==1)
z.design[idx.3,3] <- 1
####group 4
idx.4 <- which((z.standard[,1]==2&z.standard[,2]==1)|
  (z.standard[,1]==2&z.standard[,2]==2))
z.design[idx.4,4] <- 1
rowSums(z.design)
colnames(z.design) <- paste0("group",c(1:4))
model.3 <- EMmvpolySelfDesign(y,x.self.design = x, z.design=z.design)
############second stage effect: the group vs control log odds ratio and 95%CI
model.3[[4]]
###########first stage effect: the subtype vs control log odds ratio and 95% CI
model.3[[7]]
