############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
# to load the package	
library(EdSurvey)	
library(Dire)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat",	
                            package = "NAEPprimer"))	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
summary2(sdf, "composite")	
############################################### Slide 8	
summary2(sdf, "composite", weightVar = NULL)	
############################################### Slide 9	
summary2(sdf, "b017451")	
############################################### Slide 10	
summary2(sdf, "b017451", omittedLevels = TRUE)	
############################################### Slide 11	
############################################### Slide 12	
es1 <- edsurveyTable(composite ~ dsex + b017451, data = sdf)	
library(knitr)	
library(kableExtra)	
kable(es1$data, format="html") %>%	
  kable_styling(font_size = 16) %>%	
  scroll_box(width="100%", height = "30%")	
############################################### Slide 13	
es2 <- edsurveyTable(composite ~ dsex + b017451, data = sdf, pctAggregationLevel = 0)	
library(knitr)	
library(kableExtra)	
kable(es2$data, format="html") %>%	
  kable_styling(font_size = 16) %>%	
  scroll_box(width="100%", height = "75%")	
############################################### Slide 14	
############################################### Slide 15	
edexercise <- edsurveyTable(composite ~ iep + b013801,	
                            weightVar = 'origwt', data = sdf)	
edexercise	
############################################### Slide 16	
############################################### Slide 17	
############################################### Slide 18	
############################################### Slide 19	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
lm1 <- lm.sdf(composite ~ dsex + b013801,	
              weightVar = 'origwt', data = sdf)	
summary(lm1)	
############################################### Slide 20	
############################################### Slide 21	
lmexercise2 <- lm.sdf(composite ~ b017101 + b018201,	
                      weightVar = 'origwt', data = sdf)	
summary(lmexercise2)	
############################################### Slide 22	
############################################### Slide 23	
############################################### Slide 24	
mmlA <- mml.sdf(composite ~ dsex + b013801, data=sdf, weightVar='origwt', idVar="ROWID", multiCore=TRUE)	
summary(mmlA)	
############################################### Slide 25	
sdf2 <- drawPVs(sdf, mmlA, npv=20L)	
############################################### Slide 26	
lm2 <- lm.sdf(composite_dire ~ b013801, data=sdf2)	
summary(lm2)	
############################################### Slide 27	
lm1a <- lm.sdf(composite ~ b018201, data=sdf2)	
summary(lm1a)$coefmat	
lm1b <- lm.sdf(composite_dire ~ b018201, data=sdf2)	
summary(lm1b)$coefmat	
############################################### Slide 28	
############################################### Slide 29	
require(NAEPDataSimulation)	
simNAEP <- NAEPDataSimulation::NAEPlikeDt	
dim(simNAEP)	
simNAEP[1:6,1:6]	
############################################### Slide 30	
############################################### Slide 31	
url="https://api.census.gov/data/2019/acs/acs5?get=NAME,B06011_001E&for=zip%20code%20tabulation%20area:*&in=state:*"	
temp <- tempfile()	
download.file(url , temp)	
AcsDt <- read.table(temp, sep=",",header = TRUE)	
unlink(temp)	
head(AcsDt)	
############################################### Slide 32	
#remove the opening bracket on the first column	
AcsDt[,1] <- gsub(pattern="\\[", replacement="", x= AcsDt[,1])	
#remove the last empty column	
AcsDt$X <- NULL	
#remove the bracket from the last column	
AcsDt[,ncol(AcsDt)] <- gsub(pattern="\\]", replacement="", x= AcsDt[,ncol(AcsDt)])	
AcsDt$B06011_001E[AcsDt$B06011_001E==-666666666] <- NA	
AcsDt$B06011_001E <- as.numeric(AcsDt$B06011_001E)	
############################################### Slide 33	
head(AcsDt)	
tail(AcsDt)	
############################################### Slide 34	
linkedData <- merge(simNAEP, AcsDt, by.x = "zip", 	
                    by.y = "zip.code.tabulation.area.", all.x = TRUE)	
linkedData$B06011_001E_10K <- linkedData$B06011_001E/10000	
linkedData[1:6,1:8]	
linkedData[1500:1506,344:350]	
############################################### Slide 35	
require(NAEPirtparams)	
param <- NAEPirtparams::parameters	
item_par <- param[param$level == 8 & param$subject == "Mathematics" & param$year == 2015, ]	
paramTabs <- naepParamTabs(item_par)	
polyParamTab <- paramTabs$polyParamTab	
dichotParamTab <- paramTabs$dichotParamTab	
dichotParamTab$test <- 'composite'	
polyParamTab$test <- 'composite'	
polyParamTab$scorePoints <- apply(polyParamTab[,c('d1','d2','d3','d4','d5')], 1, function(x) 5 - sum(is.na(x)))	
############################################### Slide 36	
itemNames <- c(dichotParamTab$ItemID, polyParamTab$ItemID)	
stuItems <- simNAEP[,c("idVar", itemNames)]	
stuItems <- reshape(simNAEP[,c("idVar", itemNames)], idvar = "idVar", direction = "long", v.names = "score",	
                    timevar = "key", times = itemNames,	
                    varying = itemNames)	
############################################### Slide 37	
transf <- NAEPirtparams::transformations	
transFilter <- transf[transf$level== 8 & transf$year== 2015 & transf$subject== "Mathematics", ]	
testScale <- transFilter[,c('subtest','location','scale', 'subtestWeight')]	
testScale <- testScale[!is.na(testScale$subtestWeight),]	
testScale	
############################################### Slide 38	
fit <- mml(composite ~  dsex + pared,	
             stuItems = stuItems,	
             stuDat = simNAEP,	
             idVar = "idVar",	
             dichotParamTab = dichotParamTab,	
             polyParamTab =  polyParamTab,	
             testScale = testScale,	
             strataVar="repgrp1", PSUVar="jkunit",	
             weightVar = "origwt",	
             fast = TRUE,	
             multiCore = FALSE,	
             verbose=2)	
############################################### Slide 39	
summary(fit)	
############################################### Slide 40	
	
fit$testScale$test <- 'composite'	
	
############################################### Slide 41	
	
#PVs <- drawPVs(fit, npv = 10)	
	
############################################### Slide 42	
#PVs$data[1:5, 1:7]	
############################################### Slide 43	
############################################### Slide 44	
############################################### Slide 45	
############################################### Slide 46	
vignette("introduction", package="EdSurvey")	
help(package = "EdSurvey")	
############################################### Slide 47	
############################################### Slide 48	
