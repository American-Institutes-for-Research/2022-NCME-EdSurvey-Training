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
mmlA <- mml.sdf(composite ~ dsex + b013801, data=sdf)	
############################################### Slide 25	
summary(mmlA)	
############################################### Slide 26	
sdf2 <- drawPVs(sdf, mmlA, npv=20L)	
############################################### Slide 27	
lm2 <- lm.sdf(composite_dire ~ b013801, data=sdf2)	
summary(lm2)	
############################################### Slide 28	
lm1a <- lm.sdf(composite ~ b018201, data=sdf2)	
summary(lm1a)$coefmat	
lm1b <- lm.sdf(composite_dire ~ b018201, data=sdf2)	
summary(lm1b)$coefmat	
############################################### Slide 29	
############################################### Slide 30	
mmlExercise1 <- mml.sdf(algebra ~ b018101 + m815401 + m815501, data = sdf)	
summary(mmlExercise1)	
############################################### Slide 31	
############################################### Slide 32	
############################################### Slide 33	
require(NAEPDataSimulation)	
simNAEP <- NAEPDataSimulation::NAEPlikeDt	
dim(simNAEP)	
simNAEP[1:6,1:6]	
############################################### Slide 34	
############################################### Slide 35	
url="https://api.census.gov/data/2019/acs/acs5?get=NAME,B06011_001E&for=zip%20code%20tabulation%20area:*&in=state:*"	
temp <- tempfile()	
download.file(url , temp)	
AcsDt <- read.table(temp, sep=",",header = TRUE)	
unlink(temp)	
head(AcsDt)	
############################################### Slide 36	
#remove the opening bracket on the first column	
AcsDt[,1] <- gsub(pattern="\\[", replacement="", x= AcsDt[,1])	
#remove the last empty column	
AcsDt$X <- NULL	
#remove the bracket from the last column	
AcsDt[,ncol(AcsDt)] <- gsub(pattern="\\]", replacement="", x= AcsDt[,ncol(AcsDt)])	
AcsDt$B06011_001E[AcsDt$B06011_001E==-666666666] <- NA	
AcsDt$B06011_001E <- as.numeric(AcsDt$B06011_001E)	
############################################### Slide 37	
head(AcsDt)	
tail(AcsDt)	
############################################### Slide 38	
linkedData <- merge(simNAEP, AcsDt, by.x = "zip", 	
                    by.y = "zip.code.tabulation.area.", all.x = TRUE)	
linkedData$B06011_001E_10K <- linkedData$B06011_001E/10000	
linkedData[1:6,1:8]	
linkedData[1:6,344:350]	
############################################### Slide 39	
############################################### Slide 40	
require(NAEPirtparams)	
param <- NAEPirtparams::parameters	
item_par <- param[param$level == 8 & param$subject == "Mathematics" & param$year == 2015, ]	
head(item_par)	
############################################### Slide 41	
paramTabs <- naepParamTabs(item_par)	
polyParamTab <- paramTabs$polyParamTab	
dichotParamTab <- paramTabs$dichotParamTab	
dichotParamTab$test <- 'composite'	
polyParamTab$test <- 'composite'	
polyParamTab$scorePoints <- apply(polyParamTab[,c('d1','d2','d3','d4','d5')], 1, function(x) 5 - sum(is.na(x)))	
head(polyParamTab)	
############################################### Slide 42	
transf <- NAEPirtparams::transformations	
transFilter <- transf[transf$level== 8 & transf$year== 2015 & transf$subject== "Mathematics", ]	
head(transFilter)	
	
############################################### Slide 43	
testScale <- transFilter[,c('subtest','location','scale', 'subtestWeight')]	
testScale <- testScale[!is.na(testScale$subtestWeight),]	
testScale$test <- 'composite'	
testScale	
############################################### Slide 44	
itemNames <- c(dichotParamTab$ItemID, polyParamTab$ItemID)	
stuItems <- simNAEP[,c("idVar", itemNames)]	
stuItems <- reshape(simNAEP[,c("idVar", itemNames)], idvar = "idVar", direction = "long", v.names = "score",	
                    timevar = "key", times = itemNames,	
                    varying = itemNames)	
############################################### Slide 45	
head(stuItems)	
############################################### Slide 46	
fit <- mml(composite ~  B06011_001E_10K + dsex + pared,	
             stuItems = stuItems,	
             stuDat = linkedData,	
             idVar = "idVar",	
             dichotParamTab = dichotParamTab,	
             polyParamTab =  polyParamTab,	
             testScale = testScale,	
             strataVar="repgrp1", PSUVar="jkunit",	
             weightVar = "origwt",	
             fast = TRUE,	
             multiCore = FALSE,	
             verbose=2)	
############################################### Slide 47	
summary(fit)	
############################################### Slide 48	
PVs <- drawPVs(fit, npv = 20L)	
############################################### Slide 49	
PVs$data[1:5, 1:7]	
############################################### Slide 50	
############################################### Slide 51	
############################################### Slide 52	
url1 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,B10063_002E&for=zip%20code%20tabulation%20area:*&in=state:*"	
url2 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,B19001_017E&for=zip%20code%20tabulation%20area:*&in=state:*"	
url3 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,B27001_014E&for=zip%20code%20tabulation%20area:*&in=state:*"	
url4 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,B28010_007E&for=zip%20code%20tabulation%20area:*&in=state:*"	
url5 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,B28011_008E&for=zip%20code%20tabulation%20area:*&in=state:*"	
url6 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,C16001_002E&for=zip%20code%20tabulation%20area:*&in=state:*"	
############################################### Slide 53	
temp <- tempfile()	
download.file(url6 , temp)	
AcsDt <- read.table(temp, sep=",",header = TRUE)	
unlink(temp)	
head(AcsDt)	
############################################### Slide 54	
AcsDt[,1] <- gsub(pattern="\\[", replacement="", x= AcsDt[,1])	
AcsDt$X <- NULL	
AcsDt[,ncol(AcsDt)] <- gsub(pattern="\\]", replacement="", x= AcsDt[,ncol(AcsDt)])	
summary(AcsDt)	
############################################### Slide 55	
AcsDt$EngSpeakers <- AcsDt$C16001_002E/AcsDt$B01001_001E	
summary(AcsDt$EngSpeakers)	
############################################### Slide 56	
linkedData <- merge(simNAEP, AcsDt, by.x = "zip", 	
                    by.y = "zip.code.tabulation.area.", all.x = TRUE)	
dim(linkedData)	
linkedData[1:6,1:8]	
linkedData[1:6,344:350]	
############################################### Slide 57	
require(NAEPirtparams)	
require(NAEPDataSimulation)	
param <- NAEPirtparams::parameters	
item_par <- param[param$level == 8 & param$subject == "Mathematics" & param$year == 2015, ]	
polyParamTab <- naepParamTabs(item_par)$polyParamTab	
dichotParamTab <- naepParamTabs(item_par)$dichotParamTab	
polyParamTab$scorePoints <- apply(polyParamTab[,c('d1','d2','d3','d4','d5')], 1, function(x) 5 - sum(is.na(x)))	
transf <- NAEPirtparams::transformations	
transFilter <- transf[transf$level== 8 & transf$year== 2015 & transf$subject== "Mathematics", ]	
testScale <- transFilter[,c('subtest','location','scale', 'subtestWeight')]	
testScale$test <- polyParamTab$test <- dichotParamTab$test <- 'composite'	
itemNames <- c(dichotParamTab$ItemID, polyParamTab$ItemID)	
stuItems <- simNAEP[,c("idVar", itemNames)]	
stuItems <- reshape(simNAEP[,c("idVar", itemNames)], idvar = "idVar", direction = "long", v.names = "score",	
                    timevar = "key", times = itemNames, varying = itemNames)	
############################################### Slide 58	
fit <- mml(composite ~  EngSpeakers + dsex,	
             stuItems = stuItems, stuDat = linkedData,	
             idVar = "idVar", dichotParamTab = dichotParamTab,	
             polyParamTab =  polyParamTab,	
             testScale = testScale,	
             strataVar="repgrp1", PSUVar="jkunit",	
             weightVar = "origwt", verbose=2)	
############################################### Slide 59	
summary(fit)	
############################################### Slide 60	
############################################### Slide 61	
vignette("introduction", package="EdSurvey")	
help(package = "EdSurvey")	
############################################### Slide 62	
############################################### Slide 63	
