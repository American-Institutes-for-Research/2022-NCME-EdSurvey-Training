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
############################################### Slide 16	
edexercise <- edsurveyTable(composite ~ iep + b013801,	
                            weightVar = 'origwt', data = sdf)	
edexercise	
############################################### Slide 17	
############################################### Slide 18	
############################################### Slide 19	
############################################### Slide 20	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
lm1 <- lm.sdf(composite ~ dsex + b013801,	
              weightVar = 'origwt', data = sdf)	
summary(lm1)	
############################################### Slide 21	
############################################### Slide 22	
lmexercise2 <- lm.sdf(composite ~ b017101 + b018201,	
                      weightVar = 'origwt', data = sdf)	
summary(lmexercise2)	
############################################### Slide 23	
############################################### Slide 24	
############################################### Slide 25	
mmlA <- mml.sdf(composite ~ dsex + b013801, data=sdf, weightVar='origwt', idVar="ROWID", multiCore=TRUE)	
summary(mmlA)	
############################################### Slide 26	
sdf2 <- drawPVs(sdf, mmlA, npv=20L)	
############################################### Slide 27	
lm2 <- lm.sdf(composite_dire ~ b013801, data=sdf2)	
############################################### Slide 28	
summary(lm2)	
############################################### Slide 29	
lm1a <- lm.sdf(composite ~ b018201, data=sdf2)	
summary(lm1a)$coefmat	
lm1b <- lm.sdf(composite_dire ~ b018201, data=sdf2)	
summary(lm1b)$coefmat	
############################################### Slide 30	
############################################### Slide 31	
mmlExercise1 <- mml.sdf(algebra ~ b018101 + m815401 + m815501, data = sdf)	
summary(mmlExercise1)	
############################################### Slide 32	
############################################### Slide 33	
############################################### Slide 34	
require(NCESDatalike)	
#Simulated NAEP-like	
sNl <- readNAEP(system.file("extdata/data", "M46NT2PM.dat", 	
                            package = "NCESDatalike"))	
cd <- showCodebook(sNl)	
############################################### Slide 35	
############################################### Slide 36	
url="https://api.census.gov/data/2019/acs/acs5?get=NAME,B06011_001E&for=zip%20code%20tabulation%20area:*&in=state:*"	
temp <- tempfile()	
download.file(url , temp)	
AcsDt <- read.table(temp, sep=",",header = TRUE)	
unlink(temp)	
head(AcsDt)	
############################################### Slide 37	
#remove the opening bracket on the first column	
AcsDt[,1] <- gsub(pattern="\\[", replacement="", x= AcsDt[,1])	
#remove the last empty column	
AcsDt$X <- NULL	
#remove the bracket from the last column	
AcsDt[,ncol(AcsDt)] <- gsub(pattern="\\]", 	
                            replacement="", x= AcsDt[,ncol(AcsDt)])	
AcsDt$B06011_001E[AcsDt$B06011_001E==-666666666] <- NA	
AcsDt$B06011_001E <- as.numeric(AcsDt$B06011_001E)	
AcsDt$B06011_001E_10K <- AcsDt$B06011_001E/10000	
############################################### Slide 38	
head(AcsDt)	
tail(AcsDt)	
############################################### Slide 39	
items <- cd$variableName[grep("item m", cd$Labels)]	
simdf <- EdSurvey::getData(sNl, varnames = c("idvar","dsex", "pared", "zip", 	
                                             "repgrp1", "jkunit", "origwt", 	
                                             items), omittedLevels = FALSE)	
linkedData <- merge(simdf, AcsDt, by.x = "zip", 	
                    by.y = "zip.code.tabulation.area.", all.x = TRUE)	
linkAtt <- rebindAttributes(linkedData, sNl)	
############################################### Slide 40	
fit <- mml.sdf(composite ~ B06011_001E_10K + dsex + pared, 	
               data = linkAtt, weightVar='origwt', idVar="idvar")	
############################################### Slide 41	
summary(fit)	
############################################### Slide 42	
'%!in%' <- function(x,y)!('%in%'(x,y))	
fit$sCard <- fit$sCard[fit$sCard$key %!in% 	
                         c("m152602", "m2372cl", "m3498cl"),]	
PVs <- drawPVs(linkAtt, fit, npv = 20L)	
	
############################################### Slide 43	
PVs[1:5,c("algebra_dire1", "composite_dire1")]	
############################################### Slide 44	
############################################### Slide 45	
############################################### Slide 46	
url1 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,B10063_002E&for=zip%20code%20tabulation%20area:*&in=state:*"	
url2 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,B19001_017E&for=zip%20code%20tabulation%20area:*&in=state:*"	
url3 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,B27001_014E&for=zip%20code%20tabulation%20area:*&in=state:*"	
url4 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,B28010_007E&for=zip%20code%20tabulation%20area:*&in=state:*"	
url5 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,B28011_008E&for=zip%20code%20tabulation%20area:*&in=state:*"	
url6 = "https://api.census.gov/data/2019/acs/acs5?get=NAME,B01001_001E,C16001_002E&for=zip%20code%20tabulation%20area:*&in=state:*"	
############################################### Slide 47	
temp <- tempfile()	
download.file(url6 , temp)	
AcsDt6 <- read.table(temp, sep=",",header = TRUE)	
unlink(temp)	
head(AcsDt6)	
############################################### Slide 48	
AcsDt6[,1] <- gsub(pattern="\\[", replacement="", x= AcsDt6[,1])	
AcsDt6$X <- NULL	
AcsDt6[,ncol(AcsDt6)] <- gsub(pattern="\\]", 	
                              replacement="", 	
                              x= AcsDt6[,ncol(AcsDt6)])	
summary(AcsDt6)	
############################################### Slide 49	
AcsDt6$EngSpeakers <- AcsDt6$C16001_002E/AcsDt6$B01001_001E	
summary(AcsDt6$EngSpeakers)	
############################################### Slide 50	
linkedData6 <- merge(simdf, AcsDt6, by.x = "zip", 	
                    by.y = "zip.code.tabulation.area.", 	
                    all.x = TRUE)	
linkAtt6 <- rebindAttributes(linkedData6, sNl)	
############################################### Slide 51	
fitSR <- mml.sdf(composite ~ EngSpeakers + dsex + pared, 	
               data = linkAtt6, weightVar='origwt', idVar="idvar")	
############################################### Slide 52	
summary(fitSR)	
############################################### Slide 53	
fitSR$sCard <- fitSR$sCard[fitSR$sCard$key %!in% 	
                             c("m152602", "m2372cl", "m3498cl"),]	
pvSR <- drawPVs(linkAtt6, fitSR, npv= 20L)	
############################################### Slide 54	
############################################### Slide 55	
vignette("introduction", package="EdSurvey")	
help(package = "EdSurvey")	
############################################### Slide 56	
############################################### Slide 57	
