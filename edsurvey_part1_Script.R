############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
############################################### Slide 8	
############################################### Slide 9	
############################################### Slide 10	
############################################### Slide 11	
############################################### Slide 12	
############################################### Slide 13	
############################################### Slide 14	
############################################### Slide 15	
# this line is not executed	
x <- 12	
x	
j <- 12	
J	
############################################### Slide 16	
?mean	
############################################### Slide 17	
colors <- c("red", "green", "blue")	
colors	
numbers <- c(1, 2, 3)	
numbers	
############################################### Slide 18	
mean(x = numbers)	
mean(numbers)	
############################################### Slide 19	
#install Dire 2.0.1	
# you may need to get rtools	
install.packages("Dire")	
	
# then install devtools and EdSurvey from GitHub	
install.packages("devtools")	
devtools::install_github("American-Institutes-for-Research/edsurvey")	
	
#Install NCESDatalike from location of NCESDatalike_1.0.0.tar.gz file	
install.packages("lsasim")	
# the tar.gz location may differ depending on your R working directory	
# to load the package	
library(EdSurvey)	
############################################### Slide 20	
vignette("introduction", package="EdSurvey")	
help(package = "EdSurvey")	
############################################### Slide 21	
############################################### Slide 22	
############################################### Slide 23	
############################################### Slide 24	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
############################################### Slide 25	
math17 <- readNAEP("//path_to_directory/Data/M48NT2AT.dat")	
############################################### Slide 26	
############################################### Slide 27	
############################################### Slide 28	
############################################### Slide 29	
############################################### Slide 30	
############################################### Slide 31	
############################################### Slide 32	
############################################### Slide 33	
############################################### Slide 34	
############################################### Slide 35	
############################################### Slide 36	
print(sdf)	
############################################### Slide 37	
dim(sdf)	
############################################### Slide 38	
colnames(sdf)	
############################################### Slide 39	
searchSDF("education", sdf)	
searchSDF("b003501", sdf, levels = TRUE)	
searchSDF("", sdf)	
############################################### Slide 40	
levelsSDF("b018201", sdf)	
############################################### Slide 41	
showCodebook(sdf)	
View(showCodebook(sdf))	
############################################### Slide 42	
showPlausibleValues(sdf)	
showPlausibleValues(sdf, verbose = TRUE)	
############################################### Slide 43	
showWeights(sdf)	
showWeights(sdf, verbose = TRUE)	
############################################### Slide 44	
############################################### Slide 45	
############################################### Slide 46	
############################################### Slide 47	
############################################### Slide 48	
############################################### Slide 49	
gddat <- getData(sdf, varnames = c('dsex', 'sdracem', 'b018201', 'b017451',	
                                   'composite', 'geometry', 'origwt'),	
              addAttributes = TRUE, omittedLevels = FALSE)	
############################################### Slide 50	
# Note: head returns the first 6 rows of a data frame	
head(gddat)	
############################################### Slide 51	
gddat <- getData(sdf, varnames = c('dsex', 'sdracem', 'b018201', 'b017451',	
                                   'composite', 'geometry', 'origwt'),	
              addAttributes = TRUE, omittedLevels = FALSE)	
############################################### Slide 52	
subsetSDF <- subset(sdf, dsex %in% c("Male"))	
dim(sdf)	
dim(subsetSDF)	
############################################### Slide 53	
sdf2 <- recode.sdf(sdf, recode =	
                     list(b017451 = list(from = c("Never or hardly ever", "Once every few weeks"),	
                                         to = c("Infrequently")),	
                          b017451 = list(from = c("Every day"),	
                                        to = c("Frequently")))	
                   )	
searchSDF("b017451", sdf2, levels = TRUE)	
############################################### Slide 54	
sdf2 <- rename.sdf(sdf2, oldnames = "b017451",	
                   newnames = "studytalkfrequency")	
searchSDF("studytalkfrequency", sdf2, levels = TRUE)	
