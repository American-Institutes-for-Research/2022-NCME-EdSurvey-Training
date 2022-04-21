# 2022-NCME-EdSurvey-Training

## Install R and RStudio

https://rstudio-education.github.io/hopr/starting.html

## Install EdSurvey and package dependencies

1) install Dire 2.0.1

`install.packages("Dire")`

you may need to get `rtools`.

2) then install EdSurvey from GitHub
```
install.packages("devtools")
devtools::install_github("American-Institutes-for-Research/edsurvey")
```

3) Install NCESDatalike from location of `NCESDatalike_1.0.0.tar.gz` file

```
install.packages("lsasim")
# the tar.gz location may differ depending on your R working directory
install.packages("NCESDatalike_1.0.0.tar.gz", repos = NULL, type = "source")
```
