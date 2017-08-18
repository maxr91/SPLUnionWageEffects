[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **CRCmapev** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: SPLDummyRespond

Published in :     Union Wage Effects in Germany - Implications for the Wage Distribution

Description :      First part of the data preparation.

Keywords :         data preparation, data import, create dummies, convert dummies, remove NA's

Author :           Felix Boenisch, Nicole Hermann, Max Reinhardt

See also :         SPLContract, SPLFreqPlot, SPLQuantPlot, SPLSumTable, SPLRegAnalysis

Submitted :        friday 18.08.2017, by Felix Boenisch, Nicole Hermann, Max Reinhardt


```


### R Code:
```r
#clear the workspace

rm(list=ls())



#install package / load library for importing stata 13 files

install.packages("readstata13")

library(readstata13)



#importing the data into dat

dat = read.dta13("dataset2010.dta", convert.factors = TRUE, generate.factors = FALSE,

                  encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,

                  missing.type = FALSE, convert.dates = TRUE, replace.strl = TRUE,

                  add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL)



#function for calculation respondents per company

respondFunc = function(dat){

  if (missing(dat))

    stop("No data passed to the function")

  if (is.numeric(dat)!= TRUE)

    stop("numeric data needed")

  respond = numeric(length(dat))

  i = 1                                   #setting counting variables to 1

  j = 1

  temp = table(dat)                       #how many different values are in dat

  for (i in 1:length(dat)){               #for every observation

    j = dat[i]                            #store value of dat in j

    respond[i] = temp[j]                  #value of temp[i] is stored in respond[i]

    i = i+1              

  }

  return(respond)

}



respond = respondFunc(dat$ef1)           #call function respondFunc

dat["respond"] = respond                 #add to data frame



#dummyfunction to create dummy variables

dummyFunc = function(dat , x){

  if (missing(dat))

    stop("No data passed to the function")

  if(is.null(levels(dat)))

    stop("No levels found")

  d = as.numeric(dat == levels(dat)[x])      #compare data vector with selected level of data vector, if true, then function writes 1

  return(d)

}



#create eastdummy  0=west

east = dummyFunc(dat$ef4be , 5)  #call function dummyFunc

dat["east"] = east               #add to data frame



#create less classes for education

tempEdu1  = dummyFunc(dat$ef16u2 , 1 )

tempEdu1a = dummyFunc(dat$ef16u2 , 2 )

tempEdu2  = dummyFunc(dat$ef16u2 , 3 )

tempEdu2a = dummyFunc(dat$ef16u2 , 4 )

tempEdu3  = dummyFunc(dat$ef16u2 , 5 )

tempEdu3a = dummyFunc(dat$ef16u2 , 6 )

tempNa    = dummyFunc(dat$ef16u2 , 7 )

tempNa[tempNa == 1] = NA                #add NA's from dataset



#reduce dummy levels from 6 to 3

educ1 = tempEdu1 + tempEdu1a + tempNa   

educ2 = tempEdu2 + tempEdu2a + tempNa

educ3 = tempEdu3 + tempEdu3a + tempNa



dat["educ1"] = educ1                   #add to data frame

dat["educ2"] = educ2

dat["educ3"] = educ3



#create dummy for permanent workers

permanent        = dummyFunc(dat$ef17 , 1 )

dat["permanent"] = permanent         #add to data frame



#define whether someone worked in shifts/at night/...

shift        = as.numeric(dat$ef23 >= 1)

dat["shift"] = shift                    #add to data frame



#create dummy for fulltime workers and reduce levels 

tempFull1 = as.numeric(dat$ef16u1 != "Teilzeitbesch?ftigt - Beamter")

tempFull2 = as.numeric(dat$ef16u1 != "Teilzeitbesch?ftigt - weniger als 18 Std.")

tempFull3 = as.numeric(dat$ef16u1 != "Teilzeitbesch?ftigt - 18 Std. und mehr")

fulltime  = tempFull1+tempFull2+tempFull3 - 2

dat["fulltime"] = fulltime              #add to data frame



#create minimumwage dummy 0=nein 

minimumWage   = as.numeric(dat$ef31be != "nein")

minimumWageNa = dummyFunc(dat$ef31be , 3 )

minimumWageNa[minimumWageNa == 1] = NA           #add NA's from dataset

minimumWage        = minimumWage+minimumWageNa

dat["minimumWage"] = minimumWage + minimumWageNa       #add to data frame

```