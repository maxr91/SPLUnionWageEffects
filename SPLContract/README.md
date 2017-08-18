[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **CRCmapev** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

**Name of Quantlet:** SPLContract

**Published in:**     Union Wage Effects in Germany - Implications for the Wage Distribution

**Description:**      Second part of the data preparation.

**Keywords:**         data preparation, data import, create dummies, convert dummies

**Author:**           Felix Boenisch, Nicole Hermann, Max Reinhardt

**See also:**         SPLDummyRespond, SPLFreqPlot, SPLQuantPlot, SPLSumTable, SPLRegAnalysis

**Submitted:** friday 18.08.2017, by Felix Boenisch, Nicole Hermann, Max Reinhardt



```


### R Code:
```r
#create function to calculate how many employees have a union contract or not
contractFunc = function(a,b,c){            #a= company dummy, b= data information vector with dummies, c= choose level
  if (missing(b))
    stop("No data passed to the function")
  if (is.numeric(a)!= TRUE & is.numeric(b)!= TRUE  )
    stop("numeric data needed")
  if (missing(c))
    stop("No level selected")
  temp = table(a)
  cumtemp = cumsum(temp)                   #calculate cummulative sums for later addressing the vector
  cumtemp = append(cumtemp,0,after =0)     #need 0 as start value 
  g = nrow(temp) +1
  i = 2                                    #setting counting variables to 1
  j = 1
  k = 1
  q = numeric(length(b))
  for (i in 2:g){
    k = cumtemp[i-1]+1                     #store starting value company x in dat in k
    j = cumtemp[i]                         #store end value company x in dat in j
    p = table(b[k:j])                      #store results in p (how many people have kein Tarifvertrag and so on....)
    q[k:j]  = p[c]                   
    i = i+1
  }
  return(q)
}

noTariff = contractFunc(dat$ef1, dat$ef8, 1) #call function contractFunc
SCTariff = contractFunc(dat$ef1, dat$ef8, 2)
FCTariff = contractFunc(dat$ef1, dat$ef8, 3)
dat["noTariff"] = noTariff                #add to data frame
dat["SCTariff"] = SCTariff
dat["FCTariff"] = FCTariff

#create shares
shareFC = FCTariff/respond
shareSC = SCTariff/respond
dat["shareFC"] = shareFC            #add to data frame
dat["shareSC"] = shareSC

#create dummy variables for no labor contract, sectoral contract and firm-level contract
noTariffDummy = dummyFunc(dat$ef8 , 1)
SCTariffDummy = dummyFunc(dat$ef8 , 2)
FCTariffDummy = dummyFunc(dat$ef8 , 3)
dat["noTariffDummy"] = noTariffDummy          #add to data frame
dat["SCTariffDummy"] = SCTariffDummy
dat["FCTariffDummy"] = FCTariffDummy

#create interaction terms
shareFCFC = shareFC*FCTariffDummy
shareSCSC = shareSC*SCTariffDummy
dat["shareFCFC"] = shareFCFC          #add to data frame
dat["shareSCSC"] = shareSCSC

#create variables age squared and experienece squared 
agesq = dat$ef40*dat$ef40
expsq = dat$ef41*dat$ef41
dat["agesq"] = agesq         #add to data frame
dat["expsq"] = expsq

#define wage and lnwage
wage   = ifelse(dat$ef18+dat$ef20 == 0, NA, (dat$ef21+dat$ef22)/(dat$ef18+dat$ef20))
lnWage = log(wage)
dat["wage"]   = wage              #add to data frame
dat["lnWage"] = lnWage


```