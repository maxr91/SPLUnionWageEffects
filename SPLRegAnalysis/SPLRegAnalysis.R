#install and load uuqr-package
install.packages("uqr")
library(uqr)

quantile2=c(0.1, 0.25, 0.5, 0.75, 0.9)
modelUnconditionalQR = urq(lnWage ~  SCTariffDummy + shareSC + FCTariffDummy + shareFC + shareFCFC + shareSCSC + ef10 
                           + east + ef9be + ef12be + ef26be + minimumWage + ef9 + educ2 + educ3 + shift + ef40 + agesq 
                           + ef41 + expsq + permanent, data=quantileRegressionData, tau = quantile2 )

#calculate average partial effects for unconditional quantile regression:
modelUnconditionalQRCoef = modelUnconditionalQR[1]
modelUnconditionalQRCoef = as.data.frame(modelUnconditionalQRCoef)

#build data frame with results from unconditional quantile regression
calcAverageCoefUQRSCSCFCFC = data.frame(tau10 = c(modelUnconditionalQRCoef[7, 1], modelUnconditionalQRCoef[7, 1],    
                                                  modelUnconditionalQRCoef[6, 1], modelUnconditionalQRCoef[6, 1]),
                                        tau25 = c(modelUnconditionalQRCoef[7, 2], modelUnconditionalQRCoef[7, 2],
                                                  modelUnconditionalQRCoef[6, 2], modelUnconditionalQRCoef[6, 2]),
                                        tau50 = c(modelUnconditionalQRCoef[7, 3], modelUnconditionalQRCoef[7, 3],
                                                  modelUnconditionalQRCoef[6, 3], modelUnconditionalQRCoef[6, 3]),
                                        tau75 = c(modelUnconditionalQRCoef[7, 4], modelUnconditionalQRCoef[7, 4],
                                                  modelUnconditionalQRCoef[6, 4], modelUnconditionalQRCoef[6, 4]),
                                        tau90 = c(modelUnconditionalQRCoef[7, 5], modelUnconditionalQRCoef[7, 5],
                                                  modelUnconditionalQRCoef[6, 5], modelUnconditionalQRCoef[6, 5]))

#calculate average partial effects
averagePartialEffectUQR = data.frame(Quantiles = c("Sector Contract (SC)", "shareSC", "Firm Contract (FC)", "shareFC"),
                                     tau10 = modelUnconditionalQRCoef[2:5, 1] + (calcAverage * calcAverageCoefUQRSCSCFCFC$tau10),    
                                     tau25 = modelUnconditionalQRCoef[2:5, 2] + (calcAverage * calcAverageCoefUQRSCSCFCFC$tau25),
                                     tau50 = modelUnconditionalQRCoef[2:5, 3] + (calcAverage * calcAverageCoefUQRSCSCFCFC$tau50),
                                     tau75 = modelUnconditionalQRCoef[2:5, 4] + (calcAverage * calcAverageCoefUQRSCSCFCFC$tau75),
                                     tau90 = modelUnconditionalQRCoef[2:5, 5] + (calcAverage * calcAverageCoefUQRSCSCFCFC$tau90))                                                 

#print table in latex code
print(xtable(averagePartialEffectUQR, type = "latex"), file = "averagePartialEffectUQR.tex") 

#calculate confidence intervalls  set for for bootstraping (bigger then 5)
modelUnconditionalQR.BCI = urqCI(modelUnconditionalQR , R=30 , seed=NULL , 
                                 colour=NULL , confidence=NULL , graph=TRUE , 
                                 cluster=NULL , BC=FALSE)
