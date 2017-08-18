#install and load data.table-package
install.packages("data.table")    
library(data.table)  

#convert data frame into data table
dat = data.table(dat)             

#create group with 3 factors
dat[SCTariffDummy == 1, Group := factor(1)]   
dat[FCTariffDummy == 1, Group := factor(2)]
dat[noTariffDummy == 1, Group := factor(3)]

dat[, Group := factor(Group, labels = c("SC", "FC", "IC"))]     #name each factor

#how many observations in total are in the data table (without nas)
sum = dat[!is.na(Group), .N]                                    

#calculate mean and standard deviation of lnwage for each group and each gender
lnWageSummary = dat[!is.na(Group), .(LogHourlyWageMean = mean(lnWage, na.rm = T),
                                     LogHourlyWageSD = sd(lnWage, na.rm = T)), by = .(ef10, Group)] 
#order variables according to gender and group
lnWageSummary = lnWageSummary[order(ef10, Group)]              

#calculate total mean (no discrimination between gender)                      
lnWageSummaryOverall = dat[!is.na(Group), .(LogHourlyWageMean = mean(lnWage, na.rm = T),
                                      LogHourlyWageSD = sd(lnWage, na.rm = T)), by = .(Group)]

lnWageSummaryOverall = lnWageSummaryOverall[order(Group)]

#calculate frequencys for every group and by gender
mtable = table(dat$Group, dat$ef10)                                            

#calculate employee share (no discrimination between gender)
TotalEmpolyeeShare = dat[!is.na(Group), .(Share = .N/sum), by = .(Group)]   

#order values
TotalEmpolyeeShare = TotalEmpolyeeShare[order(Group)]                          

lnWageSummaryTotal = data.frame(Regime             = c("SC", "FC", "IC"),                                          #create full table with before calculcated values 
                                MaleEmpolyeeShare       = prop.table(mtable, 2)[, 1],                              #calculate proportion for employee share (male)
                                MaleLogHourlyWageMean   = lnWageSummary[ef10 == "m?nnlich", LogHourlyWageMean],    #put male wage mean value 
                                MaleLogHourlyWageSD     = lnWageSummary[ef10 == "m?nnlich", LogHourlyWageSD],      #put male standard deviation 
                                FemaleEmpolyeeShare     = prop.table(mtable, 2)[, 2],                              #calculate proportion for employee share (female)
                                FemaleLogHourlyWageMean = lnWageSummary[ef10 == "weiblich", LogHourlyWageMean],    #put male wage mean value
                                FemaleLogHourlyWageSD   = lnWageSummary[ef10 == "weiblich", LogHourlyWageSD],      #put male standard deviation
                                TotalEmpolyeeShare      = TotalEmpolyeeShare$Share,                                #put TotalEmployee share here
                                TotalLogHourlyWageMean  = lnWageSummaryOverall$LogHourlyWageMean,                  #put Total mean wage here
                                TotalLogHourlyWageSD    = lnWageSummaryOverall$LogHourlyWageSD,                    #put total standard deviation here
                                stringsAsFactors        = FALSE)

#calculate Total line of before created variables
Total = c("Total",          
          sum(lnWageSummaryTotal$MaleEmpolyeeShare),                              #sum over male shares (=1)
          dat[!is.na(Group) & ef10 == "m?nnlich", mean(lnWage, na.rm = T)],       #build total of mean log wage male
          dat[!is.na(Group) & ef10 == "m?nnlich", sd(lnWage, na.rm = T)],         #build total of standard deviation male
          sum(lnWageSummaryTotal$FemaleEmpolyeeShare),                            #sum over female shares (=1)
          dat[!is.na(Group) & ef10 == "weiblich", mean(lnWage, na.rm = T)],       #build total of mean log wage female
          dat[ef10 == "weiblich", sd(lnWage, na.rm = T)],                         #build total of standard deviation female
          sum(lnWageSummaryTotal$TotalEmpolyeeShare),                             #sum over all shares (=1 , no discromination in gender)
          dat[!is.na(Group), mean(lnWage, na.rm = T)],                            #build total of mean log wage of all observations
          dat[!is.na(Group), sd(lnWage, na.rm = T)])                              #build total of standard deviation of all observations

lnWageSummaryTotal        = rbind(lnWageSummaryTotal, Total)   #combine total table and summary table
lnWageSummaryTotal[,2:10] = rapply(lnWageSummaryTotal[,2:10], as.numeric)
lnWageSummaryTotal        = rapply(object = lnWageSummaryTotal, f = round, classes = "numeric", how = "replace", digits = 2) #round results

dat = data.frame(dat)   #put data back into data frame

#install and load xtable-package
install.packages("xtable")
library(xtable)
#print file with latex code
print(xtable(lnWageSummaryTotal, type = "latex"), file = "covRegimeandLNWages.tex") 
