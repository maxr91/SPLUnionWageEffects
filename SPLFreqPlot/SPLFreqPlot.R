#install and load plotrix-package / neccessary to use pyramid.plot
install.packages("plotrix")
library("plotrix")

#function to calculate relative frequencies in % table for variable k with l different characteristics
frequency = function(k, l){            
  if (missing(k))
    stop("No data passed to the function. Variable k has to be defined.")
  if (missing(l))
    stop("No data passed to the function. Variable l has to be defined.")
  
  100*sweep(table(k,l), 2, colSums(table(k,l)), "/")
}  

#function to build population pyramid and store it as pdf
buildpopulation = function(k, l, popname){
  if (missing(popname))
    stop('No data passed to the function. Variable popname has to be defined.
         Please define a plot name such as "populationpyramid.pdf". 
         Use quotation marks, at the beginning and the end of the plot name.')

  pop = frequency(k, l)
  pdf(popname)
  pyramid.plot(pop[,1], pop[,2], labels = rownames(pop), 
               gap = 2, lxcol = "blue", rxcol = "red")
  dev.off() 
}

#subsample with employees covered by the union contract (with FC or SC)
datFCSC = dat[ which(SCTariffDummy == 1 | FCTariffDummy == 1),]

#subsample with employees which ar not covered by the union contract (no FC & no SC)
datNoFCSC = dat[ which(SCTariffDummy == 0 & FCTariffDummy == 0),]

#population pyramid on the whole dataset
buildpopulation(dat$ef41, dat$ef10, "population_all.pdf")

#population pyramid of employees covered by the union contract
buildpopulation(datFCSC$ef41, datFCSC$ef10, "populationFCSC.pdf")

#population pyramid of employees not covered by the union contract
buildpopulation(datNoFCSC$ef41, datNoFCSC$ef10, "population-noFCSC.pdf")

#calculate arithmetic mean
mean(dat$ef41, na.rm = TRUE)          #all population
mean(datFCSC$ef41, na.rm = TRUE)      #subsample with union covered workers
mean(datNoFCSC$ef41, na.rm = TRUE)    #subsample with workers without a union contract

#calculate median
median(dat$ef41, na.rm = TRUE)          #all population
median(datFCSC$ef41, na.rm = TRUE)      #subsample with union covered workers
median(datNoFCSC$ef41, na.rm = TRUE)    #subsample with workers without a union contract

#function to simultaneously generate a boxplot and save it in the seperate pdf-file
buildboxplot =  function (v, w , boxname, z){
  if (missing(v))
    stop("No data passed to the function. Variable v has to be defined.")
  if (missing(w))
    stop("No data passed to the function. Variable w has to be defined.")
  if (missing(z))
    stop("No data passed to the function. Variable z has to be defined.
         z is a vector which should contain labels for the characteristics of the variable w.
         The number of the characteristics of w must equal the number of elements in z.")
  if (missing(boxname))
    stop('No data passed to the function.boxname has to be defined such as "graph.pdf".') 
  if (is.numeric(v)!= TRUE)
    stop("Numeric data needed. k has to be a numeric variable.")

  pdf(boxname, width = 11, height = 7)
  boxplot(v~w, range=2.5, width=NULL, notch=FALSE,varwidth=FALSE, names = z,
          boxwex=0.8, outline=FALSE, staplewex=0.5, horizontal=FALSE, border="black", 
          col="#94d639", add=FALSE, at=NULL)
  abline(h = median(v, na.rm = TRUE), col="red", lwd = 1.5)
  dev.off()
}

#define a vector with label names for gender and education
genderLAB = c("male", "female")
educLAB = c("Educ A", "Educ B", "Educ C", "Educ D", "Educ E", "Educ F", "Educ G")

#boxplot ln(wage)~gender of all employees
buildboxplot(dat$lnWage, dat$ef10, "boxplot_lnwage_gen.pdf", genderLAB)

#boxplot ln(wage)~education of all employees 
buildboxplot(dat$lnWage, dat$ef16u2, "boxplot_lnwage_educ.pdf", educLAB)

#boxplot ln(wage)~education of employees which are covered by an union contract 
buildboxplot(datFCSC$lnWage, datFCSC$ef16u2, "boxploteducFCSC.pdf", educLAB)

#boxplot ln(wage)~education of employees which are not covered by an union contract
buildboxplot(datNoFCSC$lnWage, datNoFCSC$ef16u2, "boxploteduc-NoFCSC.pdf", educLAB)

#calculate median of the variable ln(wage)
median(dat$lnWage, na.rm = TRUE)
median(datFCSC$lnWage, na.rm = TRUE)
median(datNoFCSC$lnWage, na.rm = TRUE)
