library(tidyverse)
library(lavaan)
library(lavaanPlot)
library(semPlot)
library(MVN)

theme_set(theme_minimal())

data <- read_csv('https://raw.githubusercontent.com/jpedroza1228/prev610_mediationmoderation/1d96144fd9936f4a33ef9c8cf5431de32d5e3572/data/homework2.csv') %>% 
  janitor::clean_names()

names(data)

med_sem_data <- data %>% 
  select(cage1,income1,educ1,ftcknow2,harsh3,incons3,pos3,
  ecbin3t,ecbprb3t,sdqpro3)

names_col <- c('cage1','income1','educ1','ftcknow2',
               'harsh3','incons3','pos3',
               'ecbin3t','ecbprb3t','sdqpro3')

# create univariate histograms
mvn(data = med_sem_data, mvnTest = "mardia", univariateTest = "AD")


hist_fun <- function(data, x){
  ggplot({{data}}, aes({{x}})) +
    geom_histogram(bins = 15, color = 'white',
                   fill = 'dodgerblue')
}

hist_fun(med_sem_data, cage1)

map2(med_sem_data, names_col, ~hist_fun(med_sem_data, .x) +
      labs(title = glue::glue('Variable: {.y}'))) 


##instal.packages("mice")
library(mice)
library(miceadds)


par(mfrow=c(1,1)) ##make the plot window one plot per page


md.pattern(med_sem_dat)


# You can use this method for addressing missingness
TestMCARNormality(med_sem_dat)



# install.packages("BaylorEdPsych")
# library(BaylorEdPsych)
# This won't work with R 4.0
# install.packages('mvnmle')
# library(mvnmle) This won't work with R 4.0

# mcar.Hw2 <- LittleMCAR(medSEM.dat) 
# mcar.Hw2[c("chi.square","df","p.value")] 

# COME BACK TO THIS LATER - JP


######################################################################################
#############
############# Parallel Mediators (HARSH3, INCONS3, POS3)
#############
############# Serial Mediatiors (ITT-> FTCKnow2-> Parenting Vars-> Child Vars)
#############
######################################################################################


HWmodel1 <- " ChildProb =~ ECBIN3T + ECBPRB3T + SDQPRO3 
          
            ChildProb ~ cPr*ITT + b2*HARSH3 + b3*INCONS3  + b4*POS3 + Cage1 + EDUC1
          
           
           FTCKNOW2 ~ a1*ITT
           
            INCONS3 ~ a2*FTCKNOW2
             HARSH3 ~ a2*FTCKNOW2
               POS3 ~ a4*FTCKNOW2
            

            indirect1 : = a1*a2*b2
            indirect2 : = a1*a3*b3
            indirect3 : = a1*a4*b4
            
            direct : = cPr
            total : = direct + indirect1 + indirect2 + indirect3"


HWfit1<-sem(model=HWmodel1, data=Hw2.dat, estimator = "mlr", missing = "fiml")                   


summary(HWfit1,standardized=TRUE)                   


lavaanPlot(model=HWfit1)

lavaanPlot(model=HWfit1,stand=TRUE, node_options = list(shape="box",fontname="Helvetica",fontsize="12"),
           edge_options = list(color="black"),coefs=TRUE, covs=TRUE,stars=c("regress"))




####################################################################
#############
############# Bootstrap the standard errors from the last model
#############
####################################################################



HWfit2<-sem(model=HWmodel1, data=Hw2.dat, se="bootstrap", bootstrap = 300, estimator = "ml", missing = "fiml")                       


summary(HWfit2,standardized=TRUE)           























