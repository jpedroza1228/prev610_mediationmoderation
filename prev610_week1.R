# install.packages('tidyverse')
# install.packages('psych')
# install.packages('inspectdf')

library(tidyverse)
library(psych)
library(inspectdf)

getwd()
theme_set(theme_minimal())

week1 <- read_csv('https://raw.githubusercontent.com/jpedroza1228/prev610_mediationmoderation/main/data/ftc_week1.csv') %>% 
  janitor::clean_names()

names(week1)

## examine distributional properties of the continuous or scale variables in the simple mediation
week1 %>% 
  dplyr::select(ftcknow2, pos3) %>% 
  psych::describe(na.rm = TRUE)

week1 %>% 
  ggplot(aes(harsh3, pos3)) +
  geom_point(color = 'dodgerblue')

## plot a histogram of the continuous variables
week1 %>% 
  ggplot(aes(ftcknow2)) +
  geom_histogram(bins = 15,
                 color = 'white',
                 fill = 'dodgerblue')

week1 %>% 
  ggplot(aes(pos3)) +
  geom_histogram(bins = 15,
                 color = 'white',
                 fill = 'dodgerblue')


## and you will see plot can also simply be plot (x,y) or plot (y~x)
week1 %>% 
  ggplot(aes(ftcknow2, pos3)) +
  geom_point(alpha = .3) +
  geom_smooth(method = 'lm', se = TRUE,
              color = 'dodgerblue', size = 1)


###### Step 1 Estimate "c" path Direct Effect of ITT to Positive Parenting T3

cmod <- lm(pos3 ~ itt, data = week1)

res_cmod <- broom::augment(cmod)

ggplot(res_cmod, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

####### Step 2 Estimate "a" path Effect of ITT to Mediator Father Knowledge of Intervention T2

amod <- lm(ftcknow2 ~ itt, data = week1) 

##### Step 3 Estimate "c'" or "C prime" path (CPr), what happens to direct effect when mediator is in model?

cprmod <- lm(pos3 ~ itt + ftcknow2, data = week1)

summary(cmod)
summary(amod)
summary(cprmod)


# install.packages('stargazer')

library(stargazer)

#Summary Table
stargazer(cmod, amod, cprmod, type = "text", title = "Baron and Kenny Method")


##Summary Figure using the "mediate" function in the "psych" package
### the "psych::mediate" statement identifies the mediate function from the psych package specifically
### because there are other packages unfortunately that have a function named mediate
med_mod <- psych::mediate(pos3 ~ itt + (ftcknow2), data = week1, n.iter = 1000, std=TRUE, plot=TRUE)
summary(med_mod)

