## tableone package itself
# install.packages('tableone')
library(tableone)
## survival package for Mayo Clinic's PBC data
library(survival)

library(dplyr)
library(pipeR)
library(broom)

```
Data "pbc"

Mayo Clinic Primary Biliary Cholangitis Data

Primary sclerosing cholangitis is an autoimmune disease 
leading to destruction of the small bile ducts in the liver. 
Progression is slow but inexhortable, 
eventually leading to cirrhosis and liver decompensation. 
The condition has been recognised since at least 1851 
and was named "primary biliary cirrhosis" in 1949. 
Because cirrhosis is a feature only of advanced disease, 
a change of its name to "primary biliary cholangitis" was
proposed by patient advocacy groups in 2014.
This data is from the Mayo Clinic trial in PBC conducted between 1974 and 1984. 
A total of 424 PBC patients, referred to Mayo Clinic during that ten-year interval, 
met eligibility criteria for the randomized placebo controlled trial
of the drug D-penicillamine. 
The first 312 cases in the data set participated in the randomized trial 
and contain largely complete data. 
The additional 112 cases did not participate in the clinical trial, 
but consented to have basic measurements recorded and to be followed for survival. 
Six of those cases were lost to follow-up shortly after diagnosis, 
so the data here are on an additional 106 cases as well as the 312 randomized participants.

# number of days between registration and the earlier of death, transplantion, or study analysis in July, 1986

# 1/2/NA for D-penicillamine, placebo, not randomised
```

getwd()

pbc <- read.csv(file = "./import/pbc.csv")
pbc
# data(pbc)
# ?pbc


# Single group summary ###############
CreateTableOne(data = pbc)


## Get variables names
# dput(names(pbc))
names(pbc)
colnames(pbc)


## Vector of variables to summarize
myVars <- c("time", "status", "trt", "age", "sex", "ascites", "hepato",
            "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos",
            "ast", "trig", "platelet", "protime", "stage")
## Vector of categorical variables that need transformation
catVars <- c("sex", "status", "trt", "ascites", "hepato",
             "spiders", "edema", "stage")
## Create a TableOne object
tab2 <- CreateTableOne(vars = myVars, data = pbc, factorVars = catVars)


tab2



# Showing all levels for categorical variables ###############
# print(tab2, showAllLevels = TRUE) # , formatOptions = list(big.mark = ",")



# summary(tab2)

# Summarizing nonnormal variables ###############
biomarkers <- c("bili","chol","copper","alk.phos","ast","trig","protime")
print(tab2, nonnormal = biomarkers, formatOptions = list(big.mark = ",") )



# Multiple group summary ###############
tab3 <- CreateTableOne(vars = myVars, strata = "trt" , data = pbc, factorVars = catVars)
print(tab3, nonnormal = biomarkers) # , formatOptions = list(big.mark = ",")

# kruskal.test() is used for the nonnormal continous variables and 
# fisher.test() is used for categorical variables specified in the exact argument. 
# kruskal.test() is equivalent to wilcox.test() in the two-group case

print(tab3, nonnormal = biomarkers, exact = "stage") # , smd = TRUE


# Exporting ###############
# Quick and dirty way
print(tab3, nonnormal = biomarkers, exact = "stage", quote = TRUE, noSpaces = TRUE)


# Real export way
tab3Mat <- print(tab3, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab3Mat, file = "./export/myTable.csv")

# write.csv(pbc, file = "./export/pbc.csv")



# status:	status at endpoint, 0/1/2 for censored, transplant, dead
# pbc$status

pbc2 <- pbc %>% 
  dplyr::mutate( death = if_else( status == 2, 1, 0) ) 

head(pbc2)

pbc2 %>% 
  dplyr::select( status, death ) %>%
  head()

# Obtain odds ratio of ascites for death
glm(death ~ ascites, data = pbc2 )

glm_model <- glm(death ~ ascites, family = binomial( "logit" ), data = pbc2 )

broom::tidy( glm_model, conf.int=F, exponentiate=F )

broom::tidy( glm_model, conf.int=F, exponentiate=F ) %>>% 
  dplyr::mutate( estimate = exp( estimate ) ) 

glm_model_OR <- bind_cols( 
  broom::tidy( glm_model, conf.int=F, exponentiate=F ) %>>% 
    dplyr::mutate( estimate = exp( estimate ) ) %>>% 
    dplyr::select( term, estimate, p.value ), 
  exp( confint( glm_model, func = stats::confint.default ) ) 
)

glm_model_OR



glm_model2 <- glm(death ~ ascites + sex, family = binomial( "logit" ), data = pbc2 )
broom::tidy( glm_model2, conf.int=T, exponentiate=T )


glm_model_OR2 <- bind_cols( 
  broom::tidy( glm_model2, conf.int=F, exponentiate=F ) %>>% 
    dplyr::mutate( estimate = exp( estimate ) ) %>>% 
    dplyr::select( term, estimate, p.value ), 
  exp( confint( glm_model2, func = stats::confint.default ) ) 
)

glm_model_OR2

