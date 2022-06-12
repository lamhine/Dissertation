library(tidyverse)
library(survey)
library(mi)
library(mitools)

data(api)

apisub <- apiclus1 %>% select(-c("name", "sname", "dname", "cname", "flag", 
                                 "acs.46", "acs.core"))

mdf <- missing_data.frame(apisub)
mdf <- change(mdf, "cds", what = "type", to = "irrelevant")
mdf <- change(mdf, "stype", what = "type", to = "irrelevant")
mdf <- change(mdf, "snum", what = "type", to = "irrelevant")
mdf <- change(mdf, "dnum", what = "type", to = "irrelevant")
mdf <- change(mdf, "cnum", what = "type", to = "irrelevant")
mdf <- change(mdf, "fpc", what = "type", to = "irrelevant")
mdf <- change(mdf, "pw", what = "type", to = "irrelevant")

show(mdf)

imputations <- mi(mdf)

imp_list <- complete(imputations, m = 5)

dsn <- svydesign(id = ~dnum, 
                 weights = ~pw, 
                 data = imputationList(imp_list), 
                 fpc = ~fpc)

dsn_sub <- subset(dsn, both == "No")

anl <- with(dsn_sub, 
            svyglm(api99 ~ enroll + avg.ed + meals + ell, 
                   family = gaussian(),
                   design = dsn
                   )
            )

res <- MIcombine(anl)  
  
