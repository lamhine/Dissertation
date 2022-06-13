# This test script uses the built-in api data from the survey package to create a reprex for multiple imputation with 
# complex survey design effects. This workflow relies on the survey, mi, and mitools packages. 

# load packages
library(tidyverse)
library(survey)
library(mi)
library(mitools)

# define function to tidy up MIresults
# thank you to https://github.com/vincentarelbundock for providing this code
# https://github.com/vincentarelbundock/modelsummary/issues/499#issuecomment-1153317898
tidy.MIresult <- function(x, ...) {
  nil <- utils::capture.output(out <- summary(x, ...))
  out <- out %>%
    tibble::rownames_to_column('term') %>%
    stats::setNames(c('term', 'estimate', 'std.error', 'conf.low', 'conf.high', 'missing.pct'))
  out
  }

# load data on school performance included in survey package
# documentation available here: https://r-survey.r-forge.r-project.org/survey/html/api.html
data(api)

# remove problematic variables that are unnecessary for example
apisub <- apiclus1 %>% select(-c("name", "sname", "dname", "cname", "flag", 
                                 "acs.46", "acs.core"))

# create and update variable types in missing_data.frame
mdf <- missing_data.frame(apisub)
mdf <- change(mdf, "cds", what = "type", to = "irrelevant")
mdf <- change(mdf, "stype", what = "type", to = "irrelevant")
mdf <- change(mdf, "snum", what = "type", to = "irrelevant")
mdf <- change(mdf, "dnum", what = "type", to = "irrelevant")
mdf <- change(mdf, "cnum", what = "type", to = "irrelevant")
mdf <- change(mdf, "fpc", what = "type", to = "irrelevant")
mdf <- change(mdf, "pw", what = "type", to = "irrelevant")

# summarize the missing_data.frame
show(mdf)

# impute missing data
imputations <- mi(mdf)

# create imputation list to pass to svydesign
imp_list <- complete(imputations, m = 5)

# create complex survey design using imputed data
dsn <- svydesign(id = ~dnum, 
                 weights = ~pw, 
                 data = imputationList(imp_list), 
                 fpc = ~fpc)

# subset the survey design to remove schools that did not meet both targets (just as an example of subsetting)
dsn_sub <- subset(dsn, both == "No")

# specify analytic model  
anl <- with(dsn_sub, 
            svyglm(api99 ~ enroll + avg.ed + meals + ell, 
                   family = gaussian(),
                   design = dsn
                   )
            )
# combine results into a single output
res <- MIcombine(anl)  
  
# tidy up results
tidy.MIresult(res)
