## ----setup,echo=FALSE,message=FALSE,warning=FALSE------------------------
library(tidyverse)
library(tibble)
library(dplyr)
library(purrr)
library(magrittr)
library(knitr)
library(haven)
library(ggplot2)
#reading the data
renaldata <- read_sas('./Data/renal.sas7bdat')
#creating the long format
renaldata_long <- renaldata %>% gather(key='time',value='Haematocrit',-id,-age,-male,-cardio,-reject)
#cleaning the time variable
renaldata_long <- renaldata_long %>% mutate(time=gsub('HC','',time))
#to avoid 06 becoming 6, it is recoded as 0.5
renaldata_long <- renaldata_long %>% mutate(time=as.numeric(gsub('06','0.5',time)))
#writing the long data out as a sas dataset
write_sas(renaldata_long,'./Data/renallong.sas7bdat')

