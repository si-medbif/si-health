getwd()
setwd("C:/Users/Admin/Documents")

##   Clear Object (Data)
rm(list = objects())

####################################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------#
#                                          title: "Transforming Lab Data from SI-IT"                                               #
# ---------------------------------------------------------------------------------------------------------------------------------#
####################################################################################################################################

## -------------------------------------------------- ##
##                  Import lab data                   ##
## -------------------------------------------------- ##

lab_data <- read_csv("D:/# Projects/Si.Health/Data Si.Health/Data manament/MSih_0001-4000/3) IT folders/Data af_CSV/2014_IT SIH_4.csv")
attach(lab_data)


## -------------------------------------------------- ##
##        Loading Important packages/ libraries       ##
## -------------------------------------------------- ##
## install.packages("readr")
## help.search('data.table')
library(data.table)
library(readr)
library(readxl)
library(Hmisc)
library(dplyr)
library(tidyr)


describe(lab_data)

### Data dictionary
#   1. `OH_TNO`
#   2. `OH_TRX_DT`        -- ** Need to check the format of the test dates**
#   3. `OH_PID`           -- HN ID
#   4. `OH_BOD`           -- date of birth in -YYYY-MM-DD
#   5. `OH_SEX`           -- **not sure 1/2 coding for which sex**
#   6. `OH_CLINIC_CODE`   -- for the clinic requesting the tests
#   7. `OD_TESTCODE`      -- Test ID
#   8. `TI_NAME`          -- Test ID Name
#   9. `OD_TR_VAL`        -- value of the tests


lab_data$visit_id   <- paste(lab_data$`OH_PID`,lab_data$"TI_NAME",sep="|")
DT                  <- data.table(lab_data)
Data_IT             <- DT[, visit_id2 := seq_len(.N), by=visit_id]
view.data           <- View(Data_IT[c(7332,16592),])

## --------------------------------------------------------------------------------------------------------------------------------##
##                                           data transformation >> long into wide format                                          ##
## --------------------------------------------------------------------------------------------------------------------------------##



##-----------------------------------------------------------------##
##   for data transformation               >>  visit_id == 1       ##
##-----------------------------------------------------------------##

lab_data_long       <- DT[,c("OH_PID","OH_TRX_DT","OH_BOD","OH_CLINIC_CODE","OD_TESTCODE","visit_id2","TI_NAME","OD_TR_VAL")]
label_value_NA      <- lab_data_long$`OD_TR_VAL`[lab_data_long$`OD_TR_VAL` %in% c("","!","*")] <- NA
Re.data_it          <- reshape(lab_data_long, v.names = "OD_TR_VAL", direction = "wide",idvar = "OH_PID", timevar = "TI_NAME")
names(Re.data_it)   <- gsub("OD_TR_VAL."," ", names(Re.data_it))
lab_data_wide       <- Re.data_it




##-----------------------------------------------------------------##
##  for repeated data transformation   >>  visit_id == maximum     ##
##-----------------------------------------------------------------##

lab_data1                 <- as.data.table(lab_data)
lab_data1$'visit_id'      <- paste(lab_data1$'OH_PID', lab_data1$'TI_NAME', sep = '||')
lab_data2                 <- lab_data1[, visit_id2 := seq_len(.N), by=visit_id]
lab_data3                 <- as.data.table(lab_data2)
lab_data_long             <- lab_data3[, c("OH_PID", "OH_TRX_DT", "visit_id2","TI_NAME","OD_TR_VAL")]
lab_data_long$OD_TR_VAL   <- na_if(lab_data_long$OD_TR_VAL, "!")
LAB_data_wide             <- lab_data_long %>% spread(TI_NAME, OD_TR_VAL, fill = NA, convert = FALSE)
LAB_data_wideUD           <- LAB_data_wide %>% group_by(OH_PID) %>% top_n(1, visit_id2)



####################################################################################################################################
#                                                       Export Data                                                                #
####################################################################################################################################

write.csv(lab_data_wide,na =" ", "D:/# Projects/Si.Health/Data Si.Health/Data manament/MSih_0001-4000/3) IT folders/Reshape data/Re2017_IT.csv")










