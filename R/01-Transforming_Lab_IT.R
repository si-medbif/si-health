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

lab_data <- read_csv("GitHub/si-health/data/2019_IT_SIH_9.csv")
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

## --------------------------------------------------------------------------------------------------------------------------------##
##                                           data transformation >> long into wide format                                          ##
## --------------------------------------------------------------------------------------------------------------------------------##


data.split[[1]]   ## Visit date








## Method 1
##-----------------------------------------------------------------##
##   for data transformation               >>  visit_id == 1       ##
##-----------------------------------------------------------------##


lab_data$visit_id         <- paste(lab_data$`OH_PID`,lab_data$"TI_NAME",sep="|")
DT                        <- data.table(lab_data)
Data_IT                   <- DT[, visit_id2 := seq_len(.N), by=visit_id]

lab_data_long             <- DT[,c("OH_PID", "OH_TRX_DT", "visit_id2","TI_NAME","OD_TR_VAL")]
label_value_NA            <- lab_data_long$`OD_TR_VAL`[lab_data_long$`OD_TR_VAL` %in% c("","!","*")] <- NA
Re.data_it                <- reshape(lab_data_long, v.names = "OD_TR_VAL", direction = "wide",idvar = "OH_PID", timevar = "TI_NAME")
names(Re.data_it)         <- gsub("OD_TR_VAL."," ", names(Re.data_it))
lab_data_wide             <- Re.data_it

fre_ID                    <- table(lab_data_long$'OH_PID')   # check obs. of OH_PID
################                 END                        #########################################################################


## Method 2
##-----------------------------------------------------------------##
##  for repeated data transformation       >>  visit_id == maximum ##
##-----------------------------------------------------------------##

lab_data1                 <- as.data.table(lab_data)
lab_data1$'visit_id'      <- paste(lab_data1$'OH_PID', lab_data1$'TI_NAME', sep = '||')
lab_data2                 <- lab_data1[, visit_id2 := seq_len(.N), by=visit_id]
lab_data3                 <- as.data.table(lab_data2)
lab_long                  <- lab_data3[, c("OH_PID", "OH_TRX_DT", "visit_id2","TI_NAME","OD_TR_VAL")]
lab_long$OD_TR_VAL        <- na_if(lab_long$OD_TR_VAL, "!")
lAB_wide                  <- lab_long %>% spread(TI_NAME, OD_TR_VAL, fill = NA, convert = FALSE)
lAB_wideUD                <- lAB_wide %>% group_by(OH_PID) %>% top_n(1, visit_id2)

fre_ID                    <- table(lab_long$'OH_PID')   # check obs. of OH_PID

################                 END                        #########################################################################


## Method 3
##-----------------------------------------------------------------##
##  for repeated data transformation (loop) >> visit_id == maximum ##
##-----------------------------------------------------------------##

LAB_data1                 <- as.data.table(lab_data)
LAB_data1$'visit_id'      <- paste(LAB_data1$'OH_PID', LAB_data1$'TI_NAME', sep = '||')
LAB_data2                 <- LAB_data1[, visit.no.times := seq_len(.N), by=visit_id]
LAB_data_long             <- LAB_data2[, c("OH_PID", "OH_TRX_DT", "visit.no.times","TI_NAME","OD_TR_VAL")]
LAB_data_wide             <- LAB_data_long %>% spread(TI_NAME, OD_TR_VAL, fill = NA, convert = FALSE)

#--------------------------------------------------

table.data                <- table(LAB_data_wide$'visit.no.times')
name                      <- names(table(LAB_data_wide$'OH_PID'))
fre                       <- table(LAB_data_wide$'OH_PID')
max.fre                   <- length(fre)

#--------------------------------------------------

LAB_data_wide1            <- LAB_data_wide[, visit.no.times:= seq_len(.N), by=OH_PID]   # order by OH_PID
data.split                <- split(LAB_data_wide1, LAB_data_wide1$`OH_PID`)
LAB_data_wide2            <- as.data.frame(LAB_data_wide[FALSE, ])

for (i in 1:max.fre) {

  LAB_data_wide2[i,] <- c(data.split[[i]] %>% top_n(1, visit.no.times))

}





################                 END                        #########################################################################
