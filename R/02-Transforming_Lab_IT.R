##   Clear Object (Data)
rm(list = objects())

####################################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------#
#                                          title: "Transforming Lab Data from SI-IT"                                               #
# ---------------------------------------------------------------------------------------------------------------------------------#
####################################################################################################################################


## -------------------------------------------------- ##
##        Loading Important packages/ libraries       ##
## -------------------------------------------------- ##
# install.packages("readr")
# install.packages("readxl")
# install.packages("Hmisc")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("tidyr")

library(readr)
library(readxl)
library(Hmisc)          ## General descriptive>>fn i.e. mean median describe
library(dplyr)
library(data.table)     ## data.table>>fn  as.data.frame>>fn
library(tidyr)          ## spread>>fn
library(xlsx)


LAB_IT_Transform <- function(data_LAB) {

  lab_data0 <- data_LAB

  ## -------------------------------------------------- ##
  ## describe(lab_data0)
  ## -------------------------------------------------- ##


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

  lab_data              <- as.data.table(lab_data0)
  lab_data$'visit_id'   <- paste(lab_data$'OH_PID', lab_data$'TI_NAME', sep = '||')
  # names(lab_data)
  lab_data1             <- lab_data[, visit.no.times := seq_len(.N), by = visit_id]
  # names(lab_data1)
  lab_data2             <- as.data.table(lab_data1)  # View(lab_data2[c(151:160), ])
  lab_long              <- lab_data2[, c("OH_PID", "OH_TRX_DT", "visit.no.times","TI_NAME","OD_TR_VAL")]
  lab_long$OD_TR_VAL    <- na_if(lab_long$OD_TR_VAL, "!")
  lab.name              <- names(table(lab_long$TI_NAME))


  ## -------------------------------------------------- ##

  lab_wide              <- lab_long %>% spread(TI_NAME, OD_TR_VAL, fill = NA, convert = FALSE)
  no_data_wide          <- as.data.frame(lab_wide)
  data.split            <- split(no_data_wide, no_data_wide$OH_PID)

  ## A <- data.split[[1]]
  ## rm(A)

  no.var                <- dim(data.split[[1]]) [2]
  no.ID                 <- length(unique(no_data_wide$OH_PID))

  conclude              <- as.data.frame(no_data_wide[FALSE, ]) [, -3]


  ## ----------------------------------------------------------- ##
  ##      Important function for summarizing individual data     ##
  ## ----------------------------------------------------------- ##

  get.updated.value <- function(Array.chk) {

    Array.chk.nonmiss   <- na.omit(Array.chk)
    Array.length        <- length(Array.chk.nonmiss)

    result <- NULL

    if (Array.length < 1) {
      result <- Array.chk [1]
    }
    else {
      result <- Array.chk.nonmiss[Array.length]
    }

    result
  }

  get.mode.value <- function(Array.chk) {

    get.mode <- function(vector) {
      uniqv <- unique(vector)
      uniqv[which.max(tabulate(match(vector, uniqv)))]
    }

    Array.chk.nonmiss   <- na.omit(Array.chk)
    Array.length        <- length(Array.chk.nonmiss)

    result <- NULL

    if (Array.length < 1) {
      result <- Array.chk [1]
    }
    else {
      result <- Array.chk.nonmiss[Array.length]
    }

    get.mode(result)
  }

  summarize.Individual <- function(data.split, i) {

    ######################################################################################################
    ## ----------------------------------  Computation per case  -------------------------------------- ##
    ######################################################################################################
    # List all variable name to easily pick up it which I want
    # names(conclude)

    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##
    # [1] "OH_PID"                    "% Basophils"               "% Eosinophils"             "% Lymphocytes"             "% Monocytes"
    # [6] "% Neutrophils"             "%Abnormal cells"           "%Atypical lymphocyte"      "%Band form"                "%Blasts"
    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##

    OH_PID          <- get.mode.value(data.split[[i]] [, 1])     ## HN
    OH_TRX_DT       <- get.updated.value(data.split[[i]] [, 2])
    Basophils       <- get.updated.value(data.split[[i]] [, 4])
    Eosinophils     <- get.updated.value(data.split[[i]] [, 5])
    Lymphocytes     <- get.updated.value(data.split[[i]] [, 6])
    Monocytes       <- get.updated.value(data.split[[i]] [, 7])
    Neutrophils     <- get.updated.value(data.split[[i]] [, 8])
    Abnormal_cells  <- get.updated.value(data.split[[i]] [, 9])
    Atypical_lym    <- get.updated.value(data.split[[i]] [, 10])
    Band_form       <- get.updated.value(data.split[[i]] [, 11])
    Blasts          <- get.updated.value(data.split[[i]] [, 12])

    list1 <- cbind(OH_PID, OH_TRX_DT, Basophils, Eosinophils, Lymphocytes, Monocytes, Neutrophils, Abnormal_cells, Atypical_lym, Band_form, Blasts)

    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##
    # [11] "%Metamyelocyte"            "%Myelocyte"                "%Promyelocyte"             "**eGFR(CKD-EPI equation)"  "**LDL-Calculated"
    # [16] "Absolute"                  "Absolute basophil"         "Absolute eosinophil"       "Absolute lymphocyte"       "Absolute monocyte"
    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##

    Metamyelocyte   <- get.updated.value(data.split[[i]] [, 13])
    Myelocyte       <- get.updated.value(data.split[[i]] [, 14])
    Promyelocyte    <- get.updated.value(data.split[[i]] [, 15])
    eGFR            <- get.updated.value(data.split[[i]] [, 16])
    LDL             <- get.updated.value(data.split[[i]] [, 17])
    Absolute        <- get.updated.value(data.split[[i]] [, 18])
    Absolute_baso   <- get.updated.value(data.split[[i]] [, 19])
    Absolute_eosino <- get.updated.value(data.split[[i]] [, 20])
    Absolute_lym    <- get.updated.value(data.split[[i]] [, 21])
    Absolute_mono   <- get.updated.value(data.split[[i]] [, 22])

    list2 <- cbind(Metamyelocyte, Myelocyte, Promyelocyte, eGFR, LDL, Absolute, Absolute_baso, Absolute_eosino, Absolute_lym, Absolute_mono)

    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##
    # [21] "Absolute neutrophils"      "Absolute reticulocyte"     "Anisocytosis"              "Basophillic stippling"     "BUN"
    # [26] "CBC"                       "Cholesterol"               "Creatinine"                "Creatinine (Urine)"        "Elliptocyte"
    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##

    Absolute_neutro   <- get.updated.value(data.split[[i]] [, 23])
    Absolute_retic    <- get.updated.value(data.split[[i]] [, 24])
    Anisocytosis      <- get.updated.value(data.split[[i]] [, 25])
    Basophillic_stip  <- get.updated.value(data.split[[i]] [, 26])
    BUN               <- get.updated.value(data.split[[i]] [, 27])
    CBC               <- get.updated.value(data.split[[i]] [, 28])
    Cholesterol       <- get.updated.value(data.split[[i]] [, 29])
    Creatinine        <- get.updated.value(data.split[[i]] [, 30])
    Creatinine_urine  <- get.updated.value(data.split[[i]] [, 31])
    Elliptocyte       <- get.updated.value(data.split[[i]] [, 32])

    list3 <- cbind(Absolute_neutro, Absolute_retic, Anisocytosis, Basophillic_stip, BUN, CBC, Cholesterol, Creatinine, Creatinine_urine, Elliptocyte)

    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##
    # [31] "Glucose (NaF)"             "Hb A1C (immunoassay)"      "HDL-CHOL"                  "Hematocrit"                "Hemoglobin"
    # [36] "Howell jolly body"         "Hypochromic"               "Macrocytic"                "MAU/Urine Cr ratio"        "MCH"
    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##

    Glucose         <- get.updated.value(data.split[[i]] [, 33])
    HbA1C           <- get.updated.value(data.split[[i]] [, 34])
    HDL             <- get.updated.value(data.split[[i]] [, 35])
    Hematocrit      <- get.updated.value(data.split[[i]] [, 36])
    Hemoglobin      <- get.updated.value(data.split[[i]] [, 37])
    Howell_jolly    <- get.updated.value(data.split[[i]] [, 38])
    Hypochromic     <- get.updated.value(data.split[[i]] [, 39])
    Macrocytic      <- get.updated.value(data.split[[i]] [, 40])
    MAU.urin_Ratio  <- get.updated.value(data.split[[i]] [, 41])
    MCH             <- get.updated.value(data.split[[i]] [, 42])


    list4 <- cbind(Glucose, HbA1C, HDL, Hematocrit, Hemoglobin, Howell_jolly, Hypochromic, Macrocytic, MAU.urin_Ratio, MCH)

    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##
    # [41] "MCHC"                      "MCV"                       "Microalbumin"              "Microcytic"                "NRC/ 100 WBC"
    # [46] "Platelet count"            "PLT comment"               "PLT Reflex"                "Poikilocytosis"            "Polychromasia"
    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##

    MCHC            <- get.updated.value(data.split[[i]] [, 43])
    MCV             <- get.updated.value(data.split[[i]] [, 44])
    Microalbumin    <- get.updated.value(data.split[[i]] [, 45])
    Microcytic      <- get.updated.value(data.split[[i]] [, 46])
    NRC             <- get.updated.value(data.split[[i]] [, 47])
    Platelet_count  <- get.updated.value(data.split[[i]] [, 48])
    PLT_comment     <- get.updated.value(data.split[[i]] [, 49])
    PLT_Reflex      <- get.updated.value(data.split[[i]] [, 50])
    Poikilocytosis  <- get.updated.value(data.split[[i]] [, 51])
    Polychromasia   <- get.updated.value(data.split[[i]] [, 52])

    list5 <- cbind(MCHC, MCV, Microalbumin, Microcytic, NRC, Platelet_count, PLT_comment, PLT_Reflex, Poikilocytosis, Polychromasia)

    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##
    # [51] "RBC comment"               "Rbc count"                 "Rbc morphology"            "Red cell distribution RDW" "Schistocyte"
    # [56] "Smear from DM96"           "Smear from LH780"          "Spherocyte"                "Target cell"               "Tear drop cell"
    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##

    Rbc_comment     <- get.updated.value(data.split[[i]] [, 53])
    Rbc_count       <- get.updated.value(data.split[[i]] [, 54])
    Rbc_morphology  <- get.updated.value(data.split[[i]] [, 55])
    Red_cell        <- get.updated.value(data.split[[i]] [, 56])
    Schistocyte     <- get.updated.value(data.split[[i]] [, 57])
    Smear_DM96      <- get.updated.value(data.split[[i]] [, 58])
    Smear_LH780     <- get.updated.value(data.split[[i]] [, 59])
    Spherocyte      <- get.updated.value(data.split[[i]] [, 60])
    Target_cell     <- get.updated.value(data.split[[i]] [, 61])
    Target_drop     <- get.updated.value(data.split[[i]] [, 62])

    list6 <- cbind(Rbc_comment, Rbc_count, Rbc_morphology, Red_cell, Schistocyte, Smear_DM96, Smear_LH780, Spherocyte, Target_cell, Target_drop)

    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##
    # [61] "Triglyceride"              "Urine Micro Albumin,MAU"   "WBC-C"                     "WBC (Coulter)"             "WBC comment"
    # [66] "Wbc count"                 "Wbc differential count"
    ## ----------------------------------------------------------------------------------------------------------------------------------------- ##

    TG              <- get.updated.value(data.split[[i]] [, 63])
    Urine_Micro     <- get.updated.value(data.split[[i]] [, 64])
    Wbc_C           <- get.updated.value(data.split[[i]] [, 65])
    Wbc_coulter     <- get.updated.value(data.split[[i]] [, 66])
    Wbc_comment     <- get.updated.value(data.split[[i]] [, 67])
    Wbc_count       <- get.updated.value(data.split[[i]] [, 68])
    Wbc_dif         <- get.updated.value(data.split[[i]] [, 69])


    list7 <- cbind(TG, Urine_Micro, Wbc_C, Wbc_coulter, Wbc_comment, Wbc_count, Wbc_dif)

    ## Resulth

    result <- c(list1, list2, list3, list4, list5, list6, list7)

    result
  }

  ## summarize.Individual(data.split, 1)

  for (i in 1:no.ID) {

    ######################################################################################################
    ## -----------------------------------  Summarize Individual Data  -------------------------------- ##
    ######################################################################################################

    conclude[i,] <- summarize.Individual(data.split, i)
  }

  list(lab_wide, conclude)
}

## -------------------------------------------------- ##
##                  Import lab data                   ##
## -------------------------------------------------- ##

getwd()
setwd("D:/# Projects/Si.Health/Data Si.Health/Data management/MSih_0001-4000/3) IT folders/Data CSV")


data_LAB   <- read_csv("2017_IT SIH_1.csv")


# View(data_LAB)


data.results      <- LAB_IT_Transform(data_LAB)

data_wide_rp      <- data.results[[1]]  ## Results >>> Data repeat (long)
data_wide_nonrp   <- data.results[[2]]  ## Resulth >>> Data wide

View(data_wide_rp)
View(data_wide_nonrp)












