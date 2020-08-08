## package installation script
# specific repository:
install.packages("geosphere", repos='https://cran.rstudio.com/', dependencies = TRUE)
# specific version:

## Reading in files
# csv format:
setwd("C:/Users/GZ668HL/Downloads/UT/Data for forecast/Structured Data/Phase II/Enterprise data")
ForecastAASW<-read.csv('ForecastAASW.csv',stringsAsFactors = FALSE)
# DBF format:
setwd("C:/Users/GZ668HL/Downloads/UT/Data for forecast/Structured Data/Demand/Train")
library(foreign)
Demand2016<-read.dbf('DMD2016.DBF', as.is = TRUE)
# Excel format:
setwd("C:/Users/GZ668HL/Downloads/UT/Data for forecast/Structured Data/Reference files")
library(readxl)
ITC0318<-read_excel("ITC0318.xlsx",col_names = TRUE)

## Aggregating a dataframe:
library(dplyr)
PLTCustDMD<-custDemand%>%
  group_by(PLANT,Month)%>%
  summarise(CustDMDPos = sum(CustDMDPos), CustDMDNeg = sum(CustDMDNeg))

## Completing a dataframe for missing values:
library(tidyr)
TestDemandFCASTMonthlyMatHF<-TestDemandFCASTMonthlyMatHF %>% complete(NewMaterial, Month)

## Ordering a data frame in descending order:
Custordval<-Custordval[order(-Custordval$ORDVALUE),]

# Zero imputed to NA values:
TrainDemandMonthlyggMAT16Sub[is.na(TrainDemandMonthlyggMAT16Sub)]<-0

# Below is wrong syntax
library(xts)
ARIMAXINPxts<-xts(ARIMAXINP1$Demand, order.by = ARIMAXINP1$Period)

# Removing all but one object from the environment
rm(list=setdiff(ls(), "MasterDemandData"))

# Doing a statistical operation on a group with in dataset:
install.packages("plyr", repos='https://cran.rstudio.com/')
library(plyr)
func <- function(DemandTrainTestINPASFMC)
{
  return(data.frame(COR = cor(DemandTrainTestINPASFMC$ProjDemand, DemandTrainTestINPASFMC$X49)))
}

ddply(DemandTrainTestINPASFMC, .(PLANT), func)

# Reading an excel workbook with all its sheets:
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

OVHCompPlan <- read_excel_allsheets("Hansei Data 2017-Archive.xlsx")

# Transposing a dataframe with null value imputations:
ENG_SPEED_MXaggTran<-reshape2::dcast(ENG_SPEED_MXagg, MCHN_SRL_NUM+MONTH~ENG_SPEED_MX_Bin,value.var ='xx')
ENG_SPEED_MXaggTran[is.na(ENG_SPEED_MXaggTran)]<-0

# Binning below for SMR and working hours
BKJHD7857SMRData_2016_18FE<-BKJHD7857SMRData_2016_18
BKJHD7857SMRData_2016_18FE[12:37]<-sapply(BKJHD7857SMRData_2016_18FE[12:37],Cutin3)

Cutin3<- function(x) {
  cut(x, 3, include.lowest=TRUE, labels=c(0,1,2))
}

# Renaming colnames to AGE
for (j in c(1:25))
{
  colnames(AGE)[j] = paste0('AGE',colnames(AGE)[j])
}

# To increase memory allocation
memory.limit(size=16000)

# Check current memory allocation
memory.limit()