re#Load libraries
library(ggplot2)
library(viridis)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(glmnet)
library(ranger)
#library(sobolMDA)

#Note: before running the following code, make sure to run BackEnd.R 
#in order to load the appropriate functions


#Load our data from the three excel sheets
FibrosisData = openxlsx::read.xlsx("Liver_Database_Deidentified.xlsx")
TwoC1Data=openxlsx::read.xlsx("Biopsy1(Baseline)-2C1-IHC-DATA-(Sum-Batch-GA3_2C1_V3_B1-2C1-20230614).xlsx")
PlasmaData=openxlsx::read.xlsx("DI-Vertex-Alpha1_Longitudinal_DataandPolymer_Oct 2022.xlsx")
#clean the names in all three databases and make each set of data into a data frame
TwoC1Data<-as.data.frame(TwoC1Data)
TwoC1Data<-TwoC1Data %>%
  janitor::clean_names()
FibrosisData<-as.data.frame(FibrosisData)
FibrosisData<-FibrosisData %>%
  janitor::clean_names()
PlasmaData<-as.data.frame(PlasmaData)
PlasmaData<-PlasmaData %>%
  janitor::clean_names()
#Make all three databases have matching patient ID columns
#table(is.na(TwoC1Data$biopsy_1_baseline_2c1_ihc_data_from_nikon_analysis_and_scan))
TwoC1Data<-TwoC1Data %>%
  mutate_at("biopsy_1_baseline_2c1_ihc_data_from_nikon_analysis_and_scan",
            str_replace,"B1-2C1-00","")
TwoC1Data<-TwoC1Data %>%
  mutate_at("biopsy_1_baseline_2c1_ihc_data_from_nikon_analysis_and_scan",
            str_replace,"B1-2C1-0","")
TwoC1Data<-TwoC1Data %>%
  mutate_at("biopsy_1_baseline_2c1_ihc_data_from_nikon_analysis_and_scan",
            str_replace,"B1-2C1-","")
TwoC1Data<-TwoC1Data[-1,]#remove first row as it contains column names
index<-is.na(PlasmaData[,1])#mark the NA in the first column of plasma data, so we can use this column as id numbers
PlasmaData[index,1]<-0#change the NA to a zero to avoid errors when running PatientIDSort function


#table(is.na(TwoC1Data$biopsy_1_baseline_2c1_ihc_data_from_nikon_analysis_and_scan))
#table(is.na(FibrosisData$i_participant_id))
FibrosisData<-FibrosisData %>%
  mutate_at("i_participant_id",str_replace,"63-2013-00","")
FibrosisData<-FibrosisData %>%
  mutate_at("i_participant_id",str_replace,"63-2013-0","")
FibrosisData<-FibrosisData %>%
  mutate_at("i_participant_id",str_replace,"63-2013-","")
#table(is.na(FibrosisData$i_participant_id))
#Build dataframe of independent variables, first build one data frame of
#continuous and one of categorical, then meld the two
DataContinuousB<-as.data.frame(matrix(nrow=109,ncol=87))
#Make a list of all the data that is continuous and from baseline
MyList1<-list(PlasmaData[,8],PlasmaData[,12],PlasmaData[,13],
             PlasmaData[,16],
             PlasmaData[,25],PlasmaData[,41],PlasmaData[,45],
             PlasmaData[,49],PlasmaData[,77],PlasmaData[,81],
             PlasmaData[,85],PlasmaData[,89],PlasmaData[,93],
             PlasmaData[,97],PlasmaData[,101],PlasmaData[,105],
             PlasmaData[,109],PlasmaData[,113],PlasmaData[,117],
             PlasmaData[,121],PlasmaData[,122],PlasmaData[,126],
             PlasmaData[,130],PlasmaData[,134],PlasmaData[,138],
             PlasmaData[,142],PlasmaData[,146],PlasmaData[,150],
             PlasmaData[,154],PlasmaData[,158],PlasmaData[,242],
             PlasmaData[,246],PlasmaData[,250],PlasmaData[,254],
             PlasmaData[,258],PlasmaData[,262],PlasmaData[,266],
             PlasmaData[,270],PlasmaData[,274],PlasmaData[,278],
             PlasmaData[,282],PlasmaData[,286],PlasmaData[,290],
             PlasmaData[,294],PlasmaData[,298],PlasmaData[,302],
             PlasmaData[,306],PlasmaData[,310],PlasmaData[,320],
             PlasmaData[,330])
MyList2<- list(FibrosisData[,133],FibrosisData[,176],
             FibrosisData[,177],FibrosisData[,178],FibrosisData[,179],
             FibrosisData[,180],FibrosisData[,183],FibrosisData[,185],
             FibrosisData[,192],FibrosisData[,194],FibrosisData[,195],
             FibrosisData[,200],FibrosisData[,206],FibrosisData[,208],
             FibrosisData[,209],FibrosisData[,210],FibrosisData[,211],
             FibrosisData[,212],FibrosisData[,213],FibrosisData[,215],
             FibrosisData[,216],FibrosisData[,217],FibrosisData[,218],
             FibrosisData[,219],FibrosisData[,220],FibrosisData[,221],
             FibrosisData[,222],FibrosisData[,223],FibrosisData[,224],
             FibrosisData[,225],FibrosisData[,226],FibrosisData[,243],
             FibrosisData[,244],FibrosisData[,246],FibrosisData[,247])

for (k in 1:length(MyList1)){#for each entry in MyList
  DataContinuousB[,k]<-PatientIDSort(DataContinuousB[,k],
                      MyList1[[k]],PlasmaData[,1])#Make column k with the patient IDs as the row numbers
}
for (k in 1:length(MyList2)){
  my.number<-k+length(MyList1)
  DataContinuousB[,my.number]<-PatientIDSort(DataContinuousB[,my.number],
                                MyList2[[k]],FibrosisData[1:109,1])
}

#Split blood pressure into Systolic (86th column) and Diastolic (87th column)
for (i in 1:109){
  if (is.na(FibrosisData[i,245])){DataContinuousB[i,86:87]=NA}
  else {DataContinuousB[i,86:87]=str_split_fixed(FibrosisData[i,245],
                                                 "/", 2)}
}
stopifnot(is.na(FibrosisData[1:109,245])==is.na(DataContinuousB$V86))
stopifnot(is.na(FibrosisData[1:109,245])==is.na(DataContinuousB$V87))

#Name columns with variable names
colnames(DataContinuousB)<-c("Age","AgeDiag","Symptoms","PackYears","AlcoholScore",
                             "Weight","BMI","Waist","AST","ALT","ALKP","ALB","Tbili",
                             "Dbili","GGT","PT","INR","Creatinine","Glucose",
                             "Ferritin","Cholesterol","HDL","LDL","Triglycerides",
                             "HbA1C","Insulin","Homair","WBC","Hemo","Platelet",
                             "Apri","Fib4","FVC","FVCp","FEV1","FEV1p","FEV1FVC",
                             "FEV1FVCp","VC","VCp","RV","RVp","TLC","TLCp","RVTLC",
                             "RVTLCp","DLCO","DLCOp","Polymer","Crp","Fibroscan",
                             "Sodium","Potassium","Chloride","Carbon","Urea",
                             "Calcium","Phosphorus","Magnesium","Iron","Irontibc",
                             "PTT","RBC","Hema","MCV","MCH","MCHC","CHCM","RDW","MPV",
                             "Neutrophils","Lymphocytes","Monocytes","Eosinophils",
                             "Basophils","LUC","NeutrophilsA","LymphocytesA",
                             "MonocytesA","EosinophilsA","BasophilsA","Temp",
                             "HR","RespRate","PulseOx","Systolic","Diastolic")
#Make all of the continuous data numeric

DataNumericB<-mutate_all(DataContinuousB, function(x) as.numeric(as.character(x)))
#got waring: NA's induced by coercion due to the above line. Working to figure out which NAs were induced by coercion
  index<-is.na(DataNumericB)!=is.na(DataContinuousB)#make a dataframe that is true only when the NA's were coerced
  colnames(index)<-colnames(DataContinuousB)
  colSums(index==TRUE)#discover it is c reactive protein and insulin that have NAs induced by coercion
  #NAs for insulin are fine, represent empty cells, have written Dr. Serban to ask about NAs for CRP
  
NACoerced<-DataContinuousB[index]

#Now for the categorical variables
DataCategoricalB<-as.data.frame(matrix(nrow=109,ncol=22))
#Make a list of all the baseline categorical variables
MyList1<-list(PlasmaData[,3],PlasmaData[,4],PlasmaData[,5],
             PlasmaData[,15],PlasmaData[,29],PlasmaData[,33],
             PlasmaData[,65],PlasmaData[,67],PlasmaData[,69],
             PlasmaData[,70],PlasmaData[,71],PlasmaData[,72],
             PlasmaData[,73],PlasmaData[,74],PlasmaData[,76])

MyList2<-list(FibrosisData[,227],FibrosisData[,228],FibrosisData[,229],
             FibrosisData[,230],FibrosisData[,231],FibrosisData[,232],
             FibrosisData[,233])
for (i in 1:length(MyList1)){#for each item in the list
  DataCategoricalB[,i]<-PatientIDSort(DataCategoricalB[,i],MyList1[[i]],PlasmaData[,1])#Put that data in the ith column, each patient in the row with their ID number
}
for (i in 1:length(MyList2)){
  DataCategoricalB[,i+length(MyList1)]<-PatientIDSort(DataCategoricalB[,i+length(MyList1)],
                                                      MyList2[[i]],FibrosisData[1:109,1])
}
#name the columns
colnames(DataCategoricalB)<-c("Sex","Race","Ethnicity","Smoker","UseOx",
                              "AugTherapy","HxHypertension","MedsHypertension",
                              "MedsDiabetes","Statins","Fibrates","InsulinMed",
                              "DxDiabetes","MedsDiabetesB","MetSynd",
                              "HepBAntibody","HepBAntigen","HepC","HIV",
                              "Antinuclear","ANA","Antimito")
#Change into One-Hot type of variables
DataOneHotB<-as.data.frame(c(mutate_all(DataCategoricalB,~ifelse(.==1,1,0)),#get all cases where a variable equals 1
               mutate_all(DataCategoricalB,~ifelse(.==0,1,0))))#and all cases where it equals 0
colnames(DataOneHotB)<-c("Male","White","Hispanic","FormerSmoker","UsesOx","OnAug",
                         "HasHypertension","TakesMedsHypertension","TakesMedsDiabetes",
                         "TakesStatins","TakesFibrates","TakesInsulin","HasDxDiabetes",
                         "TakesMedsDiabetesB","HasMetSynd","HepBAntibodyPos","HepBAntigenPos",
                         "HepCPos","HIVPos","AntinucleaPos","ANAPos","AntimitoPos",
                         "Female","MeaninglessRace","MeaninglessEthnicity","NeverSmoker",
                         "NoUseOx","OffAug","NoHxHypertension","NoMedsHypertension",
                         "NoMedsDiabetes","NoStatins","NoFibrates","NoInsulin",
                         "NoDxDiabetes",
                         "NoMedsDiabetesB","NoMetSynd","HepBAntibodyNeg",
                         "HepBAntigenNeg","HepCNeg","HIVNeg","AntinucleaNeg","ANANeg",
                         "AntimitoNeg")
DataOneHotB$MeaninglessEthnicity<-NULL
DataOneHotB$MeaninglessRace<-NULL
DataOneHotB$Female<-ifelse(DataCategoricalB$Sex==2,1,0)#female sex variable needs fixed
stopifnot(is.na(DataOneHotB$Female)==is.na(DataCategoricalB$Sex))
stopifnot(is.na(DataOneHotB$Male)==is.na(DataCategoricalB$Sex))
DataOneHotB$CurrentSmoker<-ifelse(DataCategoricalB$Smoker==2,1,0)#current smoker variable
DataOneHotB$NonHispanic<-ifelse(DataCategoricalB$Ethnicity==2,1,0)#nonhispanic variable
names_to_select<-c("HepBAntibodyInd","HepBAntibodyPos","HepBAntibodyNeg")
DataOneHotB[,names_to_select]<-""
DataOneHotB[,names_to_select]<-OneHotVariableRename(DataCategoricalB$HepBAntibody,
                                                    DataOneHotB[,names_to_select])
names_to_select<-c("HepBAntigenInd","HepBAntigenPos","HepBAntigenNeg")
DataOneHotB[,names_to_select]<-""
DataOneHotB[,names_to_select]<-OneHotVariableRename(DataCategoricalB$HepBAntigen,
                                                    DataOneHotB[,names_to_select])

names_to_select<-c("HepCInd","HepCPos","HepCNeg")
DataOneHotB[,names_to_select]<-""
DataOneHotB[,names_to_select]<-OneHotVariableRename(DataCategoricalB$HepC,
                                                    DataOneHotB[,names_to_select])
names_to_select<-c("HIVInd","HIVPos","HIVNeg")
DataOneHotB[,names_to_select]<-""
DataOneHotB[,names_to_select]<-OneHotVariableRename(DataCategoricalB$HIV,
                                                    DataOneHotB[,names_to_select])

names_to_select<-c("AntinucleaInd","AntinucleaPos","AntinucleaNeg")
DataOneHotB[,names_to_select]<-""
DataOneHotB[,names_to_select]<-OneHotVariableRename(DataCategoricalB$Antinuclear,
                                                    DataOneHotB[,names_to_select])
names_to_select<-c("ANAInd","ANAPos","ANANeg")
DataOneHotB[,names_to_select]<-""
DataOneHotB[,names_to_select]<-OneHotVariableRename(DataCategoricalB$ANA,
                                                    DataOneHotB[,names_to_select])

names_to_select<-c("AntimitoInd","AntimitoPos","AntimitoNeg")
DataOneHotB[,names_to_select]<-""
DataOneHotB[,names_to_select]<-OneHotVariableRename(DataCategoricalB$Antimito,
                                                    DataOneHotB[,names_to_select])

DataOneHotNumericB<-mutate_all(DataOneHotB, function(x) as.numeric(as.character(x)))#make sure one hot variables are all numeric
index<-is.na(DataOneHotNumericB)!=is.na(DataOneHotB)#make a dataframe that is true only when the NA's were coerced
colnames(index)<-colnames(DataOneHotB)
colSums(index==TRUE)#every column has sum of zero, so no NAs induced by coercion

BaselineAll<-cbind.data.frame(DataNumericB,DataOneHotNumericB)#put together all predictors into one dataframe
#Get rid of those variables which are entirely full of NAs, and also blood pressure which is not a number, and ethnicity because n=2
BaselineAll<-BaselineAll[!names(BaselineAll) %in% c("Race","HIV","ANA",
                                                    "Antimito","Antinuclear","HepC","HepBAntigen",
                                                    "HepBAntibody","Ethnicity",
                                                    "AgeDiag","Symptoms",
                                                    "PackYears","Waist",
                                                    "Crp","LUC",
                                                    "Insulin","TakesMedsDiabetesB",
                                                    "NoMedsDiabetesB")]#get rid of variables Nazli doesn't want


#Now to build vectors for the outcomes of interest
FibrosisBaseline<-matrix(nrow=109,ncol=1)
FibrosisBaseline<-PatientIDSort(FibrosisBaseline,PlasmaData[,166],PlasmaData[,1])#Puts baseline fibrosis score in row according to patient ID
FibrosisBaseline<-as.data.frame(as.numeric(FibrosisBaseline))
FibrosisProbabilityb <- matrix(nrow=109,ncol=1)
for (i in 1:109){#for each patient ID
  if (is.na(FibrosisBaseline[i,1]))#keeps as NA if entry is NA
  {FibrosisProbabilityb[i,1]=FibrosisBaseline[i,1]}
  else if (FibrosisBaseline[i,1]==0)#assigns 0 if fibrosis score is F0
  {FibrosisProbabilityb[i,1]=0}
  else
  {FibrosisProbabilityb[i,1]=1}#assigns 1 if fibrosis score is F1, or F2, or F3
}
FibrosisProbabilityb<-as.data.frame(as.numeric(FibrosisProbabilityb))
Fibrosis3<-matrix(nrow=109,ncol=1)
Fibrosis3<-PatientIDSort(Fibrosis3,PlasmaData[,167],PlasmaData[,1])#year 3 fibrosis score put in jth row for jth patient ID
Fibrosis3<-as.data.frame(as.numeric(Fibrosis3))
FibrosisProbability3 <- matrix(nrow=109,ncol=1)
for (i in 1:109){#for each patient ID
  if (is.na(Fibrosis3[i,1]))#if NA keep NA
  {FibrosisProbability3[i,1]=Fibrosis3[i,1]}
  else if (Fibrosis3[i,1]==0)#assigns 0 if fibrosis score is F0
  {FibrosisProbability3[i,1]=0}
  else
  {FibrosisProbability3[i,1]=1}#assigns 1 if fibrosis score is F1, or F2, or F3
}
FibrosisProbability3<-as.data.frame(as.numeric(FibrosisProbability3))
ChangeinFibrosis<-Fibrosis3-FibrosisBaseline#compute change in score from year 1 to year 3
DensityBaselinePreprocessed<-matrix(nrow=109,ncol=1)
DensityBaselinePreprocessed<-PatientIDSort(DensityBaselinePreprocessed,TwoC1Data[,7],TwoC1Data[,1])#2C1 events/mm^2 by patient ID
DensityBaseline<-as.data.frame(as.numeric(DensityBaselinePreprocessed))
index<-is.na(DensityBaselinePreprocessed)!=is.na(DensityBaseline)#NAs induced by coercion here
DensityBaselinePreprocessed[index]
#"NS" corresponds to no slide, meaning no data so NA is appropriate for these entries
#"NL" corresponds to no liver tissue, meaning no data so NA is appropriate for these entries as well
TwoCOneAreaPercentPreprocessed<-matrix(nrow=109,ncol=1)
TwoCOneAreaPercentPreprocessed<-PatientIDSort(TwoCOneAreaPercentPreprocessed,TwoC1Data[,6],TwoC1Data[,1])#2C1 % area by patient ID
TwoCOneAreaPercent<-as.data.frame(as.numeric(TwoCOneAreaPercentPreprocessed))
index<-is.na(TwoCOneAreaPercentPreprocessed)!=is.na(TwoCOneAreaPercent)#NAs induced by coercion here
TwoCOneAreaPercentPreprocessed[index]#Again "NS" and "NL" are appropriate for NA

#make data frames for each type of analysis with nas removed
BaselineAll1<-BaselineAll[!is.na(FibrosisBaseline),]#for fibrosis baseline outcomes

BaselineAll2<-BaselineAll[!is.na(Fibrosis3),]#for fibrosis year 3 outcomes and change in fibrosis
BaselineAll3<-BaselineAll[!is.na(TwoCOneAreaPercent),]#for 2C1 outcomes

#Make a data frame and output vector with all no-fibrosis cases duplicated
FP<-FibrosisProbabilityb[!is.na(FibrosisProbabilityb)]#make a vector without nas
FP<-as.data.frame(FP)
DFtoadd<-BaselineAll1[FP==0,]#pick the rows corresponding to no fibrosis
FPtoadd<-FP[FP==0,drop=FALSE]#pick only fibrosis probability of 0
FPtoadd<-as.data.frame(FPtoadd)
colnames(FPtoadd)<-"FP"
DFBoot<-rbind(BaselineAll1,DFtoadd)#combine data with bootstrapped data
FPBoot<-rbind(FP,FPtoadd)#combine outcome with bootstrapped outcome

#Computing Table1, all statistics here apply to the group of people who did the first biopsy
#calculate the median age
median(BaselineAll1$Age,na.rm=TRUE)
#calculate the minimum age
min(BaselineAll1$Age,na.rm=TRUE)
#calculate the maximum age
max(BaselineAll1$Age,na.rm = TRUE)
#calculate number of males
sum(BaselineAll1$Sex==1,na.rm=TRUE)
#calculate number of females
sum(BaselineAll1$Sex==0,na.rm=TRUE)
#count how many had BMI greater than or equal to 30
sum(BaselineAll1$BMI>=30,na.rm = TRUE)
#count how many had a diagnosis of diabetes
sum(BaselineAll1$DxDiabetes)
#count how many were taking meds for diabetes, take the maximum of this and previous
#value as the number of patients with diabetes
sum(BaselineAll1$MedsDiabetes)
#count the number of patients with a history of hypertension
sum(BaselineAll1$HxHypertension)
#count the number of patients whose bp was high enough to meet the definition of hypertension
sum(BaselineAll1$Systolic>=130)
#count how many patients had both a history of hypertension and a bp above 130
#take sum of previous two numbers and subtract this number to determine how many patients had hypertension
sum(BaselineAll1$Systolic>=130&BaselineAll$HxHypertension==1,na.rm=TRUE)
#count how many patients had metabolic syndrome
sum(BaselineAll1$MetSynd)
#count how many patients had FEV1 greater than or equal to 80
sum(BaselineAll1$FEV1p>=80)
#count how many patients had FEV1 greater than or equal to 50
#subtract previous number to get how many patients had FEV1 between 50 and 80
sum(BaselineAll1$FEV1p>=50)
#count how many patients had FEV1 greater than or equal to 30
#subtract previous number to get how many patients had FEV1 between 30 and 50
sum(BaselineAll1$FEV1p>=30)
#count how many patients had FEV1 below 30
sum(BaselineAll1$FEV1p<30)
#calculate mean value for ALT
mean(BaselineAll1$ALT,na.rm=TRUE)
#calculate standard deviation for ALT
sd(BaselineAll1$ALT,na.rm = TRUE)
#calculate mean for AST
mean(BaselineAll1$AST,na.rm=TRUE)
#calculate standard deviation for AST
sd(BaselineAll1$AST,na.rm=TRUE)
#calculate mean vlaue for GGT
mean(BaselineAll1$GGT,na.rm=TRUE)
#calculate standard deviation for GGT
sd(BaselineAll1$GGT,na.rm = TRUE)
#calculate mean value for alkaline phosphatase
mean(BaselineAll1$ALKP,na.rm=TRUE)
#calculate standard deviation for alkaline phosphatase
sd(BaselineAll1$ALKP,na.rm = TRUE)
#calculate mean value for albumin
mean(BaselineAll1$ALB,na.rm=TRUE)
#calculate standard deviation for albumin
sd(BaselineAll1$ALB,na.rm=TRUE)
#calculate mean value for platelet count
mean(BaselineAll1$Platelet,na.rm=TRUE)
#calculate standard deviation for platelet count
sd(BaselineAll1$Platelet,na.rm=TRUE)
#calculate mean value for prothrombin time
mean(BaselineAll1$PT,na.rm=TRUE)
#calculate standard deviation for prothrombin time
sd(BaselineAll1$PT,na.rm=TRUE)
#calculate mean value for partial thromboplastin time
mean(BaselineAll1$PTT,na.rm=TRUE)
#calculate standard deviation for partial thromboplastin time
sd(BaselineAll1$PTT,na.rm = TRUE)
#calculate mean for glucose
mean(BaselineAll1$Glucose,na.rm=TRUE)
#claculate standard deviation for glucose
sd(BaselineAll1$Glucose,na.rm = TRUE)
#calculate mean for cholesterol
mean(BaselineAll1$Cholesterol,na.rm=TRUE)
#calculate standard deviation for cholesterol
sd(BaselineAll1$Cholesterol,na.rm = TRUE)
#calculate mean for triglycerides
mean(BaselineAll1$Triglycerides,na.rm=TRUE)
#calculate standard deviation for triglycerides
sd(BaselineAll1$Triglycerides,na.rm=TRUE)
#claculate mean for high density lipoprotein
mean(BaselineAll1$HDL,na.rm=TRUE)
#calculate standard deviation for high density lipoprotein
sd(BaselineAll1$HDL,na.rm=TRUE)
#Get rid of nas in baseline fibrosis score outcome vector
FibrosisBaseline<-FibrosisBaseline[!is.na(FibrosisBaseline)]
#make an index of which cases have baseline fibrosis score of 0
nofibindex<-which(FibrosisBaseline==0)
#Make a data frame of all predictors for those patients without fibrosis
NoFib<-BaselineAll1[nofibindex,]
#Make a data frame of all predictors for those patients with fibrosis
Fib<-BaselineAll1[-nofibindex,]
#calculate the median age of those without fibrosis
median(NoFib$Age,na.rm=TRUE)
#calculate minimum age of those without fibrosis
min(NoFib$Age,na.rm = TRUE)
#calculate the maximum age of those without fibrosis
max(NoFib$Age,na.rm = TRUE)
#calculate the median age of those with fibrosis
median(Fib$Age,na.rm=TRUE)
#calculate the minimum age of those with fibrosis
min(Fib$Age,na.rm = TRUE)
#calculate the maximum age of those with fibrosis
max(Fib$Age,na.rm = TRUE)
#Count number of males without fibrosis
sum(NoFib$Sex==1,na.rm=TRUE)
#count number of females without fibrosis
sum(NoFib$Sex==0,na.rm=TRUE)
#count number of males with fibrosis
sum(Fib$Sex==1,na.rm=TRUE)
#count number of females with fibrosis
sum(Fib$Sex==0,na.rm=TRUE)
#count number of people with BMI greater than 30 without fibrosis
sum(NoFib$BMI>=30)
#count number of people with BMI greater than 30 with fibrosis
sum(Fib$BMI>=30)
#count number of people on medication for diabetes without fibrosis
sum(NoFib$MedsDiabetes)
#count number of people on medication for diabetes with fibrosis
sum(Fib$MedsDiabetes)
#count number of people with a history of hypertension and no fibrosis
sum(NoFib$HxHypertension)
#count number of people with bp greater than 130 and no fibrosis
sum(NoFib$Systolic>=130)
#count number of people with both a history of hypertension and a bp greater than 130
#add previous two numbers and subtract this one to get number of people without fibrosis who have hypertension
sum(NoFib$HxHypertension&NoFib$Systolic>=130)
#count number of people with history of hypertension who have fibrosis
sum(Fib$HxHypertension)
#count number of people with bp greater than 130 who have fibrosis
sum(Fib$Systolic>=130)
#count number of people with bp greater than 130 and a history of hypertension who have fibrosis
#add previous two numbers and subtract this number to find total number of people who have hypertension and fibrosis
sum(Fib$HxHypertension&Fib$Systolic>=130)
#count how many people have metabolic syndrome without fibrosis
sum(NoFib$MetSynd)
#count how many people have metabolic syndrome with fibrosis
sum(Fib$MetSynd)
#count how many people have FEV1 greater than or equal to 80 without fibrosis
sum(NoFib$FEV1p>=80)
#count how many people have FEV1 greater than or equal to 50 without fibrosis
#subtract previous number to get how many have FEV1 between 50 and 80
sum(NoFib$FEV1p>=50)
#count how many people have FEV1 greater than or equal to 30 without fibrosis
#subtract previous number to get how many have FEV1 between 30 and 50
sum(NoFib$FEV1p>=30)
#count how many people have FEV1 less than 30 without fibrosis
sum(NoFib$FEV1p<30)
#count how many people have FEV1 greater than or equal to 80 with fibrosis
sum(Fib$FEV1p>=80)
#count how many people have FEV1 greater than or equal to 50 with fibrosis
#subtract previous number to find number of people with FEV1 between 50 and 80
sum(Fib$FEV1p>=50)
#count how many people have FEV1 greater than or equal to 30 with fibrosis
#subtract previous number to get how many have FEV1 between 30 and 50
sum(Fib$FEV1p>=30)
#count how many people have FEV1 less than 30 with fibrosis
sum(Fib$FEV1p<30)
#calculate mean of ALT values for those without fibrosis
mean(NoFib$ALT,na.rm=TRUE)
#claculate mean of ALT values for those with fibrosis
mean(Fib$ALT,na.rm=TRUE)
#calculate standard deviation of ALT values for those without fibrosis
sd(NoFib$ALT,na.rm = TRUE)
#calculate standard deviation of ALT values for those with fibrosis
sd(Fib$ALT,na.rm = TRUE)
#calculate mean of AST values for those with fibrosis
mean(Fib$AST,na.rm=TRUE)
#calculate mean of AST values for those without fibrosis
mean(NoFib$AST,na.rm=TRUE)
#calculate standard devieation of AST values for those without fibrosis
sd(NoFib$AST,na.rm = TRUE)
#calculate standard deviation of AST values for those with fibrosis
sd(Fib$AST,na.rm = TRUE)
#calculate mean of GGT values for those without fibrosis
mean(NoFib$GGT,na.rm=TRUE)
#calculate mean of GGT values for those with fibrosis
mean(Fib$GGT,na.rm=TRUE)
#calculate standard deviation of GGT values for those with fibrosis
sd(Fib$GGT,na.rm=TRUE)
#calculate standard deviation of GGT values for those without fibrosis
sd(NoFib$GGT,na.rm=TRUE)
#calculate mean of alkaline phosphatase values for those without fibrosis
mean(NoFib$ALKP,na.rm=TRUE)
#calculate mean of alkaline phosphatase values for those with fibrosis
mean(Fib$ALKP,na.rm=TRUE)
#calculate standard deviation of alkaline phosphatase values for those without fibrosis
sd(NoFib$ALKP,na.rm=TRUE)
#calculate standard deviation of alkaline phosphatase values for those with fibrosis
sd(Fib$ALKP,na.rm=TRUE)
#calculate mean of albumin values for those without fibrosis
mean(NoFib$ALB,na.rm=TRUE)
#calculate mean of albumin values for those with fibrosis
mean(Fib$ALB,na.rm=TRUE)
#calculate standard deviation of albumin values for those without fibrosis
sd(NoFib$ALB,na.rm=TRUE)
#calculate standard deviation of albumin values for those with fibrosis
sd(Fib$ALB,na.rm=TRUE)
#calculate mean of platlet count for those without fibrosis
mean(NoFib$Platelet,na.rm=TRUE)
#calculate mean of platelet count for those with fibrosis
mean(Fib$Platelet,na.rm=TRUE)
#calculate standard deviation of platelet count for those without fibrosis
sd(NoFib$Platelet,na.rm=TRUE)
#calculate standard deviation of platelet count for those with fibrosis
sd(Fib$Platelet,na.rm=TRUE)
#calculate mean of prothrombin time for those without fibrosis
mean(NoFib$PT,na.rm=TRUE)
#calculate mean of prothrombin time for those with fibrosis
mean(Fib$PT,na.rm=TRUE)
#calculate standard deviation of prothrombin time for those without fibrosis
sd(NoFib$PT,na.rm=TRUE)
#calculate standard deviation of prothrombin time for those with fibrosis
sd(Fib$PT,na.rm=TRUE)
#calculate mean of PTT for those without fibrosis
mean(NoFib$PTT,na.rm=TRUE)
#calculate mean of PTT for those with fibrosis
mean(Fib$PTT,na.rm=TRUE)
#calculate standard deviation of PTT for those without fibrosis
sd(NoFib$PTT,na.rm=TRUE)
#calculate standard deviation of PTT for those with fibrosis
sd(Fib$PTT,na.rm=TRUE)
#calculate mean of glucose for those without fibrosis
mean(NoFib$Glucose,na.rm=TRUE)
#calculate mean of glucose for those with fibrosis
mean(Fib$Glucose,na.rm=TRUE)
#calculate standard deviation of glucose for those without fibrosis
sd(NoFib$Glucose,na.rm=TRUE)
#calculate standard deviation of glucose for those with fibrosis
sd(Fib$Glucose,na.rm=TRUE)
#calculate mean of cholesterol for those without fibrosis
mean(NoFib$Cholesterol,na.rm=TRUE)
#calculate mean of tholesterol for those with fibrosis
mean(Fib$Cholesterol,na.rm=TRUE)
#calculate standard deviation of cholesterol for those without fibrosis
sd(NoFib$Cholesterol,na.rm=TRUE)
#claculate standard deviation of cholesterol for those with fibrosis
sd(Fib$Cholesterol,na.rm=TRUE)
#calculate mean of triclycerides for those without fibrosis
mean(NoFib$Triglycerides,na.rm=TRUE)
#calculate mean of triglycerides for those with fibrosis
mean(Fib$Triglycerides,na.rm=TRUE)
#calculate standard deviation of triglycerides for those without fibrosis
sd(NoFib$Triglycerides,na.rm=TRUE)
#calculate standard deviation of triglycerides for those with fibrosis
sd(Fib$Triglycerides,na.rm=TRUE)
#calculate mean of HDL for those without fibrosis
mean(NoFib$HDL,na.rm=TRUE)
#calculate mean of HDL for those with fibrosis
mean(Fib$HDL,na.rm=TRUE)
#calculate standard deviation of HDL for those without fibrosis
sd(NoFib$HDL,na.rm=TRUE)
#calculate standard deviation of HDL for those with fibrosis
sd(Fib$HDL,na.rm=TRUE)
#get rid of NAs in fibrosis probability baseline outcome vector
FibrosisProbabilityb<-FibrosisProbabilityb[!is.na(FibrosisProbabilityb)]
#make a data frame with predictors and fibrosis probabilityt baseline outcome vector
ADataFrame<-cbind(BaselineAll1,FibrosisProbabilityb)
#Test to see whether age is different depending on whether or not one has fibrosis
p.values1<-wilcox.test(Age~FibrosisProbabilityb,data=ADataFrame)
#Test to see whether ALT is different depending on whether or not one has fibrosis
p.values2<-wilcox.test(ALT~FibrosisProbabilityb,data=ADataFrame)
#test to see whether AST is different depending on whether or not one has fibrosis
p.values3<-wilcox.test(AST~FibrosisProbabilityb,data=ADataFrame)
#test to see whether GGT is different depending on whether or not one has fibrosis
p.values4<-wilcox.test(GGT~FibrosisProbabilityb,data=ADataFrame)
#test to see whether alkaline phosphatase is different depending on whether or not one has fibrosis
p.values5<-wilcox.test(ALKP~FibrosisProbabilityb,data=ADataFrame)
#test to see whether albumin is different depending on whether or not one has fibrosis
p.values6<-wilcox.test(ALB~FibrosisProbabilityb,data=ADataFrame)
#test to see whether platelet count is different depending on whether or not one has fibrosis
p.values7<-wilcox.test(Platelet~FibrosisProbabilityb,data=ADataFrame)
#test to see whether prothrombin time is different depending on whether or not one has fibrosis
p.values8<-wilcox.test(PT~FibrosisProbabilityb,data=ADataFrame)
#test to see whether PTT is different depending on whether or not one has fibrosis
p.values9<-wilcox.test(PTT~FibrosisProbabilityb,data=ADataFrame)
#test to see whether glucose is different depending on whether or not one has fibrosis
p.values10<-wilcox.test(Glucose~FibrosisProbabilityb,data=ADataFrame)
#test to see whether cholesterol is different depending on whether or not one has fibrosis
p.values11<-wilcox.test(Cholesterol~FibrosisProbabilityb,data=ADataFrame)
#test to see whether triglycerides are different depending on whether or not one has fibrosis
p.values12<-wilcox.test(Triglycerides~FibrosisProbabilityb,data=ADataFrame)
#test to see whether HDL is different depending on whether or not one has fibrosis
p.values13<-wilcox.test(HDL~FibrosisProbabilityb,data=ADataFrame)
#make a data frame with just the variables sex and fibrosis presence/absence
Sexdataframe<-data.frame(ADataFrame$Male,ADataFrame$FibrosisProbabilityb)
#Make the data frame into a table
Sexdataframe<-table(ADataFrame$Male,ADataFrame$FibrosisProbabilityb)
#perform the chi squared test to see whether there is a difference in the proportions
#of male and female patients with and without fibrosis
p.values14<-chisq.test(Sexdataframe)
#make a data frame with just the variables BMI and fibrosis presence/absence
BMIdata<-data.frame(ADataFrame$BMI,ADataFrame$FibrosisProbabilityb)
#build and index that tells which rows correspond to BMI greater than or equal to 30
myindex<-which(BMIdata$ADataFrame.BMI>=30)
#Make the first variable in the BMI data frame 1 when a patient's BMI is greater than or equal to 30
BMIdata[myindex,1]<-1
#and zero when a patient's BMI is less than 30
BMIdata[-myindex,1]<-0
#make the data frame into a table so we can perform the chi squared test
BMItable<-table(BMIdata$ADataFrame.BMI,BMIdata$ADataFrame.FibrosisProbabilityb)
#perform the chi squared test to see whether the proportion of those with a BMI greater
#than or equalt to 30 is different in those without and with fibrosis
p.values15<-chisq.test(BMItable)
#make a data frame with just those who take meds for diabetes (this variable is more
#reliable than the variable DxDiabetes in telling who has diabetes) and presence/absence fibrosis
Diabetesdata<-data.frame(ADataFrame$TakesMedsDiabetes,ADataFrame$FibrosisProbabilityb)
#make a table so we can perform a chi squared test
DiabetesTable<-table(Diabetesdata$ADataFrame.TakesMedsDiabetes,
                     Diabetesdata$ADataFrame.FibrosisProbabilityb)
#test to see whether the proportion of people with diabetes differs between the group
#with fibrosis and the group without fibrosis
p.values16<-chisq.test(DiabetesTable)
#Make a data frame with just who has metabolic syndrome and presence/absence of fibrosis
Metdata<-data.frame(ADataFrame$HasMetSynd,ADataFrame$FibrosisProbabilityb)
#Transform the data frame into a table so we can perform a chi squared test
MetTable<-table(Metdata$ADataFrame.HasMetSynd,Metdata$ADataFrame.FibrosisProbabilityb)
#Test to see whether the proportion of who has metabolic syndrome differs between
#the group with fibrosis and the group without fibrosis
p.values17<-chisq.test(MetTable)
#make a data frame with just FEV1 and presence/absence of fibrosis
LungData<-data.frame(ADataFrame$FEV1p,ADataFrame$FibrosisProbabilityb)
#build an index that tells which patients have FEV1 greater than or equal to 80
index80<-which(LungData$ADataFrame.FEV1p>=80)
#Make a data frame for the greater than or equalt to 80 group
Data80<-LungData
#use the index to make the first column 1 when a patient has FEV1 greater than or equal to 80
Data80[index80,1]<-1
#and zero otherwise
Data80[-index80,1]<-0
#transform the data frame into a table so we can perform a chi squared test
Table80<-table(Data80$ADataFrame.FEV1p,Data80$ADataFrame.FibrosisProbabilityb)
#test whether the proportion of people with FEV1 greater than or eqault to 80 is different
#between those with and without fibrosis
chisq.test(Table80)
#make a new index that marks the rows for those whose FEV1 is greater than or equal
#to 50 and less than 80
index50<-which(LungData$ADataFrame.FEV1p>=50&LungData$ADataFrame.FEV1p<80)
#Make a data frame for this group by copying the lung data data frame
Data50<-LungData
#Use the index to make the first column 1 where a patient's FEV1 is between 50 and 80
Data50[index50,1]<-1
#and zero otherwise
Data50[-index50,1]<-0
#make a table from this dataframe so we can perform a chi squared test
Table50<-table(Data50$ADataFrame.FEV1p,Data50$ADataFrame.FibrosisProbabilityb)
#test to see whether the proporotion of people whose FEV1 is between 50 and 80 is 
#different for those with and without fibrosis
chisq.test(Table50)
#build an index that marks the rows corresponding to patients whose FEV1 is between 30 and 50
index30<-which(LungData$ADataFrame.FEV1p>=30&LungData$ADataFrame.FEV1p<50)
#copy the lung data frame again
Data30<-LungData
#Make the first column 1 for those patients whose FEV1 is between 30 and 50
Data30[index30,1]<-1
#and zero otherwise
Data30[-index30,1]<-0
#make a table from the data frame so we can calculate the chi squared test
Table30<-table(Data30$ADataFrame.FEV1p,Data30$ADataFrame.FibrosisProbabilityb)
#calculate whether there is a difference in the proportion of patients with fEV1 between
#30 and 50 for those with and without fibrosis
chisq.test(Table30)
#make a new index which marks those rows for patients whose FEV1 is below 30
index0<-which(LungData$ADataFrame.FEV1p<30)
#copy the lung data frame again
Data0<-LungData
#make the first column 1 when a patient's FEV1 is less than 30
Data0[index0,1]<-1
#and zero otherwise
Data0[-index0,1]<-0
#make a table from the data frame so we can calculate a chi squared test
Table0<-table(Data0$ADataFrame.FEV1p,Data0$ADataFrame.FibrosisProbabilityb)
#compute whether the proportion of people with an FEV1 less than 30 is different for 
#those with and without fibrosis
chisq.test(Table0)
# compute FEV1 as vector chi-sqared test: replaces all the previous lung chi squared tests
#build a data frame with the number of cases for each level for fibrosis and no fibrois
BDataFrame<-data.frame(mild=c(12,20),
                       moderate=c(8,16),
                       severe=c(5,23),
                       verysevere=c(3,9))
#switch columns and rows to get the data frame the way we want it
BDataFrame<-t(BDataFrame)
#name the columns according to which doesn't and does have fibrosis
colnames(BDataFrame)<-c("NoFib","Fib")
#make it a data frame again because for some reason it becomes something else during these steps
BDataFrame<-as.data.frame(BDataFrame)
#perform a vectorized chi squared test on this data frame containing the number of cases in each category
p.values18<-chisq.test(BDataFrame$NoFib, p = BDataFrame$Fib, rescale.p = TRUE)



#computing chi squared test for hypertension
#Start by making a new column with 1 for having hypertension 
#and 0 for not having hypertension
#make a data frame with hypertension only and presence/absence of fibrosis
DataHy<-cbind(ADataFrame$HasHypertension,ADataFrame$FibrosisProbabilityb)
#make an index marking those rows that either are 1 for history of hypertension or
#have a systolic bp greater than or equal to 130
indexHy<-which(ADataFrame$HasHypertension==1|ADataFrame$Systolic>=130)
#Change the first column to be one when either history of hypertension is one or
#systolic bp is greater than or equal to 130
DataHy[indexHy,1]<-1
#and zero otherwise
DataHy[-indexHy,1]<-0
DataHy<-as.data.frame(DataHy)
#make a table using the data frame so we can perform a chi squared test
TableHy<-table(DataHy$V1,DataHy$V2)
#Test whether the proportion of those with and without hypertension is different between
#those with and without fibrosis
p.values19<-chisq.test(TableHy)
#make a vector of all saved p-values so that we can perform benjamini-hochberg adjustment
pvalues<-c(p.values1$p.value,p.values2$p.value,p.values3$p.value,p.values4$p.value,p.values5$p.value,
           p.values6$p.value,p.values7$p.value,p.values8$p.value,p.values9$p.value,p.values10$p.value,p.values11$p.value,
           p.values12$p.value,p.values13$p.value,p.values14$p.value,p.values15$p.value,p.values16$p.value,
           p.values17$p.value,p.values18$p.value,p.values19$p.value)
#Adjust the p-values using the BH method to get new pvalues
newpvalues<-p.adjust(pvalues,method = "hochberg",n=length(pvalues))


#Main analysis
#run multinomial function for baseline fibrosis score
Results.FB<-MyMult(BaselineAll1,FibrosisBaseline)
Results.FB[[4]]
ggsave("SobolMDAFB.jpg",width = 10,height = 5,units = "in",dpi=300)
#run binary function for baseline fibrosis probability
Results.FP<-MyBin(BaselineAll1,FibrosisProbabilityb)
Results.FP[[9]]
ggsave("CoeficientsFP.jpg",width = 10,height = 5,units = "in",dpi=300)

#Try baseline fibrosis probability with bootstrapped data
Results.FPboot<-MyBin(DFBoot,FPBoot)
Results.FPboot[[9]]
ggsave("SobolMDAFP.jpg",width = 10,height = 5,units = "in",dpi=300)
#calculate matthews correlation coefficient
RFTP<-Results.FPboot[[11]][[1]][2,2] #true positives for random forest
RFTN<-Results.FPboot[[11]][[1]][1,1] #true negatives for random forest
RFFP<-Results.FPboot[[11]][[1]][2,1]#False positives for random forest
RFFN<-Results.FPboot[[11]][[1]][1,2]#False negatives for random forest
TP<-Results.FPboot[[7]][[1]][2,2]#true positives for elastic net regression
TN<-Results.FPboot[[7]][[1]][1,1]#true negatives for ENR
FP<-Results.FPboot[[7]][[1]][2,1]#false positives for ENR
FN<-Results.FPboot[[7]][[1]][1,2]#false negatives for ENR
#Compute accuracy for elastic net regression
Accuracy1<-(TP+TN)/(TP+TN+FP+FN)
#compute accuracy for random forest
Accuracy2<-(RFTP+RFTN)/(RFTP+RFTN+RFFP+RFFN)
MCCENR<-(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))#matthews correlation coefficient for elastic net regression
MCCRF<- (RFTP*RFTN-RFFP*RFFN)/sqrt((RFTP+RFFP)*(RFTP+RFFN)*(RFTN+RFFP)*(RFTN+RFFN))#matthews correlation coefficient for random forest

#run multinomial function with year 3 fibrosis score
Results.F3<-MyMult(BaselineAll2,Fibrosis3)
Results.F3[[4]]
ggsave("SobolMDAF3.jpg",width = 10,height = 5,units = "in",dpi=300)
#run multinomial function with change in fibrosis score
Results.CH<-MyMult(BaselineAll2,ChangeinFibrosis)
Results.CH[[4]]
ggsave("SobolMDACh.jpg",width = 10,height = 5,units = "in",dpi=300)
#with second 2C1 outcome
Results.2C1D<-MyCts(BaselineAll3,DensityBaseline)
Results.2C1D[[4]]
ggsave("Coefficients2C1D.jpg",width = 10,height = 5,units = "in",dpi=300)
Results.2C1D[[7]]
ggsave("SobolMDA2C1D.jpg",width = 10,height = 5,units = "in",dpi=300)
#with third 2C1 outcome
Results.2C1P<-MyCts(BaselineAll3,TwoCOneAreaPercent)
Results.2C1P[[4]]
ggsave("Coefficients2C1P.jpg",width = 10,height = 5,units = "in",dpi=300)
Results.2C1P[[7]]
ggsave("SobolMDA2C1P.jpg",width = 10,height = 5,units = "in",dpi=300)


#For making figure with fibrosis scores by sex
ADataFrame<-cbind(BaselineAll,FibrosisBaseline)#combine predictors with outcome
ADataFrame<-ADataFrame[!is.na(FibrosisBaseline),,drop=FALSE]#get rid of those rows with nas in outcome

n<-ncol(ADataFrame)
m<-n+1
ADataFrame<-cbind(ADataFrame,matrix(nrow = 
                                      nrow(ADataFrame),ncol=1))#add a new column to the data frame
ADataFrame<-as.data.frame(ADataFrame)
colnames(ADataFrame)[m]<-"Sex"#name the new column "Sex"
for (i in 1:nrow(ADataFrame)){#for each row in the data frame
  if (ADataFrame$Male[i]==0){ADataFrame$Sex[i]="Female"}#if the numeric value for sex is 0, call sex female
  else if (ADataFrame$Male[i]==1){ADataFrame$Sex[i]="Male"}#if the numeric value for sex is 1, call sex male
}
ADataFrame<-ADataFrame %>%
  rename(
    "FibrosisBaseline"="as.numeric(FibrosisBaseline)"#rename variable so it's easier to refernece
  )
ggplot(data=ADataFrame,aes(x=FibrosisBaseline,group=Sex))+#plot baseline fibrosis score
  #as a bar chart with separate bars by sex
  geom_bar(aes(x=FibrosisBaseline,color=Sex,fill=Sex),
           position="dodge")+#don't put the bars on top of each other
  labs(x="Baseline Fibrosis Score",y="Count")+#label the axes
  theme(text=element_text(size=rel(5),family="sans"),
        legend.text=element_text(size=rel(5),family="sans"))#make the text large
ggsave("SexFibScore.jpg",width = 10,height = 5,units = "in",dpi=300)#save the figure in publication level dpi

