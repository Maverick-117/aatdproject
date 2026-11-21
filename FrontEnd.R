#Load libraries
library(ggplot2)
library(viridis)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(glmnet)
library(ranger)

########################################
########################################
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
TwoC1Data<-TwoC1Data %>%
  mutate_at("biopsy_1_baseline_2c1_ihc_data_from_nikon_analysis_and_scan",
            str_replace,"B1-2C1-00","")
TwoC1Data<-TwoC1Data %>%
  mutate_at("biopsy_1_baseline_2c1_ihc_data_from_nikon_analysis_and_scan",
            str_replace,"B1-2C1-0","")
TwoC1Data<-TwoC1Data %>%
  mutate_at("biopsy_1_baseline_2c1_ihc_data_from_nikon_analysis_and_scan",
            str_replace,"B1-2C1-","")
FibrosisData<-FibrosisData %>%
  mutate_at("i_participant_id",str_replace,"63-2013-00","")
FibrosisData<-FibrosisData %>%
  mutate_at("i_participant_id",str_replace,"63-2013-0","")
FibrosisData<-FibrosisData %>%
  mutate_at("i_participant_id",str_replace,"63-2013-","")
#Build dataframe of independent variables, first build one data frame of
#continuous and one of categorical, then meld the two
DataContinuousB<-matrix(nrow=109,ncol=87)
DataContinuous1<-matrix(nrow=109,ncol=45)
DataContinuous2<-matrix(nrow=109,ncol=44)
DataContinuous3<-matrix(nrow=109,ncol=44)
DataContinuousB<-as.data.frame(DataContinuousB)
DataContinuous1<-as.data.frame(DataContinuous1)
DataContinuous2<-as.data.frame(DataContinuous2)
DataContinuous3<-as.data.frame(DataContinuous3)
#Make a list of all the data that is continuous and from baseline
MyList<-list(PlasmaData[,8],PlasmaData[,12],PlasmaData[,13],
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
             PlasmaData[,330],FibrosisData[,133],FibrosisData[,176],
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

for (k in 1:85){#for each entry in MyList
  DataContinuousB[,k]<-PatientIDSort(DataContinuousB[,k],MyList[[k]])#Make column k with the patient IDs as the row numbers
}

#Split blood pressure into Systolic (86th column) and Diastolic (87th column)
for (i in 1:109){
  if (is.na(FibrosisData[i,245])){DataContinuousB[i,86:87]=NA}
  else {DataContinuousB[i,86:87]=str_split_fixed(FibrosisData[i,245],
                                                 "/", 2)}
}
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
#Make a list of all the data that is continuous and from year 1
MyList<-list(PlasmaData[,26],PlasmaData[,42],PlasmaData[,46],PlasmaData[,50],
             PlasmaData[,78],PlasmaData[,82],PlasmaData[,86],
             PlasmaData[,90],PlasmaData[,94],PlasmaData[,98],
             PlasmaData[,102],PlasmaData[,106],PlasmaData[,110],
             PlasmaData[,114],PlasmaData[,118],PlasmaData[,123],
             PlasmaData[,127],PlasmaData[,131],PlasmaData[,135],
             PlasmaData[,139],PlasmaData[,143],PlasmaData[,147],
             PlasmaData[,101],PlasmaData[,155],PlasmaData[,159],
             PlasmaData[,243],PlasmaData[,247],PlasmaData[,251],
             PlasmaData[,255],PlasmaData[,259],PlasmaData[,263],
             PlasmaData[,267],PlasmaData[,271],PlasmaData[,275],
             PlasmaData[,279],PlasmaData[,283],PlasmaData[,287],
             PlasmaData[,291],PlasmaData[,295],PlasmaData[,299],
             PlasmaData[,303],PlasmaData[,307],PlasmaData[,311],
             PlasmaData[,321],PlasmaData[,331])
for (i in 1:45){#for each element in MyList
  DataContinuous1[,i]<-PatientIDSort(DataContinuous1[,i],MyList[[i]])#build each column i with that variable and each patient id as the row
}
#name the columns
colnames(DataContinuous1)<-c("Alcohol1","Weight1","BMI1","Waist1","AST1",
                             "ALT1","ALKP1","ALB1","Tbili1","Dbili1",
                             "GGT1","PT1","INR1","Creatinine1","Glucose1",
                             "Cholesterol1","HDL1","LDL1","Triglycerides1",
                             "HbA1C1","Insulin1","Homair1","WBC1","Hemo1",
                             "Platelet1","Apri1","Fib41","FVC1","FVCp1",
                             "FEV101","FEV1p1","FEV1FVC1","FEV1FVCp1",
                             "VC1","VCp1","RV1","RVp1","TLC1","TLCp1",
                             "RVTLC1","RVTLCp1","DLCO1","DLCOp1",
                             "Polymer1","Crp1")
#Make a list of all the data that is continuous and from year 2
MyList<-list(PlasmaData[,27],PlasmaData[,43],PlasmaData[,47],
             PlasmaData[,79],PlasmaData[,83],PlasmaData[,87],
             PlasmaData[,91],PlasmaData[,95],PlasmaData[,99],
             PlasmaData[,103],PlasmaData[,107],PlasmaData[,111],
             PlasmaData[,115],PlasmaData[,119],PlasmaData[,124],
             PlasmaData[,128],PlasmaData[,132],PlasmaData[,136],
             PlasmaData[,140],PlasmaData[,144],PlasmaData[,148],
             PlasmaData[,152],PlasmaData[,156],PlasmaData[,160],
             PlasmaData[,244],PlasmaData[,248],PlasmaData[,252],
             PlasmaData[,256],PlasmaData[,260],PlasmaData[,264],
             PlasmaData[,268],PlasmaData[,272],PlasmaData[,276],
             PlasmaData[,280],PlasmaData[,284],PlasmaData[,288],
             PlasmaData[,292],PlasmaData[,296],PlasmaData[,300],
             PlasmaData[,304],PlasmaData[,308],PlasmaData[,312],
             PlasmaData[,322],PlasmaData[,332])

for (i in 1:44){#for each element in the list
  DataContinuous2[,i]<-PatientIDSort(DataContinuous2[,i],MyList[[i]])#Put each entry into the ith column with the row being the patient ID number
}

#name the columns
colnames(DataContinuous2)<-c("Alcohol2","Weight2","BMI2","AST2","ALT2",
                             "ALKP2","ALB2","Tbili2","Dbili2","GGT2",
                             "PT2","INR2","Creatinine2","Glucose2",
                             "Cholesterol2","HDL2","LDL2","Triglycerides2",
                             "HbA1c2","Insulin2","Homair2","WBC2",
                             "Hemo2","Platelet2","Apri2","Fib42","FVC2",
                             "FVCp2","FEV12","FEV1p2","FEV1FVC2",
                             "FEV1FVCp2","VC2","VCp2","RV2","RVp2",
                             "TLC2","TLCp2","RVTLC2","RVTLCp2","DLCO2",
                             "DLCOp2","Polymer2","Crp2")
#Make a list of continuous variables from year 3
MyList<-list(PlasmaData[,28],PlasmaData[,44],PlasmaData[,48],
             PlasmaData[,80],PlasmaData[,84],PlasmaData[,88],
             PlasmaData[,92],PlasmaData[,96],PlasmaData[,100],
             PlasmaData[,104],PlasmaData[,108],PlasmaData[,112],
             PlasmaData[,116],PlasmaData[,120],PlasmaData[,125],
             PlasmaData[,129],PlasmaData[,133],PlasmaData[,137],
             PlasmaData[,141],PlasmaData[,145],PlasmaData[,149],
             PlasmaData[,153],PlasmaData[,157],PlasmaData[,161],
             PlasmaData[,245],PlasmaData[,249],PlasmaData[,253],
             PlasmaData[,257],PlasmaData[,261],PlasmaData[,265],
             PlasmaData[,269],PlasmaData[,273],PlasmaData[,277],
             PlasmaData[,281],PlasmaData[,285],PlasmaData[,289],
             PlasmaData[,293],PlasmaData[,297],PlasmaData[,301],
             PlasmaData[,305],PlasmaData[,309],PlasmaData[,313],
             PlasmaData[,323],PlasmaData[,333])
for (i in 1:44){#for each element of my list
  DataContinuous3[,i]<-PatientIDSort(DataContinuous3[,i],MyList[[i]])#for the ith column put each patient's value into the row corresponding to their ID number
}
#name the columns
colnames(DataContinuous3)<-c("Alcohol3","Weight3","BMI3","AST3","ALT3",
                             "ALKP3","ALB3","Tbili3","Dbili3","GGT3",
                             "PT3","INR3","Creatinine3","Glucose3",
                             "Cholesterol3","HDL3","LDL3","Triglycerides",
                             "HbA1C3","Insulin3","Homair3","WBC3","Hemo3",
                             "Platelet3","Apri3","Fib43","FVC3","FVCp3",
                             "FEV13","FEV1p3","FEV1FVC3","FEV1FVCp3",
                             "VC3","VCp3","RV3","RVp3","TLC3","TLCp3",
                             "RVTLC3","RVTLCp3","DLCO3","DLCOp3","Polymer3",
                             "Crp3")
#Make all of the continuous data numeric
DataContinuousB<-mutate_all(DataContinuousB, function(x) as.numeric(as.character(x)))
DataContinuous1<-mutate_all(DataContinuous1, function(x) as.numeric(as.character(x)))
DataContinuous2<-mutate_all(DataContinuous2, function(x) as.numeric(as.character(x)))
DataContinuous3<-mutate_all(DataContinuous3, function(x) as.numeric(as.character(x)))




#Now for the categorical variables
DataCategoricalB<-matrix(nrow=109,ncol=22)
DataCategorical1<-matrix(nrow=109,ncol=2)
DataCategorical2<-matrix(nrow=109,ncol=2)
DataCategorical3<-matrix(nrow=109,ncol=7)
DataCategoricalB<-as.data.frame(DataCategoricalB)
DataCategorical1<-as.data.frame(DataCategorical1)
DataCategorical2<-as.data.frame(DataCategorical2)
DataCategorical3<-as.data.frame(DataCategorical3)
#Make a list of all the baseline categorical variables
MyList<-list(PlasmaData[,3],PlasmaData[,4],PlasmaData[,5],
             PlasmaData[,15],PlasmaData[,29],PlasmaData[,33],
             PlasmaData[,65],PlasmaData[,67],PlasmaData[,69],
             PlasmaData[,70],PlasmaData[,71],PlasmaData[,72],
             PlasmaData[,73],PlasmaData[,74],PlasmaData[,76],
             FibrosisData[,227],FibrosisData[,228],FibrosisData[,229],
             FibrosisData[,230],FibrosisData[,231],FibrosisData[,232],
             FibrosisData[,233])
for (i in 1:22){#for each item in the list
  DataCategoricalB[,i]<-PatientIDSort(DataCategoricalB[,i],MyList[[i]])#Put that data in the ith column, each patient in the row with their ID number
}
#name the columns
colnames(DataCategoricalB)<-c("Sex","Race","Ethnicity","Smoker","UseOx",
                              "AugTherapy","HxHypertension","MedsHypertension",
                              "MedsDiabetes","Statins","Fibrates","InsulinMed",
                              "DxDiabetes","MedsDiabetesB","MetSynd",
                              "HepBAntibody","HepBAntigen","HepC","HIV",
                              "Antinuclear","ANA","Antimito") # is this column set an independent basis? Check data matrix dimensionality
#convert multinomial variables into n-1 binary variables
DataMultinomialB<-DataCategoricalB
DataMultinomialB$PastSmoker<-ifelse(DataCategoricalB$Smoker==1,1,0)#past smoker variable
DataMultinomialB$CurrentSmoker<-ifelse(DataCategoricalB$Smoker==2,1,0)#current smoker variable
DataMultinomialB$Sex<-ifelse(DataCategoricalB$Sex==2,0,1)#change female from 2 to 0
DataMultinomialB$Ethnicity<-ifelse(DataCategoricalB$Ethnicity==2,0,1)#change Hispanic from 2 to 0
DataMultinomialB$HepBAntibodyPos<-ifelse(DataCategoricalB$HepBAntibody=="Positive",1,0)#variable for testing positive for hep B antibody
DataMultinomialB$HepBAntibodyNeg<-ifelse(DataCategoricalB$HepBAntibody=="Negative",1,0)#variable for testing negative for hep B antibody
DataMultinomialB$HepBAntigenPos<-ifelse(DataCategoricalB$HepBAntigen=="Positive",1,0)#hep B antigen positive variable
DataMultinomialB$HepBAntigenNeg<-ifelse(DataCategoricalB$HepBAntigen=="Negative",1,0)#hep B antigen negative variable
DataMultinomialB$HepCPos<-ifelse(DataCategoricalB$HepC=="Positive",1,0)#Hep C positive variable
DataMultinomialB$HepCNeg<-ifelse(DataCategoricalB$HepC=="Negative",1,0)#Hep C negative variable
DataMultinomialB$HIVpos<-ifelse(DataCategoricalB$HIV=="Positive",1,0)#HIV positive variable
DataMultinomialB$HIVneg<-ifelse(DataCategoricalB$HIV=="Negative",1,0)#HIV negative variable
DataMultinomialB$AntinuclearPos<-ifelse(DataCategoricalB$Antinuclear=="Positive",1,0)#antinuclear antibodies positive variable
DataMultinomialB$AntinuclearNeg<-ifelse(DataCategoricalB$Antinuclear=="Negative",1,0)#antinuclear antibodies negative variable
DataMultinomialB$ANAPos<-ifelse(DataCategoricalB$ANA=="Positive",1,0)#ANA positive variable
DataMultinomialB$ANANeg<-ifelse(DataCategoricalB$ANA=="Negative",1,0)#ANA negative variable
DataMultinomialB$AntimitoPos<-ifelse(DataCategoricalB$Antimito=="Positive",1,0)#antimito positive variable
DataMultinomialB$AntimitoNeg<-ifelse(DataCategoricalB$Antimito=="Negative",1,0)#antimito negative variable


#make a list of all year 1 categorical variables
MyList<-list(PlasmaData[,30],PlasmaData[,34])
for (i in 1:2){#for each entry in the list
  DataCategorical1[,i]<-PatientIDSort(DataCategorical1[,i],MyList[[i]])#put the data in the ith column with each patient going into their row
}
#name the columns
colnames(DataCategorical1)<-c("UseOx1","AugTher1")
#make a list of all year 2 categorical variables
MyList<-list(PlasmaData[,31],PlasmaData[,35])
for (i in 1:2){#for each entry in the list
  DataCategorical2[,i]<-PatientIDSort(DataCategorical2[,i],MyList[[i]])#put the data in the ith column with each patient going into the row with their ID number
}
#name the columns
colnames(DataCategorical2)<-c("UseOx2","AugTher2")
#make a list of all year 3 categorical variables
MyList<-list(PlasmaData[,32],PlasmaData[,36],PlasmaData[,55],
             PlasmaData[,56],PlasmaData[,66],PlasmaData[,68],
             PlasmaData[,75])
for (i in 1:7){#for each entry in the list
  DataCategorical3[,i]<-PatientIDSort(DataCategorical3[,i],MyList[[i]])#put the data in column i with patient ID j in the jth row
}
#name the columns
colnames(DataCategorical3)<-c("UseOx3","AugTher3","Jaundice3","Ascites3",
                              "Hypertension3","MedsHypertension3",
                              "DxDiabetes3")
#Make all entries numeric in all the categorical data frames
DataCategoricalB<-mutate_all(DataCategoricalB, function(x) as.numeric(as.character(x)))
DataCategorical1<-mutate_all(DataCategorical1, function(x) as.numeric(as.character(x)))
DataCategorical2<-mutate_all(DataCategorical2, function(x) as.numeric(as.character(x)))
DataCategorical3<-mutate_all(DataCategorical3, function(x) as.numeric(as.character(x)))


#Now to build vectors for the outcomes of interest
FibrosisBaseline<-matrix(nrow=109,ncol=1)
FibrosisBaseline<-PatientIDSort(FibrosisBaseline,PlasmaData[,166])#Puts baseline fibrosis score in row according to patient ID
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
Fibrosis3<-PatientIDSort(Fibrosis3,PlasmaData[,167])#year 3 fibrosis score put in jth row for jth patient ID
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
TwoCOneBaseline<-matrix(nrow=109,ncol=1)
TwoCOneBaseline<-PatientIDSort(TwoCOneBaseline,TwoC1Data[,5])#Avg area per 2C1 event in um^2 by patient ID
TwoCOneBaseline<-as.data.frame(as.numeric(TwoCOneBaseline))
DensityBaseline<-matrix(nrow=109,ncol=1)
DensityBaseline<-PatientIDSort(DensityBaseline,TwoC1Data[,7])#2C1 events/mm^2 by patient ID
DensityBaseline<-as.data.frame(as.numeric(DensityBaseline))
TwoCOneAreaPercent<-matrix(nrow=109,ncol=1)
TwoCOneAreaPercent<-PatientIDSort(TwoCOneAreaPercent,TwoC1Data[,6])#2C1 % area by patient ID
TwoCOneAreaPercent<-as.data.frame(as.numeric(TwoCOneAreaPercent))
#Steatosis for comparison
StBaseline<-matrix(nrow = 109,ncol=1)
StBaseline<-PatientIDSort(StBaseline,FibrosisData[,317])
Steatosis<-matrix(nrow=109,ncol=1)
for (i in 1:nrow(StBaseline)){
  Steatosis[i,1]<-str_split_i(StBaseline[i,1]," ",1)#take the first part of the entry
}
Steatosis<-as.numeric(Steatosis)
####################################################################
####################################################################
####################################################################
#Make data frames for analysis
BaselineAll<-cbind.data.frame(DataContinuousB,DataMultinomialB)
#Get rid of those variables which are entirely full of NAs, and also blood pressure which is not a number, and ethnicity because n=2
BaselineAll<-BaselineAll[!names(BaselineAll) %in% c("Race","HIV","ANA",
                                                    "Antimito","Antinuclear","HepC","HepBAntigen",
                                                    "HepBAntibody","Ethnicity",
                                                    "AgeDiag","Symptoms",
                                                    "PackYears","Waist",
                                                    "Crp","LUC",
                                                    "Insulin")]#get rid of variables Nazli doesn't want
#make data frames and dependent vectors for steatosis analysis
#Make a data frame of those with steatosis level of 0
S0<-BaselineAll[Steatosis==0,]
#Make a data frame of those with steatosis level nonzero
Spos<-BaselineAll[!Steatosis==0,]
#Take the 2C1 outcomes for those with steatosis of zero
TwoS0<-TwoCOneAreaPercent[Steatosis==0,1]
#remove nas
TwoS0na<-TwoS0[!is.na(TwoS0)]
#Take the 2C1 outcomes for those with steatosis nonzero
TwoSpos<-TwoCOneAreaPercent[!Steatosis==0,1]
#remove the nas
TwoSposna<-TwoSpos[!is.na(TwoSpos)]
#remove the rows corresponding to where the outcome is na from the predictor dataframes
S0<-S0[!is.na(TwoS0),]
Spos<-Spos[!is.na(TwoSpos),]
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
fltr = !is.na(FibrosisBaseline);
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
a1<-wilcox.test(Age~FibrosisProbabilityb,data=ADataFrame)
#Test to see whether ALT is different depending on whether or not one has fibrosis
a3<-wilcox.test(ALT~FibrosisProbabilityb,data=ADataFrame)
#test to see whether AST is different depending on whether or not one has fibrosis
a4<-wilcox.test(AST~FibrosisProbabilityb,data=ADataFrame)
#test to see whether GGT is different depending on whether or not one has fibrosis
a5<-wilcox.test(GGT~FibrosisProbabilityb,data=ADataFrame)
#test to see whether alkaline phosphatase is different depending on whether or not one has fibrosis
a6<-wilcox.test(ALKP~FibrosisProbabilityb,data=ADataFrame)
#test to see whether albumin is different depending on whether or not one has fibrosis
a7<-wilcox.test(ALB~FibrosisProbabilityb,data=ADataFrame)
#test to see whether platelet count is different depending on whether or not one has fibrosis
a8<-wilcox.test(Platelet~FibrosisProbabilityb,data=ADataFrame)
#test to see whether prothrombin time is different depending on whether or not one has fibrosis
a9<-wilcox.test(PT~FibrosisProbabilityb,data=ADataFrame)
#test to see whether PTT is different depending on whether or not one has fibrosis
a10<-wilcox.test(PTT~FibrosisProbabilityb,data=ADataFrame)
#test to see whether glucose is different depending on whether or not one has fibrosis
a11<-wilcox.test(Glucose~FibrosisProbabilityb,data=ADataFrame)
#test to see whether cholesterol is different depending on whether or not one has fibrosis
a12<-wilcox.test(Cholesterol~FibrosisProbabilityb,data=ADataFrame)
#test to see whether triglycerides are different depending on whether or not one has fibrosis
a13<-wilcox.test(Triglycerides~FibrosisProbabilityb,data=ADataFrame)
#test to see whether HDL is different depending on whether or not one has fibrosis
a14<-wilcox.test(HDL~FibrosisProbabilityb,data=ADataFrame)
#make a data frame with just the variables sex and fibrosis presence/absence
Sexdataframe<-data.frame(ADataFrame$Sex,ADataFrame$FibrosisProbabilityb)
#Make the data frame into a table
Sexdataframe<-table(ADataFrame$Sex,ADataFrame$FibrosisProbabilityb)
#perform the chi squared test to see whether there is a difference in the proportions
#of male and female patients with and without fibrosis
b1<-chisq.test(Sexdataframe)
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
b2<-chisq.test(BMItable)
#make a data frame with just those who take meds for diabetes (this variable is more
#reliable than the variable DxDiabetes in telling who has diabetes) and presence/absence fibrosis
Diabetesdata<-data.frame(ADataFrame$MedsDiabetes,ADataFrame$FibrosisProbabilityb)
#make a table so we can perform a chi squared test
DiabetesTable<-table(Diabetesdata$ADataFrame.MedsDiabetes,Diabetesdata$ADataFrame.FibrosisProbabilityb)
#test to see whether the proportion of people with diabetes differs between the group
#with fibrosis and the group without fibrosis
b3<-chisq.test(DiabetesTable)
#Make a data frame with just who has metabolic syndrome and presence/absence of fibrosis
Metdata<-data.frame(ADataFrame$MetSynd,ADataFrame$FibrosisProbabilityb)
#Transform the data frame into a table so we can perform a chi squared test
MetTable<-table(Metdata$ADataFrame.MetSynd,Metdata$ADataFrame.FibrosisProbabilityb)
#Test to see whether the proportion of who has metabolic syndrome differs between
#the group with fibrosis and the group without fibrosis
b4<-chisq.test(MetTable)
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
b5<-chisq.test(BDataFrame$NoFib, p = BDataFrame$Fib, rescale.p = TRUE)



#computing chi squared test for hypertension
#Start by making a new column with 1 for having hypertension 
#and 0 for not having hypertension
#make a data frame with hypertension only and presence/absence of fibrosis
DataHy<-cbind(ADataFrame$HxHypertension,ADataFrame$FibrosisProbabilityb)
#make an index marking those rows that either are 1 for history of hypertension or
#have a systolic bp greater than or equal to 130
indexHy<-which(ADataFrame$HxHypertension==1|ADataFrame$Systolic>=130)
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
b6<-chisq.test(TableHy)
#make a vector of all saved p-values so that we can perform benjamini-hochberg adjustment
pvalues<-c(a1$p.value,a3$p.value,a4$p.value,a5$p.value,a6$p.value,
           a7$p.value,a8$p.value,a9$p.value,a10$p.value,a11$p.value,a12$p.value,
           a13$p.value,a14$p.value,b1$p.value,b2$p.value,b3$p.value,
           b4$p.value,b5$p.value,b6$p.value)
#Adjust the p-values using the BH method to get new pvalues
newpvalues<-p.adjust(pvalues,method = "hochberg",n=length(pvalues))

#Main analysis
#run multinomial function for baseline fibrosis score

# By running:

# ttt <- DataCategoricalB
# ttt <- ttt[, -(16:22)]
# ttt <- na.omit(ttt)
# sum(is.na(ttt))
# qr(as.matrix(ttt))
# dim(ttt)
# tttqr = qr(as.matrix(ttt))
# tttqr.Q
# qr.Q(tttqr)
# qr.R(tttqr)

# we find that the dataframe has a rank equal to the number of columns minus 1. If there's an obvious dependency then this data may very well be aBLE TO GENERATE AN INDEPENDENT BASIS SET, WHICH WOULD MEAN THE DATA ARE NUMERICALLY UNRELATED TO ONE ANOTHER. 

# because there is partial overlap between the holdout and permutation importance measures, I'm not sure if there is a contextual grouping that would increase the number of intersecting factors contributing to the data as measured by permutation and holdout importance. 

#MyMult: 4 for p2 and 9 for p2HO
# MyCts: 7 for p2 and 13 for p2HO
# MyBin: 9 for p2 and 15 for p2HO
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
#run multinomial function with year 3 fibrosis score
Results.F3<-MyMult(BaselineAll2,Fibrosis3)
Results.F3[[4]]
ggsave("SobolMDAF3.jpg",width = 10,height = 5,units = "in",dpi=300)
#run multinomial function with change in fibrosis score
Results.CH<-MyMult(BaselineAll2,ChangeinFibrosis)
Results.CH[[4]]
ggsave("SobolMDACh.jpg",width = 10,height = 5,units = "in",dpi=300)
#run continuous outcome function with first 2C1 outcome
Results.2C1A<-MyCts(BaselineAll3,TwoCOneBaseline)
Results.2C1A[[4]]
ggsave("Coefficients2C1A.jpg",width = 10,height = 5,units = "in",dpi=300)
Results.2C1A[[7]]
ggsave("SobolMDA2C1A.jpg",width = 10,height = 5,units = "in",dpi=300)
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


#run continuous outcome function without steatosis
Results.S0<-MyCts(S0,TwoS0na)
ggsave("CoefficientsS0.jpg",width = 10,height = 5,units = "in",dpi=300)

ggsave("SobolMDAS0.jpg",width = 10,height = 5,units = "in",dpi=300)
#and with steatosis
Results.Spos<-MyCts(Spos,TwoSposna)
ggsave("CoefficientsSpos.jpg",width = 10,height = 5,units = "in",dpi=300)

ggsave("SobolMDASpos.jpg",width = 10,height = 5,units = "in",dpi=300)

#For making figure with fibrosis scores by sex
FibrosisBaseline<-as.data.frame(FibrosisBaseline);
ADataFrame<-cbind(BaselineAll[fltr,],FibrosisBaseline)#combine predictors with outcome
ADataFrame<-ADataFrame[!is.na(FibrosisBaseline),,drop=FALSE]#get rid of those rows with nas in outcome
ADataFrame<-ADataFrame %>%
  rename(
    "SexNumeric"="Sex"#rename the variable known as "Sex" to have the new name "SexNumeric"
  )
n<-ncol(ADataFrame)
m<-n+1
ADataFrame<-cbind(ADataFrame,matrix(nrow = 
                                      nrow(ADataFrame),ncol=1))#add a new column to the data frame
ADataFrame<-as.data.frame(ADataFrame)
colnames(ADataFrame)[m]<-"Sex"#name the new column "Sex"
for (i in 1:nrow(ADataFrame)){#for each row in the data frame
  if (ADataFrame$SexNumeric[i]==0){ADataFrame$Sex[i]="Female"}#if the numeric value for sex is 0, call sex female
  else if (ADataFrame$SexNumeric[i]==1){ADataFrame$Sex[i]="Male"}#if the numeric value for sex is 1, call sex male
}
# ADataFrame<-ADataFrame %>%
#   rename(
#     "FibrosisBaseline"="as.numeric(FibrosisBaseline)"#rename variable so it's easier to refernece
#   )
ggplot(data=ADataFrame,aes(x=FibrosisBaseline,group=Sex))+#plot baseline fibrosis score
  #as a bar chart with separate bars by sex
  geom_bar(aes(x=FibrosisBaseline,color=Sex,fill=Sex),
           position="dodge")+#don't put the bars on top of each other
  labs(x="Baseline Fibrosis Score",y="Count")+#label the axes
  theme(text=element_text(size=rel(3)),legend.text=element_text(size=rel(3)))#make the text large
ggsave("SexFibScore.jpg",width = 10,height = 5,units = "in",dpi=300)#save the figure in publication level dpi


#Fibrosis probability and sex
#do the same thing for fibrosis probability and sex
#make a data frame with predictors and fibrosis probability at baseline outcome
BDataFrame<-cbind(BaselineAll1,FibrosisProbabilityb)
#rename sex to sex numeric so we can make a new variable named sex
BDataFrame<-BDataFrame %>%
  rename(
    "SexNumeric"="Sex"
  )
#add a column to the data frame
n<-ncol(BDataFrame)
m<-n+1
BDataFrame<-cbind(BDataFrame,matrix(nrow = nrow(BDataFrame),ncol = 1))
BDataFrame<-as.data.frame(BDataFrame)
#Name the new column sex
#colnames(BDataFrame)[m]<-"SexNumeric"
#Fill in the entries in the new column with female if the numeric value for sex is zero
#and male if the numeric value for sex is 1
for (i in 1:nrow(BDataFrame)){
  if (BDataFrame$SexNumeric[i]==0){BDataFrame$Sex[i]="Female"}
  else if (BDataFrame$SexNumeric[i]==1){BDataFrame$Sex[i]="Male"}
}
#Plot Fibrosis presence/absence by sex
ggplot(data=BDataFrame,aes(x=FibrosisProbabilityb,group=Sex))+
  geom_bar(aes(x=FibrosisProbabilityb,color=Sex,fill=Sex),
           position="dodge")+
  labs(x="Baseline Fibrosis Presence",y="Count")+
  #note: the line below doesn't work for some reason
  #  scale_x_discrete(breaks=c("0","1"),labels=c("No Fibrosis","Fibrosis"))+
  theme(text=element_text(size=rel(3)),legend.text = element_text(size=rel(3)))
#Save plot at publication level dots per inch
ggsave("SexFibProb.jpg",width = 10,height = 5,units = "in",dpi = 300)


#calculate matthews correlation coefficient
RFTP<-Results.FPboot[[12]][[1]][2,2] #true positives for random forest
RFTN<-Results.FPboot[[12]][[1]][1,1] #true negatives for random forest
RFFP<-Results.FPboot[[12]][[1]][2,1]#False positives for random forest
RFFN<-Results.FPboot[[12]][[1]][1,2]#False negatives for random forest
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


#Code for making plots of steatosis vs 2C1 average area per event
#format steatosis as a data frame so we can combine the variables into one data frame for plotting
Steatosis<-as.data.frame(Steatosis)
#combine variables into a data frame for plotting
CDataFrame<-cbind(Steatosis,TwoCOneBaseline)
#get rid of those rows that don't have steatosis values
CDataFrame<-CDataFrame[!is.na(Steatosis),]
#rename column so it will show up with nice name
colnames(CDataFrame)[2]<-"TwoCOneBaseline"
#get rid of those rows that don't have 2C1 values
CDataFrame<-CDataFrame[!is.na(CDataFrame$TwoCOneBaseline),]
#make a plot of steatosis vs 2C1 with points and violin plots for each steatosis score
ggplot(CDataFrame,mapping=aes(y=CDataFrame$TwoCOneBaseline,
                              x=as.factor(CDataFrame$Steatosis)))+
  geom_point()+
  geom_violin(alpha=0.05)+
  labs(x="Steatosis Score",y="2C1 avg. area per event")+
  theme_bw()+
  theme(text=element_text(size=rel(3)),legend.text = element_text(size=rel(3)))
#save plot at publication level dpi
ggsave("SteatosisOne.jpg",width = 10,height = 5,units = "in",dpi = 300)

#perform a kolmogorov-smirnov test to determine whether there is a difference in
#2C1 outcome for those with vs those without steatosis
ks.test(CDataFrame$TwoCOneBaseline[CDataFrame$Steatosis==0],
        CDataFrame$TwoCOneBaseline[CDataFrame$Steatosis==1])
#Add a column to the data frame
CDataFrame<-cbind(CDataFrame,matrix(nrow = nrow(CDataFrame),ncol = 1))
#name the column steatosis binary
colnames(CDataFrame)[3]<-"StBin"
#make the entries zero if steatosis is 0 and 1 otherwise
CDataFrame$StBin[CDataFrame$Steatosis==0]<-0
CDataFrame$StBin[CDataFrame$Steatosis!=0]<-1
#Make a plot comparing 2C1 outcome for those with and without steatosis
ggplot(CDataFrame,aes(y=CDataFrame$TwoCOneBaseline,
                      x=as.factor(CDataFrame$StBin)))+
  geom_point()+
  geom_violin(alpha=0.05)+
  scale_x_discrete(breaks=c("0","1"),labels=c("No","Yes"))+
  labs(x="Steatosis",y="2C1 avg. area per event")+
  theme_bw()+
  theme(text=element_text(size=rel(3)),legend.text = element_text(size=rel(3)))
#save plot at publication level dpi
ggsave("SteatosisTwo.jpg",width = 10,height = 5,units = "in",dpi = 300)


ks.test(CDataFrame$TwoCOneBaseline[CDataFrame$StBin==0],
        CDataFrame$TwoCOneBaseline[CDataFrame$StBin==1])

#Q:What happens if we just take the average value from the training set and use it to predict the testing set?
set.seed(seed1)#set seed for reproducibility
FibrosisProbabilityb<-FibrosisProbabilityb[!is.na(FibrosisProbabilityb)]#Have already done this to matrix
FibrosisProbabilityb<-as.data.frame(FibrosisProbabilityb)
Data.matrix<-makeX(BaselineAll1,na.impute=TRUE) #imputes NAs and returns a matrix
#Split into train and test sets
Data.matrix<-as.data.frame(Data.matrix)#reformat into dataframe
train<-sample(1:nrow(Data.matrix),nrow(Data.matrix)*0.7)#70/30 split
TrainData<-as.data.frame(Data.matrix[train,])#Take train data and make a dataframe
TestData<-as.data.frame(Data.matrix[-train,])#Make a dataframe of test data
FibrosisProbabilityb<-as.data.frame(FibrosisProbabilityb)
myFibrosisProbabilityb<-FibrosisProbabilityb[train,1]#Split output into train part
DepTest<-FibrosisProbabilityb[-train,1]#and test part of output
DepTest<-as.data.frame(DepTest)
colnames(DepTest)<-"DepTest"
my.predictor<-mean(myFibrosisProbabilityb)#use the mean as the predictor
predicted<-matrix(nrow = nrow(DepTest),ncol = 2)#make a data frame for the predictor
predicted<-as.data.frame(predicted)
predicted[1:nrow(DepTest),2]<-my.predictor#fill the data frame with the desired value
colnames(predicted)<-c("truth","Class1")#name the columns to work with pr_auc function requirements
predicted$truth<-as.factor(unlist(DepTest))#fill in the other column values
AUPRC<-yardstick::pr_auc(predicted,truth="truth","Class1")#A:calculate area under the curve
#Q:What is the RMSE of the constant model? for change in fibrosis score
set.seed(seed1)#set seed for reproducibility
ChangeinFibrosis<-ChangeinFibrosis[!is.na(ChangeinFibrosis)]#Have already done this to matrix
ChangeinFibrosis<-as.data.frame(ChangeinFibrosis)
Data.matrix<-makeX(BaselineAll2,na.impute=TRUE) #imputes NAs and returns a matrix
#Split into train and test sets
Data.matrix<-as.data.frame(Data.matrix)#reformat into dataframe
train<-sample(1:nrow(Data.matrix),nrow(Data.matrix)*0.7)#70/30 split
TrainData<-as.data.frame(Data.matrix[train,])#Take train data and make a dataframe
TestData<-as.data.frame(Data.matrix[-train,])#Make a dataframe of test data
ChangeinFibrosis<-as.data.frame(ChangeinFibrosis)
myChangeinFibrosis<-ChangeinFibrosis[train,1]#Split output into train part
DepTest<-ChangeinFibrosis[-train,1]#and test part of output
DepTest<-as.data.frame(DepTest)
colnames(DepTest)<-"DepTest"
my.predictor<-mean(myChangeinFibrosis)#use mean value as my predictor
RMSE<-sqrt((1/nrow(DepTest))*sum((DepTest-my.predictor)^2))#A: calculate root mean squared error

#Q: what are the outcome values for the cases of hep c pos and antimito pos?
#index those cases which have hep C pos and antimito pos
my.indices<-which(BaselineAll3$HepCPos==1 | BaselineAll3$AntimitoPos==1)
#Make variables of 2C1 without NAs so I can use the indices to look up those patient's 2C1 values
Var1<-TwoCOneBaseline[!is.na(TwoCOneBaseline)]
#Look up outcomes for first outcome variable for those patients
Outputs<-Var1[my.indices]
Var2<-DensityBaseline[!is.na(DensityBaseline)]
Var2[my.indices]
Var3<-TwoCOneAreaPercent[!is.na(TwoCOneAreaPercent)]
Var3[my.indices]

#Build a data frame with the three outcomes and the predictors so that we can plot
#predictors and outcomes
DDataFrame<-cbind(BaselineAll3,Var1,Var2,Var3)
#Make a dummy variable that is zero except at my indices
DDataFrame$Var4<-0
DDataFrame$Var4[my.indices]<-1
#Plot hep c status vs 2C1 outcome with coloration by my indices
ggplot(DDataFrame,aes(x=HepCPos,y=Var1,group = Var4))+
  geom_point(aes(color=Var4))+
  geom_violin(aes(group=HepCPos,alpha=0.05))+
  geom_hline(aes(yintercept=mean(Var1)))
#plot the patient ids vs 2C1 outcome with a horizontal line at the mean of the 2C1 outcome
ggplot(DDataFrame,aes(x=rownames(DDataFrame),y=Var1))+
  
  geom_hline(yintercept=mean(Var1))


#Q: What is the RMSE of the constant model? for 2C1 average area per event

set.seed(seed1)#set seed for reproducibility
TwoCOneBaseline<-TwoCOneBaseline[!is.na(TwoCOneBaseline)]#Have already done this to matrix
TwoCOneBaseline<-as.data.frame(TwoCOneBaseline)
Data.matrix<-makeX(BaselineAll3,na.impute=TRUE) #imputes NAs and returns a matrix
#Split into train and test sets
Data.matrix<-as.data.frame(Data.matrix)#reformat into dataframe
train<-sample(1:nrow(Data.matrix),nrow(Data.matrix)*0.7)#70/30 split
TrainData<-as.data.frame(Data.matrix[train,])#Take train data and make a dataframe
TestData<-as.data.frame(Data.matrix[-train,])#Make a dataframe of test data
TwoCOneBaseline<-as.data.frame(TwoCOneBaseline)
myTwoCOneBaseline<-TwoCOneBaseline[train,1]#Split output into train part
DepTest<-TwoCOneBaseline[-train,1]#and test part of output
DepTest<-as.data.frame(DepTest)
colnames(DepTest)<-"DepTest"
my.predictor<-mean(myTwoCOneBaseline)#use the mean of the outcome as my predictor
RMSE<-sqrt((1/nrow(DepTest))*sum((DepTest-my.predictor)^2))#A: calculate root mean squared error

#Fit model using only HepCPos and AntimitoPos
set.seed(seed1)#set seed for reproducibility
TwoCOneBaseline<-TwoCOneBaseline[!is.na(TwoCOneBaseline)]#Have already done this to matrix
TwoCOneBaseline<-as.data.frame(TwoCOneBaseline)
Data.matrix<-makeX(BaselineAll3,na.impute=TRUE) #imputes NAs and returns a matrix
#Split into train and test sets
Data.matrix<-as.data.frame(Data.matrix)#reformat into dataframe
train<-sample(1:nrow(Data.matrix),nrow(Data.matrix)*0.7)#70/30 split
TrainData<-as.data.frame(Data.matrix[train,])#Take train data and make a dataframe
TestData<-as.data.frame(Data.matrix[-train,])#Make a dataframe of test data
TwoCOneBaseline<-as.data.frame(TwoCOneBaseline)
myTwoCOneBaseline<-TwoCOneBaseline[train,1]#Split output into train part
DepTest<-TwoCOneBaseline[-train,1]#and test part of output
DepTest<-as.data.frame(DepTest)
colnames(DepTest)<-"DepTest"
#build a linear model of two C one with just those two variables on train data only
my.model<-lm(myTwoCOneBaseline~HepCPos+AntimitoPos,data=TrainData) 
#make a data frame consisting of the residuals and the fitted values from the linear model
res<-my.model[["residuals"]]
fit<-my.model[["fitted.values"]]
df<-cbind(res,fit)
df<-as.data.frame(df)
#plot the residuals vs the fitted values
ggplot(data=df,aes(x=fit,y=res))+
  geom_point(size=6)+
  geom_hline(yintercept=0)+
  labs(x="Fitted Values",y="Residuals")


#waiting to hear back from Cory about what plots to make

#What is the RMSE of the constant model? for 2C1 events per mm^2

set.seed(seed1)#set seed for reproducibility
DensityBaseline<-DensityBaseline[!is.na(DensityBaseline)]#Have already done this to matrix
DensityBaseline<-as.data.frame(DensityBaseline)
Data.matrix<-makeX(BaselineAll3,na.impute=TRUE) #imputes NAs and returns a matrix


#Split into train and test sets
Data.matrix<-as.data.frame(Data.matrix)#reformat into dataframe
set.seed(seed4)
train<-sample(1:nrow(Data.matrix),nrow(Data.matrix)*0.7)#70/30 split
TrainData<-as.data.frame(Data.matrix[train,])#Take train data and make a dataframe
TestData<-as.data.frame(Data.matrix[-train,])#Make a dataframe of test data
DensityBaseline<-as.data.frame(DensityBaseline)
myDensityBaseline<-DensityBaseline[train,1]#Split output into train part
DepTest<-DensityBaseline[-train,1]#and test part of output
DepTest<-as.data.frame(DepTest)
colnames(DepTest)<-"DepTest"
my.predictor<-mean(myDensityBaseline)
#Calculate the root mean squared error we're looking for
RMSE<-sqrt((1/nrow(DepTest))*sum((DepTest-my.predictor)^2))

#Does augmentation therapy status affect circulating polymer levels?

#Select out the polymer data for those who are on aug therapy
onaug<-BaselineAll$Polymer[BaselineAll$AugTherapy==1]
#Select out the polymer data for those who are not on aug therapy
offaug<-BaselineAll$Polymer[BaselineAll$AugTherapy==0]
#calculate Kolmogorov-Smirnov test to compare two distributions
mystat<-ks.test(onaug,offaug)
#Sample with replacement from on aug group to get data for PDF
onpdf<-onaug[sample(length(onaug),size=10000,replace = TRUE)]
#Sample with replacement from off aug group to get data for PDF
offpdf<-offaug[sample(length(offaug),size = 10000,replace = TRUE)]
#Need to specify bins manually so that there are exactly 100, otherwise get different number of bins
binwidth1<-(max(onaug,na.rm=TRUE)-min(onaug,na.rm=TRUE))/100
binwidth2<-(max(offaug,na.rm=TRUE)-min(offaug,na.rm=TRUE))/100
nbins1<-seq(min(onaug,na.rm = TRUE)-binwidth1,
            max(onaug,na.rm = TRUE)+binwidth1,
            by=binwidth1)
nbins2<-seq(min(offaug,na.rm = TRUE)-binwidth2,
            max(offaug,na.rm = TRUE)+binwidth2,
            by=binwidth2)
#Build histograms to get x and y coordinates for ggplot, using plug-in method allows for exactly 100 bins
hist1<-hist(onpdf,breaks=nbins1,main = "Plug-in method")
hist2<-hist(offpdf,breaks = nbins2,main = "Plug-in method")
#Take data from histograms and make it into a data frame, naming y-variables so that labels will be appropriate in the plot
mydataframe<-data.frame(
  x1=rep(hist1$breaks,each=2),
  On=c(0,rep(hist1$counts,each=2),0),
  x2=rep(hist2$breaks,each=2),
  Off=c(0,rep(hist2$counts,each=2),0)
)
mydataframe$On<-mydataframe$On/sum(mydataframe$On,na.rm = TRUE)
mydataframe$Off<-mydataframe$Off/sum(mydataframe$Off,na.rm = TRUE)
#make the plot with on aug group in blue and off aug group in orange
ggplot(mydataframe,aes(y=On,x=x1))+
  geom_line(aes(col="On"),size=1)+
  geom_line(aes(x=x2,y=Off,col="Off"),size=1)+
  theme_bw()+
  labs(x="Plasma Polymer",y="Relative Density")+
  scale_color_manual(name = "Augmentation Status", values = c("On" = "blue", "Off" = "orange"))
#Save the plot in publication-ready dots per inch
ggsave("AugDist.jpg",width = 10,height = 5,units = "in",dpi = 300)
