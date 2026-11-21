seed1<-42#set values that will be assigned to seeds in following functions
seed2<-42
seed4<-42

# ++++++++++++++++
# PatientIDSort
# ++++++++++++++++
# DataFrame1: a data frame I'm sorting the data into
# DataFrame2: the data frame made from the excel sheet
PatientIDSort<-function(DataFrame1,DataFrame2){
  for (i in 1:109){
    for (j in 1:101){
      #FIXME: this should be one of the input dataframes
      if (PlasmaData[j,1]==i){DataFrame1[i]=DataFrame2[j]}  
      #Puts data into the row number corresponding to the ID number, 
      #i.e. patient 1 in row 1, etc.
    }}
  return(DataFrame1)}


extract.rankings <- function(object) {
  losttheplot = ggplot_build(object);
  xlabels = losttheplot$layout$panel_params[[1]]$x$get_labels()
  xlabels
}

#Average predictions from both holdoutRF forests for test-set evaluation.
predict.holdoutRF <- function(object, data = NULL, ...) {
  pred1 <- predict(object$rf1, data, ...)
  pred2 <- predict(object$rf2, data, ...)
  if (is.factor(pred1$predictions)) {
    levels <- levels(pred1$predictions)
    avg <- round((as.integer(pred1$predictions) + as.integer(pred2$predictions)) / 2)
    pred1$predictions <- factor(levels[avg], levels = levels)
  } else if (is.matrix(pred1$predictions)) {
    pred1$predictions <- (pred1$predictions + pred2$predictions) / 2
  } else {
    avg <- (pred1$predictions + pred2$predictions) / 2
    if (identical(object$treetype, "Classification")) {
      avg <- round(avg)
    }
    pred1$predictions <- avg
  }
  pred1
}

#Load functions

# #############
# MyCts
# a function to take a continuous outcome and model it using random forests
# and elastic net regression
# #############
MyCts<-function(IndepMatrix,DepVector){
  start.time<-Sys.time()#save start time to check how long it takes to run
  set.seed(seed1)#set seed for reproducibility
  DepVector<-DepVector[!is.na(DepVector)]#Have already done this to matrix
  DepVector<-as.data.frame(DepVector)
  Data.matrix<-makeX(IndepMatrix,na.impute=TRUE) #imputes NAs and returns a matrix
  
  #Split into train and test sets
  Data.matrix<-as.data.frame(Data.matrix)#reformat into dataframe
  set.seed(seed4)
  train<-sample(1:nrow(Data.matrix),nrow(Data.matrix)*0.7)#70/30 split indices
  TrainData<-as.data.frame(Data.matrix[train,])#Take train data and make a dataframe
  TestData<-as.data.frame(Data.matrix[-train,])#Make a dataframe of test data
  DepVector<-as.data.frame(DepVector)
  mydepvector<-DepVector[train,1]#Split output into train part
  DepTest<-DepVector[-train,1]#and test part of output
  DepTest<-as.data.frame(DepTest)
  colnames(DepTest)<-"DepTest"
  
  
  #5-fold cross-validation
  set.seed(seed4)
  cvindex=sample(rep(seq(5),length=nrow(TrainData)))#Make an index for the 5-fold
  #cross-validation
  CVTrain<-cbind(TrainData,cvindex)#attach index to matrix of predictors
  CVDep<-cbind(mydepvector,cvindex)#attach index to dependent variable
  CVDep<-as.data.frame(CVDep)
  DataFrame<-list()#initialize outputs
  Vector<-list()#initialize outputs
  my.seq<-seq(0,1,0.1)#sequence of alphas to grid search
  my.seq2<-exp(seq(-5,5,0.5))#sequence of lambdas to grid search
  my.seq3<-seq(50,1000,50)#sequence of ntrees to grid search
  my.seq4<-seq(10,30,2)#sequence of tree depths to grid search
  RMSE<-matrix(nrow = length(my.seq),ncol=length(my.seq2))#initialize outputs
  SD<-matrix(nrow = length(my.seq),ncol=length(my.seq2))
  my.alpha<-matrix(nrow = length(my.seq),ncol=length(my.seq2))
  my.lambda<-matrix(nrow = length(my.seq),ncol=length(my.seq2))
  my.length<-length(my.seq3)#total length needed for other outputs
  RMSERF<-matrix(nrow=my.length,ncol=length(my.seq4))#intitialize outputs
  SDRF<-matrix(nrow=my.length,ncol=length(my.seq4))
  RMSERFHO<-matrix(nrow=my.length,ncol=length(my.seq4))#intitialize outputs
  SDRFHO<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.n<-matrix(nrow = my.length,ncol = length(my.seq4))
  my.depth<-matrix(nrow = my.length,ncol = length(my.seq4))
  my.object<-list()
  forests<-list()
  for (j in 1:length(my.seq)){#for each value of alpha in the grid
    for (n in 1:length(my.seq2)){#for each value of lambda in the grid
      myvector<-matrix(nrow=5,ncol=1)
      for (i in 1:5){#for each holdout set
        DataFrame[[i]]<-CVTrain[CVTrain$cvindex!=i,]#sort the predictors into train
        Vector[[i]]<-CVDep[CVDep$cvindex!=i,1]#sort the output into train
        TestData1<-CVTrain[CVTrain$cvindex==i,]#sort the predictors into test
        TestData1<-as.matrix(TestData1)
        DepTest1<-CVDep[CVDep$cvindex==i,1]#sort the output into test
        DepTest1<-as.data.frame(DepTest1)
        set.seed(seed2)
        foldid=sample(rep(seq(10),length=nrow(DataFrame[[i]])))#hold foldid constant
        #by setting the seed at the same number before doing this each time
        k<-i+(j-1)*(n-1)*5#count total number of loops to index
        my.object[[k]]<-glmnet(DataFrame[[i]],Vector[[i]],
                               foldid=foldid,alpha=my.seq[j],family = "gaussian",
                               lambda=my.seq2[n])#Builds a glmnet object
        #using the train predictor, train output, foldid held steady for all
        #values of alpha and lambda on the grid, picking each alpha and lambda
        
        my.model<-my.object[[k]]#separates out the model object
        my.alpha[j,n]<-my.seq[j]#saves the alpha for after the loop, indexed by k,
        #which has the same index as the variable where we store the variable we
        #are optimizing over
        my.lambda[j,n]<-my.seq2[n]#saves the lambda for after the loop
        TestData1<-as.matrix(TestData1)
        Predicted<-predict(my.model,TestData1,s="lambda.min",type="response")#Predict
        #model output using test predictors
        Predicted<-as.data.frame(Predicted)
        myvector[i]<-sqrt(1/nrow(DepTest1)*sum((Predicted-DepTest1)^2)) 
        #evaluate prediction using desired statistic
        
      }
      RMSE[j,n]=mean(myvector)#average over the holdout sets
      SD[j,n]=sd(myvector)#take standard deviation over the holdout sets
    }}
  for (j in 1:length(my.seq3)){#for each possible number of trees
    for (n in 1:length(my.seq4)){#for each possible tree depth
      myvector2<-matrix(nrow=5,ncol=1)#initialize temporary variable
      myvector2HO<-matrix(nrow=5,ncol=1)#initialize temporary variable
      for (i in 1:5){#for each of the holdout sets
        DataFrame[[i]]<-CVTrain[CVTrain$cvindex!=i,]#sort the predictors into train
        Vector[[i]]<-CVDep[CVDep$cvindex!=i,1]#sort the output into train
        TestData1<-CVTrain[CVTrain$cvindex==i,]#sort the predictors into test
        TestData1<-as.matrix(TestData1)
        DepTest1<-CVDep[CVDep$cvindex==i,1]#sort the output into test
        DepTest1<-as.data.frame(DepTest1)
        
        rf<-ranger(Vector[[i]]~.,data=DataFrame[[i]],#build a random forest with
                   #train predictor, train output
                   num.trees = my.seq3[j],#given number of trees
                   replace = TRUE,#sample with replacement
                   importance="permutation",#Sobol MDA importance measure
                   min.node.size = my.seq4[n],#given min node size
                   keep.inbag = TRUE)#save so that we can use later
        
        RFTrainData<-DataFrame[[i]]
        RFTrainData$DepVar<-Vector[[i]]
        rfHO<-holdoutRF(DepVar~.,data=RFTrainData,num.trees = my.seq3[j],
                        #build a random forest using all train data, with chosen
                        min.node.size = my.seq4[n],
                        keep.inbag = TRUE)
        predicted<-predict(rf,TestData1)#predict using the random forest and test]
        #predictors
        predicted<-as.data.frame(predicted)
        myvector2[i]<-sqrt(1/nrow(DepTest1)*sum((predicted-DepTest1)^2)) #calculate metric of fit
        predictedHO<-as.data.frame(predict(rfHO,TestData1))
        myvector2HO[i]<-sqrt(1/nrow(DepTest1)*sum((predictedHO-DepTest1)^2)) #calculate metric of fit
        
      }
      my.n[j,n]<-my.seq3[j]#save number of trees with same index as measure of fit
      RMSERF[j,n]=mean(myvector2)#average over the holdout sets
      SDRF[j,n]=sd(myvector2)
      my.depth[j,n]<-my.seq4[n]
      RMSERFHO[j,n]=mean(myvector2HO)#average over the holdout sets
      SDRFHO[j,n]=sd(myvector2HO)
    }
  }
  my.alpha<-c(my.alpha)#transform matrices into vectors
  my.lambda<-c(my.lambda)
  RMSE<-c(RMSE)
  SD<-c(SD)
  
  RMSE<-as.data.frame(unlist(RMSE))
  colnames(RMSE)<-"value"
  dfRMSE<-cbind(RMSE,SD,my.alpha,my.lambda)
  
  RMSE<-rownames_to_column(RMSE)
  RMSE1<- RMSE %>%
    arrange(RMSE)#make sure to arrange in ascending order so can minimize
  my.indices<-RMSE1$rowname[1:5]
  
  
  my.best.alpha<-my.alpha[as.numeric(my.indices)]#determine which alpha has best metric
  my.best.lambda<-my.lambda[as.numeric(my.indices)]#determine which lambda has best metric
  
  RMSE1<-matrix(nrow = length(my.best.alpha),ncol = 1)#initialize output vector
  my.model<-list()
  for (i in 1:length(my.best.alpha)){#for each top candidate alpha-lambda pair
    TrainData<-as.matrix(TrainData)
    my.model[[i]]<-glmnet(TrainData,mydepvector,#build final model using
                          #all train data, with full training output
                          alpha=my.best.alpha[i],family = "gaussian",#using selected alpha
                          lambda = my.best.lambda[i])#and selected lambda, make a gaussian
    #model as our outcome is continuous
    my.coefs<-coef(my.model[[1]],s="lambda.min")#extract the coefficients from the model
    TestData<-as.matrix(TestData)
    Predicted<-predict(my.model[[i]],TestData,"lambda.min")#predict outcome using test
    #predictors
    Predicted<-as.data.frame(Predicted)
    RMSE1[i]<-sqrt(1/nrow(DepTest)*sum((Predicted-DepTest)^2)) #measure with the same metric
  }
  
  DepTest<-as.matrix(DepTest)
  Metrics<-assess.glmnet(object=my.model[[1]],newx=TestData,
                         newy = DepTest,family="gaussian",s="lambda.min")
  #measure outcomes with metrics from the glmnet package
  
  my.coefs<-as.data.frame(as.matrix(my.coefs))
  
  my.coefs<-my.coefs %>%#arranges the coefficients in descending absolute value
    arrange(desc(abs(my.coefs$`s=lambda.min`)))
  my.coefs<-my.coefs[!rownames(my.coefs) %in% "(Intercept)",,drop=FALSE]#gets rid of intercept
  my.coefs<-my.coefs[1:15,,drop=FALSE]#selects top 15 most important coefficients
  my.coefs<-my.coefs %>%#Gets rid of all coefficients that are zero
    filter(!`s=lambda.min`==0)
  my.coefs<-as.data.frame(my.coefs)
  my.coefs<-my.coefs %>%#arranges the coefficients in descinding absolute value
    arrange(desc(abs(my.coefs$`s=lambda.min`)))#have to do this again because they get rearanged by intermediate steps
  my.coefs<-rownames_to_column(my.coefs)#make a column and put the rownames there
  #note: rownames are the variable names
  my.coefs$rowname<-factor(my.coefs$rowname,levels = unique(my.coefs$rowname))
  #set rownames as factor so that they won't automatically graph in alphabetical order
  
  p1<-ggplot(my.coefs,aes(x=rowname,y=`s=lambda.min`))+#plot the remaining coefficients in
    #order of descending magnitude with variable name as x-asis label
    geom_point(colour="#3c78d8",size=6)+#stick to theme colors and make dot big
    labs(x="Variable",y="Coefficient")+#label the axes
    theme_bw()+#get rid of grey
    theme(axis.text.x = element_text(angle=90),#turn the variable names vertical
          axis.title.x=element_blank(),#get rid of x axis title
          text=element_text(face="bold",color="black",size=rel(3)),#make font bold, large
          axis.text = element_text(color = "black"))#specify text color for axes
  
  
  #This ends the post-loop portion to do with elastic net regression and begins 
  #the portion to do with random forests
  RMSERF<-c(RMSERF)#make matrices into vectors
  my.n<-c(my.n)
  SDRF<-c(SDRF)
  my.depth<-c(my.depth)
  RMSERF<-as.data.frame(RMSERF)
  dfRMSERF<-cbind(RMSERF,my.n,my.depth)
  
  RMSERF<-rownames_to_column(RMSERF)
  RMSERF<- RMSERF %>%
    arrange(RMSERF)#arrange in ascending order to minimize
  my.indices<-RMSERF$rowname[1:5]
  
  RMSERFHO<-c(RMSERFHO)#make matrices into vectors
  SDRFHO<-c(SDRFHO)
  RMSERFHO<-as.data.frame(RMSERFHO)
  dfRMSERFHO<-cbind(RMSERFHO,my.n,my.depth)
  
  RMSERFHO<-rownames_to_column(RMSERFHO)
  RMSERFHO<- RMSERFHO %>%
    arrange(RMSERFHO)#arrange in ascending order to minimize
  my.indicesHO<-RMSERFHO$rowname[1:5]  
  
  my.number<-my.n[as.numeric(my.indices)]#determine which number of trees optimizes metric
  my.optimal.depth<-my.depth[as.numeric(my.indices)]#determine which tree depth optimizes metric
  RMSEtest<-matrix(nrow = length(my.number),ncol = 1)#initialize outputs
  
  my.numberHO<-my.n[as.numeric(my.indicesHO)]#determine which number of trees optimizes metric
  my.optimal.depthHO<-my.depth[as.numeric(my.indicesHO)]#determine which tree depth optimizes metric
  RMSEHOtest<-matrix(nrow = length(my.numberHO),ncol = 1)#initialize outputs
  rf.regression<-list()
  rf.regressionHO<-list()
  for (i in 1:length(my.numberHO)){#for each optimal number of trees with tree depth combo
    TrainData<-as.data.frame(TrainData)
    rf.regression[[i]]<-ranger(mydepvector~.,data=TrainData,num.trees = my.number[i],
                               #build a random forest using all train data, with chosen
                               replace = TRUE,#number of trees, keeping other parameters
                               importance="permutation",#the same as in the loop
                               min.node.size = my.optimal.depth[i],
                               keep.inbag = TRUE)
    TrainRF<-TrainData
    TrainRF$DepVar<-mydepvector
    rf.regressionHO[[i]]<-holdoutRF(DepVar~.,data=TrainRF,num.trees = my.numberHO[i],
                                    #build a random forest using all train data, with chosen
                                    min.node.size = my.optimal.depthHO[i],
                                    keep.inbag = TRUE)
    
    predicted<-predict(rf.regression[[i]],TestData)#predict using this random forest
    predictedHO<-predict(rf.regressionHO[[i]],TestData)#predict using this random forest
    #and test predictors
    predicted<-as.data.frame(predicted)
    DepTest<-as.data.frame(DepTest)
    RMSEtest[i]<-sqrt((1/nrow(predicted))*sum((DepTest-predicted)^2))#evaluate using the same metric
    predictedHO<-as.data.frame(predictedHO)
    RMSEHOtest[i]<-sqrt((1/nrow(predictedHO))*sum((DepTest-predictedHO)^2))#evaluate using the same metric
  }
  
  ImportanceMeasure<-rf.regression[[1]][["variable.importance"]]#extract variable importance scores
  ImportanceMeasure<-as.data.frame(ImportanceMeasure)
  ImportanceMeasure<-ImportanceMeasure %>%#arrange variable importance scores in
    arrange(desc(as.numeric(ImportanceMeasure)))#descending value (all are positive, so no abs needed)
  ImportanceMeasure<-rownames_to_column(ImportanceMeasure)#make variable names a column
  ImportanceMeasure<-ImportanceMeasure[1:15,]#only keep the top 15 most important factors
  ImportanceMeasure<-as.data.frame(ImportanceMeasure)
  ImportanceMeasure$rowname<-factor(ImportanceMeasure$rowname,
                                    levels=unique(ImportanceMeasure$rowname))
  ImportanceMeasureHO<-rf.regressionHO[[1]][["variable.importance"]]#extract variable importance scores
  ImportanceMeasureHO<-as.data.frame(ImportanceMeasureHO)
  ImportanceMeasureHO<-ImportanceMeasureHO %>%#arrange variable importance scores in
    arrange(desc(as.numeric(ImportanceMeasureHO)))#descending value (all are positive, so no abs needed)
  ImportanceMeasureHO<-rownames_to_column(ImportanceMeasureHO)#make variable names a column
  ImportanceMeasureHO<-ImportanceMeasureHO[1:15,]#only keep the top 15 most important factors
  ImportanceMeasureHO<-as.data.frame(ImportanceMeasureHO)
  ImportanceMeasureHO$rowname<-factor(ImportanceMeasureHO$rowname,
                                      levels=unique(ImportanceMeasureHO$rowname))
  #make variable names a factor so they won't automatically be alphabetized when plotted
  p2<-ggplot(data = ImportanceMeasure,mapping = aes(x=rowname,#plot importance measures
                                                    y=ImportanceMeasure))+
    geom_point(size=6,fill="#de7608",color="#de7608")+#use color pallet
    expand_limits(y=0)+#force y-axis to include zero rather than starting at nonzero value
    labs(x="Variable",y="permutation")+#label axes
    theme_bw()+#get rid of grey background
    theme(axis.text.x = element_text(angle=90),#rotate variable names to vertical
          axis.title.x=element_blank(),#get rid of x-axis title
          text=element_text(face="bold",color="black",size=rel(3)),#make text black and big
          axis.text = element_text(color = "black"))#make text black
  p2HO<-ggplot(data = ImportanceMeasureHO,mapping = aes(x=rowname,#plot importance measures
                                                        y=ImportanceMeasureHO))+
    geom_point(size=6,fill="#de7608",color="#de7608")+#use color pallet
    expand_limits(y=0)+#force y-axis to include zero rather than starting at nonzero value
    labs(x="Variable",y="ranger Holdout")+#label axes
    theme_bw()+#get rid of grey background
    theme(axis.text.x = element_text(angle=90),#rotate variable names to vertical
          axis.title.x=element_blank(),#get rid of x-axis title
          text=element_text(face="bold",color="black",size=rel(3)),#make text black and big
          axis.text = element_text(color = "black"))#make text black
  
  p3<-ggplot(data=dfRMSE,aes(x=my.alpha,y=log(my.lambda),fill=value))+
    geom_tile()+
    scale_fill_viridis(discrete = FALSE,name="RMSE")+
    labs(x="Alpha",y="log(Lambda)")+
    theme_bw()
  
  
  p4<-ggplot(dfRMSERF,aes(x=my.n,y=my.depth,fill=RMSERF))+
    geom_tile()+
    scale_fill_viridis(discrete = FALSE,name="RMSE")+
    labs(x="Number of Trees",y="Min. Node Size")+
    theme_bw()
  
  
  
  
  end.time<-Sys.time()#notes the end time
  time.taken<-round(end.time-start.time,2)#calculates the time taken to 2 decimals
  MyList<-list(my.model[[1]],RMSE1,Metrics,p1,rf.regression[[1]],RMSEtest,p2,time.taken,
               p3,p4,my.best.alpha,my.best.lambda,p2HO)
  #things I want to return from this function
  return(MyList)
}



# #############
# MyMult
# a function that takes a multinomial outcome and models it using a random forest
# #############
MyMult<-function(IndepMatrix,DepVector){
  start.time<-Sys.time()#save start time to check how long it takes to run
  set.seed(seed1)#set seed for reproducibility
  DepVector<-DepVector[!is.na(DepVector)]#Have already done this to matrix
  DepVector<-as.data.frame(DepVector)
  Data.matrix<-makeX(IndepMatrix,na.impute=TRUE) #imputes NAs and returns a matrix
  
  #Split into train and test sets
  Data.matrix<-as.data.frame(Data.matrix)#reformat into dataframe
  train<-sample(1:nrow(Data.matrix),nrow(Data.matrix)*0.7)#70/30 split
  TrainData<-as.data.frame(Data.matrix[train,])#Take train data and make a dataframe
  TestData<-as.data.frame(Data.matrix[-train,])#Make a dataframe of test data
  DepVector<-as.data.frame(DepVector)
  mydepvector<-DepVector[train,1]#Split output into train part
  DepTest<-DepVector[-train,1]#and test part of output
  DepTest<-as.data.frame(DepTest)
  colnames(DepTest)<-"DepTest"
  
  #5-fold cross-validation
  set.seed(seed4)
  cvindex=sample(rep(seq(5),length=length(mydepvector)))#make an index for 5-fold cross validation
  CVTrain<-cbind(TrainData,cvindex)#attach index to data frame of predictors
  CVDep<-cbind(mydepvector,cvindex)#attach index to vector of outputs
  CVDep<-as.data.frame(CVDep)
  DataFrame<-list()#initialize outputs
  Vector<-list()
  my.seq3<-seq(50,1000,50)#sequence of numbers of trees to try
  my.seq4<-seq(10,30,2)#sequence of tree depths to grid search
  my.length<-length(my.seq3)#total number of iterations in the loop
  RMSERF<-matrix(nrow=my.length,ncol=length(my.seq4))#initialize outputs
  SDRF<-matrix(nrow=my.length,ncol=length(my.seq4))
  RMSERFHO<-matrix(nrow=my.length,ncol=length(my.seq4))#initialize outputs
  SDRFHO<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.n<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.depth<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.object<-list()
  forests<-list()
  
  for (j in 1:length(my.seq3)){#for each possible number of trees
    for (n in 1:length(my.seq4)){#for each possible tree depth
      myvector2<-matrix(nrow = 5,ncol = 1)#temporary object to store metric in before averaging
      myvector2HO<-matrix(nrow = 5,ncol = 1)#temporary object to store metric in before averaging
      for (i in 1:5){#for each of the holdout sets
        
        DataFrame[[i]]<-CVTrain[CVTrain$cvindex!=i,]#Make training data frame of predictors
        Vector[[i]]<-CVDep[CVDep$cvindex!=i,1]#make training vector of outcome
        TestData1<-CVTrain[CVTrain$cvindex==i,]#make test data frame of predictors
        TestData1<-as.matrix(TestData1)
        DepTest1<-CVDep[CVDep$cvindex==i,1]#make test data frame of outcome
        DepTest1<-as.data.frame(DepTest1)
        rf<-ranger(Vector[[i]]~.,data=DataFrame[[i]],#builds a random forest using
                   #training predictors and outcome
                   num.trees = my.seq3[j],#loop through number of trees options
                   replace = TRUE,#keep other settings the same as in other functions MyBin and MyCts
                   importance="permutation",
                   min.node.size = my.seq4[n],
                   keep.inbag = TRUE)
        predicted<-predict(rf,TestData1)#predict using the random forest we just built
        #and the test predictors
        #predicted<-as.data.frame(predicted)
        myvector2[i]<-sqrt((1/length(predicted[[1]]))*sum((DepTest1-predicted[[1]])^2))#measure outcome using chosen metric
        
        RFTrainData<-DataFrame[[i]]
        RFTrainData$DepVar<-Vector[[i]]
        rfHO<-holdoutRF(DepVar~.,data=RFTrainData,num.trees = my.seq3[j],
                        #build a random forest using all train data, with chosen
                        min.node.size = my.seq4[n],
                        keep.inbag = TRUE)    
        predictedHO<-predict.holdoutRF(rfHO,TestData1)#predict using the random forest we just built
        #and the test predictors
        #predictedHO<-as.data.frame(predictedHO)
        myvector2HO[i]<-sqrt((1/length(predictedHO[[1]]))*sum((DepTest1-predictedHO[[1]])^2))#measure outcome using chosen metric
        
      }
      RMSERF[j,n]<-mean(myvector2)
      SDRF[j,n]<-sd(myvector2)
      my.n[j,n]<-my.seq3[j]#save number of trees with same index as metric
      my.depth[j,n]<-my.seq4[n]#save tree depth with same index too
      RMSERFHO[j,n]<-mean(myvector2HO)
      SDRFHO[j,n]<-sd(myvector2HO)
    }
  }
  
  RMSERF<-c(RMSERF)#transform each output matrix into a vector
  SDRF<-c(SDRF)
  my.n<-c(my.n)
  my.depth<-c(my.depth)
  RMSERF<-as.data.frame(RMSERF)
  dfRMSERF<-cbind(RMSERF,my.n,my.depth)
  RMSERF<-rownames_to_column(RMSERF)
  RMSERF<- RMSERF %>%
    arrange(RMSERF)#arrange in ascending order to minimize
  my.indices<-RMSERF$rowname[1:5]
  
  my.number<-my.n[as.numeric(my.indices)]#determine which number of trees optimizes metric
  my.optimal.depth<-my.depth[as.numeric(my.indices)]#determine which tree depth optimizes metric
  RMSEtest<-matrix(nrow = length(my.number),ncol = 1)
  
  RMSERFHO<-c(RMSERFHO)#transform each output matrix into a vector
  SDRFHO<-c(SDRFHO)
  RMSERFHO<-as.data.frame(RMSERFHO)
  dfRMSERF<-cbind(RMSERFHO,my.n,my.depth)
  RMSERFHO<-rownames_to_column(RMSERFHO)
  RMSERFHO<- RMSERFHO %>%
    arrange(RMSERFHO)#arrange in ascending order to minimize
  my.indicesHO<-RMSERFHO$rowname[1:5]
  
  my.numberHO<-my.n[as.numeric(my.indicesHO)]#determine which number of trees optimizes metric
  my.optimal.depthHO<-my.depth[as.numeric(my.indicesHO)]#determine which tree depth optimizes metric
  RMSEtestHO<-matrix(nrow = length(my.numberHO),ncol = 1) 
  
  for (i in 1:length(my.number)){#for each optimal number of trees with tree depth combo
    TrainData<-as.data.frame(TrainData)
    rf<-ranger(mydepvector~.,data=TrainData,num.trees = my.number[i],#build random
               #forest with all training data, keeping other parameters the same
               replace = TRUE,
               importance="permutation",
               min.node.size = my.optimal.depth[i],
               keep.inbag = TRUE)
    predicted<-predict(rf,TestData)#predict using random forest and test predictors
    predicted<-as.data.frame(predicted)
    DepTest<-as.data.frame(DepTest)
    RMSEtest[i]<-sqrt((1/length(predicted[[1]]))*sum((DepTest-predicted[[1]])^2))#evaluate using same metric
    
    TrainRF<-TrainData
    TrainRF$DepVar<-mydepvector
    rfHO<-holdoutRF(DepVar~.,data=TrainRF,num.trees = my.number[i],
                    #build a random forest using all train data, with chosen
                    min.node.size = my.optimal.depth[i],
                    keep.inbag = TRUE)  
    
    predictedHO<-predict(rfHO,TestData)#predict using random forest and test predictors
    predictedHO<-as.data.frame(predictedHO)
    RMSEtestHO[i]<-sqrt((1/length(predictedHO[[1]]))*sum((DepTest-predictedHO[[1]])^2))#evaluate using same metric
    
  }
  
  ImportanceMeasure<-rf[["variable.importance"]]#extract importance measures from the random forest
  ImportanceMeasure<-as.data.frame(ImportanceMeasure)
  ImportanceMeasure<-ImportanceMeasure %>%#sort the variables by descending importance measure
    arrange(desc(as.numeric(ImportanceMeasure)))
  ImportanceMeasure<-rownames_to_column(ImportanceMeasure)#make a column of variable names
  ImportanceMeasure<-ImportanceMeasure[1:15,]#keep only 15 most important variables
  ImportanceMeasure<-as.data.frame(ImportanceMeasure)
  ImportanceMeasure$rowname<-factor(ImportanceMeasure$rowname,
                                    levels=unique(ImportanceMeasure$rowname))
  
  ImportanceMeasureHO<-rfHO[["variable.importance"]]#extract importance measures from the random forest
  ImportanceMeasureHO<-as.data.frame(ImportanceMeasureHO)
  ImportanceMeasureHO<-ImportanceMeasureHO %>%#sort the variables by descending importance measure
    arrange(desc(as.numeric(ImportanceMeasureHO)))
  ImportanceMeasureHO<-rownames_to_column(ImportanceMeasureHO)#make a column of variable names
  ImportanceMeasureHO<-ImportanceMeasureHO[1:15,]#keep only 15 most important variables
  ImportanceMeasureHO<-as.data.frame(ImportanceMeasureHO)
  ImportanceMeasureHO$rowname<-factor(ImportanceMeasureHO$rowname,
                                      levels=unique(ImportanceMeasureHO$rowname))
  
  #make variable names a factor so that they won't automatically alphabetize when we plot them
  p2<-ggplot(data = ImportanceMeasure,mapping = aes(x=rowname,
                                                    y=ImportanceMeasure))+
    #plot variables by decreasing importance measure
    geom_point(size=6,fill="#de7608",color="#de7608")+#use the theme colors
    expand_limits(y=0)+#force y-axis to start at 0 rather than a positive number
    labs(x="Variable",y="permutation")+#label axes
    theme_bw()+#get rid of grey background
    theme(axis.text.x = element_text(angle=90),#rotate variable names vertical
          axis.title.x=element_blank(),#get rid of x-axis title
          text=element_text(face="bold",color="black",size=rel(3)),#make text black and large
          axis.text = element_text(color = "black"))#make text black
  p2HO<-ggplot(data = ImportanceMeasureHO,mapping = aes(x=rowname,
                                                        y=ImportanceMeasureHO))+
    #plot variables by decreasing importance measure
    geom_point(size=6,fill="#de7608",color="#de7608")+#use the theme colors
    expand_limits(y=0)+#force y-axis to start at 0 rather than a positive number
    labs(x="Variable",y="ranger Holdout")+#label axes
    theme_bw()+#get rid of grey background
    theme(axis.text.x = element_text(angle=90),#rotate variable names vertical
          axis.title.x=element_blank(),#get rid of x-axis title
          text=element_text(face="bold",color="black",size=rel(3)),#make text black and large
          axis.text = element_text(color = "black"))#make text black
  p3<-ggplot(dfRMSERF,aes(x=my.n,y=my.depth,fill=RMSERF))+
    geom_tile()+
    scale_fill_viridis(discrete = FALSE,name="RMSE")+
    labs(x="Number of Trees",y="Min. Node Size")+
    theme_bw()
  
  end.time<-Sys.time()#notes the end time
  time.taken<-round(end.time-start.time,2)#calculates the time taken to 2 decimals
  MyList<-list(time.taken,rf,RMSEtest,p2,p3,my.number,my.optimal.depth,RMSEtestHO,p2HO)
  #make and return the relavant list of items
  return(MyList)
}

#IndepMatrix<-DFBoot#these are here for debugging mode
#DepVector<-FPBoot


# #############
# MyBin
# a function that takes a binary outcome and models it using a random forest
# #############
MyBin<-function(IndepMatrix,DepVector){#This function takes as inputs
  #a matrix of predictors and a vector of outputs
  start.time<-Sys.time()#save start time to check how long it takes to run
  set.seed(seed1)#set seed for reproducibility
  DepVector<-DepVector[!is.na(DepVector)]#Have already done this to matrix
  DepVector<-as.data.frame(DepVector)
  Data.matrix<-makeX(IndepMatrix,na.impute=TRUE) #imputes NAs and returns a matrix
  
  
  #Split into train and test sets
  Data.matrix<-as.data.frame(Data.matrix)#reformat into dataframe
  set.seed(seed4)
  train<-sample(1:nrow(Data.matrix),nrow(Data.matrix)*0.7)#70/30 split
  TrainData<-as.data.frame(Data.matrix[train,])#Take train data and make a dataframe
  TestData<-as.data.frame(Data.matrix[-train,])#Make a dataframe of test data
  DepVector<-as.data.frame(DepVector)
  mydepvector<-DepVector[train,1]#Split output into train part
  DepTest<-DepVector[-train,1]#and test part of output
  DepTest<-as.data.frame(DepTest)
  colnames(DepTest)<-"DepTest"
  
  #5-fold cross-validation
  set.seed(seed4)
  cvindex=sample(rep(seq(5),length=nrow(TrainData)))#make index for 5-fold cross validation
  CVTrain<-cbind(TrainData,cvindex)#attach index to matrix of predictors
  CVDep<-cbind(mydepvector,cvindex)#attach index to vector of outcomes
  CVDep<-as.data.frame(CVDep)
  DataFrame<-list()#initialize outputs
  Vector<-list()
  my.seq<-seq(0,1,0.2)#make a sequence of alphas to try
  my.seq2<-exp(seq(-5,5,0.5))#make a sequence of lambdas to try
  my.seq3<-seq(50,1000,50)#make a sequence of numbers of trees to try
  my.seq4<-seq(10,30,2)#make a sequence of tree depths to grid search
  SD<-matrix(nrow=length(my.seq),ncol=length(my.seq2))
  myPR<-matrix(nrow=length(my.seq),ncol=length(my.seq2))
  my.alpha<-matrix(nrow=length(my.seq),ncol=length(my.seq2))
  my.lambda<-matrix(nrow=length(my.seq),ncol=length(my.seq2))
  my.length<-length(my.seq3)#calculate how many rows other outputs need
  rfPR<-matrix(nrow=my.length,ncol=length(my.seq4))#initialize outputs
  SDRF<-matrix(nrow=my.length,ncol=length(my.seq4))
  rfPRHO<-matrix(nrow=my.length,ncol=length(my.seq4))#initialize outputs
  SDRFHO<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.n<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.depth<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.object<-list()
  forests<-list()
  for (j in 1:length(my.seq)){#for each value of alpha to try
    for (n in 1:length(my.seq2)){#for each value of lambda to try
      myvector<-list()#temporary variable to hold metrics before averaging
      for (i in 1:5){#for each holdout set
        DataFrame[[i]]<-CVTrain[CVTrain$cvindex!=i,]#use index to select appropriate subset of predictors
        Vector[[i]]<-CVDep[CVDep$cvindex!=i,1]#use index to select appropriate subset of outcomes
        TestData1<-CVTrain[CVTrain$cvindex==i,]#use index to select appropriate subset of predictors
        TestData1<-as.matrix(TestData1)
        DepTest1<-CVDep[CVDep$cvindex==i,1]#use index to select appropriate subset of outcomes
        DepTest1<-as.factor(DepTest1)
        set.seed(seed2)#make sure the foldid is the same every time (modulo the number of rows in the data frame)
        foldid=sample(rep(seq(10),length=nrow(DataFrame[[i]])))#make a foldid to feed into the function call
        my.model<-glmnet(DataFrame[[i]],factor(Vector[[i]]),#build an elastic
                         #net regression model with the given cross-validation set
                         #make sure outcome is a factor so that it will perform classification
                         foldid=foldid,alpha=my.seq[j],family = "binomial",
                         #given foldid, given alpha and lambda, specify family as binomial
                         lambda=my.seq2[n])
        
        TestData1<-as.matrix(TestData1)
        Predicted<-predict(my.model,TestData1,s="lambda.min",#predict outcome using
                           #the model, test predictors, use the lambda which minimizes MSE
                           family="binomial", type = "response")
        #keep family as binomial, and get the output rather than the class
        Predicted<-as.numeric(Predicted)
        ADataFramePR<-cbind(DepTest1,Predicted)#make a data frame to feed into pr_auc
        colnames(ADataFramePR)<-c("truth","Class1")#give it the correct column names
        ADataFramePR <- as.data.frame(ADataFramePR)
        ADataFramePR$truth <- as.factor(ADataFramePR$truth)
        
        myvector[[i]]<-yardstick::pr_auc(ADataFramePR,truth = "truth","Class1")#calculate area under PR curve
        #and index the metric by the same index
      }
      myPRvector<-matrix(nrow=length(myvector),ncol=1)#calculate area under PR curve
      for (i in 1:length(myvector)){
        if (length(myvector[[i]][[".estimate"]])>0){#if there is something in that entry
          myPRvector[i]=myvector[[i]][[".estimate"]]#separate out the values for the area under the PR curve from the labels
        }  }
      myPR[j,n]<-mean(myPRvector)
      SD[j,n]<-sd(myPRvector)
      my.alpha[j,n]<-my.seq[j]#save the alpha with the same index as the metric
      my.lambda[j,n]<-my.seq2[n]#save the lambda with the same index as the metric
      
    }
    
  }
  
  for (j in 1:length(my.seq3)){#for each option for number of trees
    for (n in 1:length(my.seq4)){#for each possible tree depth
      myvector2<-list()#make temporary variable to hold metrics before averaging
      myvector2HO<-list()#make temporary variable to hold metrics before averaging
      for (i in 1:5){#for each holdout set
        DataFrame[[i]]<-CVTrain[CVTrain$cvindex!=i,]#use index to select appropriate subset of predictors
        Vector[[i]]<-CVDep[CVDep$cvindex!=i,1]#use index to select appropriate subset of outcomes
        TestData1<-CVTrain[CVTrain$cvindex==i,]#use index to select appropriate subset of predictors
        TestData1<-as.matrix(TestData1)
        DepTest1<-CVDep[CVDep$cvindex==i,1]#use index to select appropriate subset of outcomes
        DepTest1<-as.factor(DepTest1)
        rf<-ranger(Vector[[i]]~.,data=DataFrame[[i]],#make a random forest with
                   #the cross-validation predictors and outcomes
                   num.trees = my.seq3[j],#iterate through number of trees
                   replace = TRUE,#keep other parameters the same as in
                   importance="permutation",#other models for other outcome variables
                   min.node.size = my.seq4[n],#given tree depth
                   keep.inbag = TRUE,
                   classification = TRUE)#except for classification, as this is the only binary outcome
        predicted<-predict(rf,TestData1)#predict using the random forest and test predictors
        predicted<-as.data.frame(predicted)
        BDataFramePR<-cbind(DepTest1,predicted)#make a data frame to calculate the area under the PR curve
        colnames(BDataFramePR)<-c("truth","Class1")#give it the necessary column names
        myvector2[[i]]<-yardstick::pr_auc(BDataFramePR,truth="truth","Class1")#calculate the area
        #under the pr curve and save the metric indexed by which loop we are on
        
        
        RFTrainData<-DataFrame[[i]]
        RFTrainData$DepVar<-Vector[[i]]
        rfHO<-holdoutRF(DepVar~.,data=RFTrainData,num.trees = my.seq3[j],
                        #build a random forest using all train data, with chosen
                        min.node.size = my.seq4[n],
                        keep.inbag = TRUE,
                        classification = TRUE) #except for classification, as this is the only binary outcome
        
        predictedHO<-predict.holdoutRF(rfHO,TestData1)#predict using the random forest and test predictors
        predictedHO<-as.data.frame(predictedHO)
        BDataFramePRHO<-cbind(DepTest1,predictedHO)#make a data frame to calculate the area under the PR curve
        colnames(BDataFramePRHO)<-c("truth","Class1")#give it the necessary column names
        myvector2HO[[i]]<-yardstick::pr_auc(BDataFramePRHO,truth="truth","Class1")#calculate the area
        #under the pr curve and save the metric indexed by which loop we are on
        
      }
      myPRvector<-matrix(nrow=length(myvector2),ncol=1)#calculate area under PR curve
      for (i in 1:length(myvector2)){
        if (length(myvector2[[i]][[".estimate"]])>0){#if there is something in that entry
          myPRvector[i]=myvector2[[i]][[".estimate"]]#separate out the values for the area under the PR curve from the labels
        }  }
      rfPR[j,n]<-mean(myPRvector)#save the metric with an index
      SDRF[j,n]<-sd(myPRvector)#save the standard deviation with the same index
      my.n[j,n]<-my.seq3[j]#save the number of trees tried indexed same as the metric
      my.depth[j,n]<-my.seq4[n]#save tree depth with same index
      
      myPRvectorHO<-matrix(nrow=length(myvector2HO),ncol=1)#calculate area under PR curve
      for (i in 1:length(myvector2HO)){
        if (length(myvector2HO[[i]][[".estimate"]])>0){#if there is something in that entry
          myPRvectorHO[i]=myvector2HO[[i]][[".estimate"]]#separate out the values for the area under the PR curve from the labels
        }  }
      rfPRHO[j,n]<-mean(myPRvectorHO)#save the metric with an index
      SDRFHO[j,n]<-sd(myPRvectorHO)#save the standard deviation with the same index
    }}
  
  
  myPR<-c(myPR)
  SD<-c(SD)
  my.alpha<-c(my.alpha)
  my.lambda<-c(my.lambda)
  df1<-cbind(myPR,SD,my.alpha,my.lambda)
  df1<-as.data.frame(df1)
  
  myPR<-as.data.frame(myPR)
  myPR<-rownames_to_column(myPR)
  myPR<- myPR %>%
    arrange(desc(myPR))#make sure to arrange in descending order so can maximize
  my.indices<-myPR$rowname[1:5]
  
  
  my.best.alpha<-my.alpha[as.numeric(my.indices)]#determine which alpha has optimal metric
  my.best.lambda<-my.lambda[as.numeric(my.indices)]#determine which lambda has optimal metric
  
  ConfMat<-list()
  ConfMatHO<-list()
  my.model<-list()
  for (i in 1:length(my.best.alpha)){#for each top candidate alpha-lambda pair
    TrainData<-as.matrix(TrainData)
    mydepvector<-factor(mydepvector)#make sure to input a factor so that glmnet will make a binomial model
    my.model[[i]]<-glmnet(TrainData,mydepvector,#make an elastic net regression model
                          #using all the training data
                          alpha=my.best.alpha[i],family = "binomial",
                          #using the optimal alpha and lambda, and keeping in mind our outcome is binary
                          lambda = my.best.lambda[i])
    my.coefs<-coef(my.model[[1]],s="lambda.min")#get the coefficients for our model
    
    TestData<-as.matrix(TestData)
    Predicted<-predict(my.model[[i]],TestData,#predict outcomes for our model using test predictors
                       s="lambda.min",family="binomial",#using the lambda that minimizes the MSE
                       type = "class")#predict the outcome as a class rather than a probability
    Predicted<-as.data.frame(Predicted)
    Predicted<-as.numeric(Predicted$`s=lambda.min`)
    DepTest<-as.numeric(unlist(DepTest))
    #calculate the confusion matrix for the test data and save to be reported out
    ConfMat[[i]]<-confusionMatrix(factor(Predicted,levels = c(0,1)),
                                  factor(DepTest,levels = c(0,1)))$table
  }
  
  DepTest<-as.matrix(DepTest)
  Metrics<-assess.glmnet(object=my.model[[1]],newx=TestData,#calculate the metrics to assess
                         #given in the glmnet package
                         newy = DepTest,family="binomial",s="lambda.min")
  
  my.coefs<-as.matrix(my.coefs)
  my.coefs<-as.data.frame(my.coefs)
  
  my.coefs<-my.coefs %>%#arranges the coefficients in descending absolute value
    arrange(desc(abs(my.coefs$`s=lambda.min`)))
  my.coefs<-my.coefs[!rownames(my.coefs) %in% "(Intercept)",,drop=FALSE]
  #get rid of the intercept so that we only have the coefficients for the variables
  my.coefs<-my.coefs[1:15,,drop=FALSE]#keep only the top 15 most influential
  my.coefs<-my.coefs %>%#Gets rid of all coefficients that are zero
    filter(!`s=lambda.min`==0)
  my.coefs<-as.data.frame(my.coefs)
  my.coefs<-my.coefs %>%#do this again because an intermediate step disorganizes them
    arrange(desc(abs(my.coefs$`s=lambda.min`)))
  my.coefs<-rownames_to_column(my.coefs)#make a column with variable names
  my.coefs$rowname<-factor(my.coefs$rowname,levels = unique(my.coefs$rowname))
  #make variable names a factor so that ggplot2 won't automatically alphabetize them
  p1<-ggplot(my.coefs,aes(x=rowname,y=`s=lambda.min`))+#plot the coefficients in decreasing
    #absolute value with variable name on x-axis
    geom_point(colour="#3c78d8",size=6)+#keep to our color pallet
    labs(x="Variable",y="Coefficient")+#make axis labels
    theme_bw()+#get rid of grey
    theme(axis.text.x = element_text(angle=90),#rotate variable names to vertical
          axis.title.x=element_blank(),#get rid of x-axis title
          text=element_text(face="bold",color="black",size=rel(3)),#make text black and large
          axis.text = element_text(color = "black"))#make text black
  
  #Now we have finished the post-loop part for the elastic net regression
  #Next we will do the post-loop part for the random forest
  rfPR<-c(rfPR)#transform output from matrix to vector
  SDRF<-c(SDRF)
  my.n<-c(my.n)
  my.depth<-c(my.depth)
  rfPR<-as.data.frame(rfPR)
  df2<-cbind(rfPR,SDRF,my.n,my.depth)
  rfPR<-rownames_to_column(rfPR)
  rfPR<-rfPR %>%
    arrange(desc(rfPR))#make sure to arrange in descending order so can maximize
  my.indices<-rfPR$rowname[1:5]
  
  my.number<-my.n[as.numeric(my.indices)]#determine which number of trees optimizes our metric
  my.optimal.depth<-my.depth[as.numeric(my.indices)]#determine which tree depth optimizes our metric
  
  rfPRHO<-c(rfPRHO)#transform output from matrix to vector
  SDRFHO<-c(SDRFHO)
  rfPRHO<-as.data.frame(rfPRHO)
  df2HO<-cbind(rfPRHO,SDRFHO,my.n,my.depth)
  rfPRHO<-rownames_to_column(rfPRHO)
  rfPRHO<-rfPRHO %>%
    arrange(desc(rfPRHO))#make sure to arrange in descending order so can maximize
  my.indicesHO<-rfPRHO$rowname[1:5]
  
  ConfMat2<-list()
  ConfMat2HO<-list()
  for (i in 1:length(my.number)){#for each combination of parameters chosen
    TrainData<-as.data.frame(TrainData)
    rf<-ranger(mydepvector~.,data=TrainData,num.trees = my.number[i],
               #build a random forest using all the training data with optimal 
               replace = TRUE,#number of trees
               importance="permutation",#keeping other parameters the same
               min.node.size = my.optimal.depth[i],#as for other outcome variables
               keep.inbag = TRUE,
               classification=TRUE)#except for this, which is different because this outcome is binary
    predicted<-predict(rf,TestData)#predict outcomes using test predictors
    predicted<-as.data.frame(predicted)
    DepTest<-as.data.frame(DepTest)
    Pred<-predicted#get prediction vector from random forest
    Pred<-unlist(Pred)#make into a factor
    DepTestFactor<-unlist(DepTest)#get the ground truth vector
    ConfMat2[[i]]<-confusionMatrix(Pred,#Calculate the confusion matrix for the random forest
                                   factor(DepTestFactor,levels = c(0,1)))$table
    
    TrainRF<-TrainData
    TrainRF$DepVar<-mydepvector
    rfHO<-holdoutRF(DepVar~.,data=TrainRF,num.trees = my.number[i],
                    #build a random forest using all train data, with chosen
                    min.node.size = my.optimal.depth[i],
                    keep.inbag = TRUE,
                    classification = TRUE)#except for this, which is different because this outcome is binary
    
    predictedHO<-predict(rfHO,TestData)#predict outcomes using test predictors
    predictedHO<-as.data.frame(predictedHO)
    PredHO<-predictedHO#get prediction vector from random forest
    PredHO<-unlist(PredHO)#make into a factor
    DepTestFactorHO<-unlist(DepTest)#get the ground truth vector
    ConfMat2HO[[i]]<-confusionMatrix(PredHO,#Calculate the confusion matrix for the random forest
                                     factor(DepTestFactorHO,levels = c(0,1)))$table
  }
  
  
  ImportanceMeasure<-rf[["variable.importance"]]#extract importance measures
  ImportanceMeasure<-as.data.frame(ImportanceMeasure)
  ImportanceMeasure<-ImportanceMeasure %>%#arranges variables by descending importance measure
    arrange(desc(as.numeric(ImportanceMeasure)))
  ImportanceMeasure<-ImportanceMeasure %>%
    filter(ImportanceMeasure>0)#eliminates negative values of importance measure
  ImportanceMeasure<-rownames_to_column(ImportanceMeasure)#converts variable names to a column
  ImportanceMeasure<-ImportanceMeasure[1:15,]#keeps only 15 most important variables
  ImportanceMeasure<-as.data.frame(ImportanceMeasure)
  ImportanceMeasure$rowname<-factor(ImportanceMeasure$rowname,
                                    levels=unique(ImportanceMeasure$rowname))
  
  ImportanceMeasureHO<-rfHO[["variable.importance"]]#extract importance measures
  ImportanceMeasureHO<-as.data.frame(ImportanceMeasureHO)
  ImportanceMeasureHO<-ImportanceMeasureHO %>%#arranges variables by descending importance measure
    arrange(desc(as.numeric(ImportanceMeasureHO)))
  ImportanceMeasureHO<-ImportanceMeasureHO %>%
    filter(ImportanceMeasureHO>0)#eliminates negative values of importance measure
  ImportanceMeasureHO<-rownames_to_column(ImportanceMeasureHO)#converts variable names to a column
  ImportanceMeasureHO<-ImportanceMeasureHO[1:15,]#keeps only 15 most important variables
  ImportanceMeasureHO<-as.data.frame(ImportanceMeasureHO)
  ImportanceMeasureHO$rowname<-factor(ImportanceMeasureHO$rowname,
                                      levels=unique(ImportanceMeasureHO$rowname))
  
  #makes variable names a factor so that they won't be automatically alphabetized
  p2<-ggplot(data = ImportanceMeasure,mapping = aes(x=rowname,
                                                    y=ImportanceMeasure))+
    #plots variable names by decreasing importance measure
    geom_point(size=6,fill="#de7608",color="#de7608")+#use theme colors and make dots large
    expand_limits(y=0)+#force y-axis to start at 0
    labs(x="Variable",y="permutation")+#label axes
    theme_bw()+#get rid of grey background
    theme(axis.text.x = element_text(angle=90),#rotate variable names to vertical
          axis.title.x=element_blank(),#get rid of x-axis label
          text=element_text(face="bold",color="black",size=rel(3)),#make text black and large
          axis.text = element_text(color = "black"))#make text black
  
  #makes variable names a factor so that they won't be automatically alphabetized
  p2HO<-ggplot(data = ImportanceMeasureHO,mapping = aes(x=rowname,
                                                        y=ImportanceMeasureHO))+
    #plots variable names by decreasing importance measure
    geom_point(size=6,fill="#de7608",color="#de7608")+#use theme colors and make dots large
    expand_limits(y=0)+#force y-axis to start at 0
    labs(x="Variable",y="ranger Holdout")+#label axes
    theme_bw()+#get rid of grey background
    theme(axis.text.x = element_text(angle=90),#rotate variable names to vertical
          axis.title.x=element_blank(),#get rid of x-axis label
          text=element_text(face="bold",color="black",size=rel(3)),#make text black and large
          axis.text = element_text(color = "black"))#make text black
  p3<-ggplot(df1,aes(x=my.alpha,y=log(my.lambda),fill = myPR))+
    geom_tile()+
    scale_fill_viridis(discrete = FALSE,name="AUPRC")+
    labs(x="Alpha",y="log(Lambda)")+
    theme_bw()
  
  p4<-ggplot(df2,aes(x=my.n,y=my.depth,fill=rfPR))+
    geom_tile()+
    scale_fill_viridis(discrete = FALSE,name="AUPRC")+
    labs(x="Number of Trees",y="Min. Node Size")+
    theme_bw()
  
  
  
  end.time<-Sys.time()#notes the end time
  time.taken<-round(end.time-start.time,2)#calculates the time taken to 2 decimals
  MyList<-list(my.model[[1]],my.best.alpha,my.best.lambda,Metrics,p1,
               time.taken,ConfMat,rf,p2,predicted,Predicted,ConfMat2,p3,p4,p2HO,ConfMat2HO,predictedHO)
  #makes a list of things to be returned by the function
  return(MyList)
}




