seed1<-42#set values that will be assigned to seeds in following functions
seed2<-42
seed4<-42

# ++++++++++++++++
# PatientIDSort
# ++++++++++++++++
# DataFrame1: a data frame I'm sorting the data into
# DataFrame2: the data frame made from the excel sheet
# DataFrame3: the column from my excel sheet with the patient IDs in it
PatientIDSort<-function(DataFrame1,DataFrame2,DataFrame3){
  for (i in 1:109){
    for (j in 1:length(DataFrame3)){
      if (DataFrame3[j]==i){DataFrame1[i]=DataFrame2[j]}  
      #Puts data into the row number corresponding to the ID number, 
      #i.e. patient 1 in row 1, etc.
    }}
  return(DataFrame1)}


# +++++++++++++++++
# OneHotVariableRename
# +++++++++++++++++
# DataFrame1: a data frame I'm pulling data from
# DataFrame2: a data frame I'm putting data into
OneHotVariableRename<-function(DataFrame1,DataFrame2){
  DataFrame2[,1]<-ifelse(DataFrame1=="Indeterminate",1,0)#indeterminate variable
  DataFrame2[,2]<-ifelse(DataFrame1=="Positive",1,0)#fix positive variable
  DataFrame2[,3]<-ifelse(DataFrame1=="Negative",1,0)#fix negative variable
  stopifnot(is.na(DataFrame2[,1])==is.na(DataFrame1))
  stopifnot(is.na(DataFrame2[,2])==is.na(DataFrame1))
  stopifnot(is.na(DataFrame2[,3])==is.na(DataFrame1))
  return(DataFrame2)
}

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
  
  
  #Leave one out cross validation (LOOCV)
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
  my.n<-matrix(nrow = my.length,ncol = length(my.seq4))
  my.depth<-matrix(nrow = my.length,ncol = length(my.seq4))
  my.object<-list()
  forests<-list()
  for (j in 1:length(my.seq)){#for each value of alpha in the grid
    for (n in 1:length(my.seq2)){#for each value of lambda in the grid
      myvector<-matrix(nrow=nrow(TrainData),ncol=1)
      for (i in 1:nrow(TrainData)){#for each holdout set
        DataFrame[[i]]<-TrainData[-i,]#sort the predictors into train
        Vector[[i]]<-mydepvector[-i]#sort the output into train
        TestData1<-as.matrix(TrainData[i,])#sort the predictors into test
        DepTest1<-as.data.frame(mydepvector[i])#sort the output into test
        set.seed(seed2)
        foldid=sample(rep(seq(10),length=nrow(DataFrame[[i]])))#hold foldid constant
        #by setting the seed at the same number before doing this each time
        k<-i+(j-1)*(n-1)*nrow(TrainData)#count total number of loops to index
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
      myvector2<-matrix(nrow=nrow(TrainData),ncol=1)#initialize temporary variable
      for (i in 1:nrow(TrainData)){#for each of the holdout sets
        DataFrame[[i]]<-TrainData[-i,]#sort the predictors into train
        Vector[[i]]<-mydepvector[-i]#sort the output into train
        TestData1<-as.matrix(TrainData[i,])#sort the predictors into test
        DepTest1<-as.data.frame(mydepvector[i])#sort the output into test
        
        rf<-ranger(Vector[[i]]~.,data=DataFrame[[i]],#build a random forest with
                   #train predictor, train output
                   num.trees = my.seq3[j],#given number of trees
                   replace = TRUE,#sample with replacement
                   importance="sobolMDA",#Sobol MDA importance measure
                   min.node.size = my.seq4[n],#given min node size
                   keep.inbag = TRUE)#save so that we can use later
        predicted<-predict(rf,TestData1)#predict using the random forest and test
        #predictors
        predicted<-as.data.frame(predicted)
        myvector2[i]<-sqrt(1/nrow(DepTest1)*sum((predicted-DepTest1)^2)) #calculate metric of fit
        
      }
      my.n[j,n]<-my.seq3[j]#save number of trees with same index as measure of fit
      RMSERF[j,n]=mean(myvector2)#average over the holdout sets
      SDRF[j,n]=sd(myvector2)
      my.depth[j,n]<-my.seq4[n]
      
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
    arrange(desc(abs(my.coefs$s1)))
  my.coefs<-my.coefs[!rownames(my.coefs) %in% "(Intercept)",,drop=FALSE]#gets rid of intercept
  my.coefs<-my.coefs[1:15,,drop=FALSE]#selects top 15 most important coefficients
  my.coefs<-my.coefs %>%#Gets rid of all coefficients that are zero
    filter(!s1==0)
  my.coefs<-as.data.frame(my.coefs)
  my.coefs<-my.coefs %>%#arranges the coefficients in descinding absolute value
    arrange(desc(abs(my.coefs$s1)))#have to do this again because they get rearanged by intermediate steps
  my.coefs<-rownames_to_column(my.coefs)#make a column and put the rownames there
  #note: rownames are the variable names
  my.coefs$rowname<-factor(my.coefs$rowname,levels = unique(my.coefs$rowname))
  #set rownames as factor so that they won't automatically graph in alphabetical order
  
  p1<-ggplot(my.coefs,aes(x=rowname,y=s1))+#plot the remaining coefficients in
    #order of descending magnitude with variable name as x-asis label
    geom_point(colour="#3c78d8",size=6)+#stick to theme colors and make dot big
    labs(x="Variable",y="Coefficient")+#label the axes
    theme_bw()+#get rid of grey
    theme(axis.text.x = element_text(angle=90),#turn the variable names vertical
          axis.title.x=element_blank(),#get rid of x axis title
          text=element_text(face="bold",color="black",size=rel(5)),#make font bold, large
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
  
  
  my.number<-my.n[as.numeric(my.indices)]#determine which number of trees optimizes metric
  my.optimal.depth<-my.depth[as.numeric(my.indices)]#determine which tree depth optimizes metric
  RMSEtest<-matrix(nrow = length(my.number),ncol = 1)#initialize outputs
  rf.regression<-list()
  for (i in 1:length(my.number)){#for each optimal number of trees with tree depth combo
    TrainData<-as.data.frame(TrainData)
    rf.regression[[i]]<-ranger(mydepvector~.,data=TrainData,num.trees = my.number[i],
                               #build a random forest using all train data, with chosen
                               replace = TRUE,#number of trees, keeping other parameters
                               importance="sobolMDA",#the same as in the loop
                               min.node.size = my.optimal.depth[i],
                               keep.inbag = TRUE)
    
    predicted<-predict(rf.regression[[i]],TestData)#predict using this random forest
    #and test predictors
    predicted<-as.data.frame(predicted)
    DepTest<-as.data.frame(DepTest)
    RMSEtest[i]<-sqrt((1/nrow(predicted))*sum((DepTest-predicted)^2))#evaluate using the same metric
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
  #make variable names a factor so they won't automatically be alphabetized when plotted
  p2<-ggplot(data = ImportanceMeasure,mapping = aes(x=rowname,#plot importance measures
                                                    y=ImportanceMeasure))+
    geom_point(size=6,fill="#de7608",color="#de7608")+#use color pallet
    expand_limits(y=0)+#force y-axis to include zero rather than starting at nonzero value
    labs(x="Variable",y="Importance Index")+#label axes
    theme_bw()+#get rid of grey background
    theme(axis.text.x = element_text(angle=90),#rotate variable names to vertical
          axis.title.x=element_blank(),#get rid of x-axis title
          text=element_text(face="bold",color="black",size=rel(5)),#make text black and big
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
               p3,p4,my.best.alpha,my.best.lambda)
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
  
  #LOOCV
  DataFrame<-list()#initialize outputs
  Vector<-list()
  my.seq3<-seq(50,1000,50)#sequence of numbers of trees to try
  my.seq4<-seq(10,30,2)#sequence of tree depths to grid search
  my.length<-length(my.seq3)#total number of iterations in the loop
  RMSERF<-matrix(nrow=my.length,ncol=length(my.seq4))#initialize outputs
  SDRF<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.n<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.depth<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.object<-list()
  forests<-list()
  
  for (j in 1:length(my.seq3)){#for each possible number of trees
    for (n in 1:length(my.seq4)){#for each possible tree depth
      myvector2<-matrix(nrow = nrow(TrainData),ncol = 1)#temporary object to store metric in before averaging
      for (i in 1:nrow(TrainData)){#for each of the holdout sets
        DataFrame[[i]]<-TrainData[-i,]#Make training data frame of predictors
        Vector[[i]]<-mydepvector[-i]#make training vector of outcome
        TestData1<-as.matrix(TrainData[i,])#make test data frame of predictors
        DepTest1<-as.data.frame(mydepvector[i])#make test data frame of outcome
        rf<-ranger(Vector[[i]]~.,data=DataFrame[[i]],#builds a random forest using
                   #training predictors and outcome
                   num.trees = my.seq3[j],#loop through number of trees options
                   replace = TRUE,#keep other settings the same as in other functions MyBin and MyCts
                   importance="sobolMDA",
                   min.node.size = my.seq4[n],
                   keep.inbag = TRUE)
        predicted<-predict(rf,TestData1)#predict using the random forest we just built
        #and the test predictors
        predicted<-as.data.frame(predicted)
        myvector2[i]<-sqrt((1/nrow(predicted))*sum((DepTest1-predicted)^2))#measure outcome using chosen metric
      }
      RMSERF[j,n]<-mean(myvector2)
      SDRF[j,n]<-sd(myvector2)
      my.n[j,n]<-my.seq3[j]#save number of trees with same index as metric
      my.depth[j,n]<-my.seq4[n]#save tree depth with same index too
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
  rf<-list()
  for (i in 1:length(my.number)){#for each optimal number of trees with tree depth combo
    TrainData<-as.data.frame(TrainData)
    rf[[i]]<-ranger(mydepvector~.,data=TrainData,num.trees = my.number[i],#build random
               #forest with all training data, keeping other parameters the same
               replace = TRUE,
               importance="sobolMDA",
               min.node.size = my.optimal.depth[i],
               keep.inbag = TRUE)
    predicted<-predict(rf[[i]],TestData)#predict using random forest and test predictors
    predicted<-as.data.frame(predicted)
    DepTest<-as.data.frame(DepTest)
    RMSEtest[i]<-sqrt((1/nrow(predicted))*sum((DepTest-predicted)^2))#evaluate using same metric
    
  }
  
  ImportanceMeasure<-rf[[1]][["variable.importance"]]#extract importance measures from the random forest
  ImportanceMeasure<-as.data.frame(ImportanceMeasure)
  ImportanceMeasure<-ImportanceMeasure %>%#sort the variables by descending importance measure
    arrange(desc(as.numeric(ImportanceMeasure)))
  ImportanceMeasure<-rownames_to_column(ImportanceMeasure)#make a column of variable names
  ImportanceMeasure<-ImportanceMeasure[1:15,]#keep only 15 most important variables
  ImportanceMeasure<-as.data.frame(ImportanceMeasure)
  ImportanceMeasure$rowname<-factor(ImportanceMeasure$rowname,
                                    levels=unique(ImportanceMeasure$rowname))
  #make variable names a factor so that they won't automatically alphabetize when we plot them
  p2<-ggplot(data = ImportanceMeasure,mapping = aes(x=rowname,
                                                    y=ImportanceMeasure))+
    #plot variables by decreasing importance measure
    geom_point(size=6,fill="#de7608",color="#de7608")+#use the theme colors
    expand_limits(y=0)+#force y-axis to start at 0 rather than a positive number
    labs(x="Variable",y="Importance Index")+#label axes
    theme_bw()+#get rid of grey background
    theme(axis.text.x = element_text(angle=90),#rotate variable names vertical
          axis.title.x=element_blank(),#get rid of x-axis title
          text=element_text(face="bold",color="black",size=rel(5)),#make text black and large
          axis.text = element_text(color = "black"))#make text black
  
  p3<-ggplot(dfRMSERF,aes(x=my.n,y=my.depth,fill=RMSERF))+
    geom_tile()+
    scale_fill_viridis(discrete = FALSE,name="RMSE")+
    labs(x="Number of Trees",y="Min. Node Size")+
    theme_bw()
  
  end.time<-Sys.time()#notes the end time
  time.taken<-round(end.time-start.time,2)#calculates the time taken to 2 decimals
  MyList<-list(time.taken,rf,RMSEtest,p2,p3,my.number,my.optimal.depth,RMSEtest)
  #make and return the relavant list of items
  return(MyList)
}



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
  
  #LOOCV
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
  my.n<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.depth<-matrix(nrow=my.length,ncol=length(my.seq4))
  my.object<-list()
  forests<-list()
  for (j in 1:length(my.seq)){#for each value of alpha to try
    for (n in 1:length(my.seq2)){#for each value of lambda to try
      myvector<-list()#temporary variable to hold metrics before averaging
      for (i in 1:nrow(TrainData)){#for each holdout set
        DataFrame[[i]]<-TrainData[-i,]#use index to select appropriate subset of predictors
        Vector[[i]]<-mydepvector[-i]#use index to select appropriate subset of outcomes
        TestData1<-as.matrix(TrainData[i,])#use index to select appropriate subset of predictors
        DepTest1<-factor(mydepvector[i], levels=c(0,1))#use index to select appropriate subset of outcomes
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
        Predicted<-as.data.frame(Predicted)
        ADataFramePR<-cbind(DepTest1,Predicted)#make a data frame to feed into pr_auc
        colnames(ADataFramePR)<-c("truth","Class1")#give it the correct column names
        
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
      for (i in 1:nrow(TrainData)){#for each holdout set
        DataFrame[[i]]<-TrainData[-i,]#use index to select appropriate subset of predictors
        Vector[[i]]<-mydepvector[-i]#use index to select appropriate subset of outcomes
        TestData1<-as.matrix(TrainData[i,])#use index to select appropriate subset of predictors
        DepTest1<-factor(mydepvector[i],levels=c(0,1))#use index to select appropriate subset of outcomes
        rf<-ranger(Vector[[i]]~.,data=DataFrame[[i]],#make a random forest with
                   #the cross-validation predictors and outcomes
                   num.trees = my.seq3[j],#iterate through number of trees
                   replace = TRUE,#keep other parameters the same as in
                   importance="sobolMDA",#other models for other outcome variables
                   min.node.size = my.seq4[n],#given tree depth
                   keep.inbag = TRUE,
                   classification = TRUE)#except for classification, as this is the only binary outcome
        predicted<-predict(rf,TestData1)#predict using the random forest and test predictors
        predicted<-as.data.frame(predicted)
        BDataFramePR<-cbind(DepTest1,predicted)#make a data frame to calculate the area under the PR curve
        colnames(BDataFramePR)<-c("truth","Class1")#give it the necessary column names
        myvector2[[i]]<-yardstick::pr_auc(BDataFramePR,truth="truth","Class1")#calculate the area
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
    Predicted<-as.numeric(Predicted$s1)
    DepTest<-as.numeric(unlist(DepTest))
    #calculate the confusion matrix for the test data and save to be reported out
    ConfMat[[i]]<-caret::confusionMatrix(factor(Predicted,levels = c(0,1)),
                                  factor(DepTest,levels = c(0,1)))$table}
  
  DepTest<-as.matrix(DepTest)
  Metrics<-assess.glmnet(object=my.model[[1]],newx=TestData,#calculate the metrics to assess
                         #given in the glmnet package
                         newy = DepTest,family="binomial",s="lambda.min")
  
  my.coefs<-as.matrix(my.coefs)
  my.coefs<-as.data.frame(my.coefs)
  
  my.coefs<-my.coefs %>%#arranges the coefficients in descending absolute value
    arrange(desc(abs(my.coefs$s1)))
  my.coefs<-my.coefs[!rownames(my.coefs) %in% "(Intercept)",,drop=FALSE]
  #get rid of the intercept so that we only have the coefficients for the variables
  my.coefs<-my.coefs[1:15,,drop=FALSE]#keep only the top 15 most influential
  my.coefs<-my.coefs %>%#Gets rid of all coefficients that are zero
    filter(!s1==0)
  my.coefs<-as.data.frame(my.coefs)
  my.coefs<-my.coefs %>%#do this again because an intermediate step disorganizes them
    arrange(desc(abs(my.coefs$s1)))
  my.coefs<-rownames_to_column(my.coefs)#make a column with variable names
  my.coefs$rowname<-factor(my.coefs$rowname,levels = unique(my.coefs$rowname))
  #make variable names a factor so that ggplot2 won't automatically alphabetize them
  p1<-ggplot(my.coefs,aes(x=rowname,y=s1))+#plot the coefficients in decreasing
    #absolute value with variable name on x-axis
    geom_point(colour="#3c78d8",size=6)+#keep to our color pallet
    labs(x="Variable",y="Coefficient")+#make axis labels
    theme_bw()+#get rid of grey
    theme(axis.text.x = element_text(angle=90),#rotate variable names to vertical
          axis.title.x=element_blank(),#get rid of x-axis title
          text=element_text(face="bold",color="black",size=rel(5)),#make text black and large
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
  ConfMat2<-list()
  rf<-list()
  for (i in 1:length(my.number)){#for each combination of parameters chosen
    TrainData<-as.data.frame(TrainData)
    rf[[i]]<-ranger(mydepvector~.,data=TrainData,num.trees = my.number[i],
               #build a random forest using all the training data with optimal 
               replace = TRUE,#number of trees
               importance="sobolMDA",#keeping other parameters the same
               min.node.size = my.optimal.depth[i],#as for other outcome variables
               keep.inbag = TRUE,
               classification=TRUE)#except for this, which is different because this outcome is binary
    predicted<-predict(rf[[i]],TestData)#predict outcomes using test predictors
    predicted<-as.data.frame(predicted)
    DepTest<-as.data.frame(DepTest)
    Pred<-predicted#get prediction vector from random forest
    Pred<-unlist(Pred)#make into a factor
    DepTestFactor<-unlist(DepTest)#get the ground truth vector
    ConfMat2[[i]]<-caret::confusionMatrix(Pred,#Calculate the confusion matrix for the random forest
                                   factor(DepTestFactor,levels = c(0,1)))$table}
  
  ImportanceMeasure<-rf[[1]][["variable.importance"]]#extract importance measures
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
  #makes variable names a factor so that they won't be automatically alphabetized
  p2<-ggplot(data = ImportanceMeasure,mapping = aes(x=rowname,
                                                    y=ImportanceMeasure))+
    #plots variable names by decreasing importance measure
    geom_point(size=6,fill="#de7608",color="#de7608")+#use theme colors and make dots large
    expand_limits(y=0)+#force y-axis to start at 0
    labs(x="Variable",y="Importance Index")+#label axes
    theme_bw()+#get rid of grey background
    theme(axis.text.x = element_text(angle=90),#rotate variable names to vertical
          axis.title.x=element_blank(),#get rid of x-axis label
          text=element_text(face="bold",color="black",size=rel(5)),#make text black and large
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
               time.taken,ConfMat,rf,p2,predicted,ConfMat2,p3,p4)
  #makes a list of things to be returned by the function
  return(MyList)
}