#' get the name of R package for some frequently used methods
#'
#' preset method and package pairs
#'   * xgbTree = xgboost
#'   * randomForest = randomForest
#'   * rf = randomForest
#'   * parRF = randomForest
#'   * ranger = ranger
#'   * glmnet = glmnet
#'   * NaiveBayes = klaR
#'   * svmRadial = e1071
#'
#'
#' @param method char; the name of the method
#'
#' @returns NA or the name of the package
#'
#' @examples # none
method2package=function(method){
  PACKAGE_NAMES=c(xgbTree='xgboost',randomForest='randomForest',rf='randomForest',
                  parRF='randomForest',glmnet='glmnet', ranger='ranger',
                  NaiveBayes='klaR', svmRadial='e1071')
  PACKAGE_NAMES[method]
}


#' reset `Inf` and `char` values in a data.frame
#'
#' `Inf` will be set as NA, and `char` columns as factor
#'
#' @param df data.frame
#' @param charAsFactor TRUE, logical; set `char` columns as factor
#'
#' @return a modified data.frame
#' @export
#'
#' @examples # none
df.reset=function(df,charAsFactor=TRUE){
  for (i in 1:ncol(df)) {
    x=df[[i]]
    if(is.numeric(x)){
      inds=which( is.infinite(x) )
      if(length(inds)>0) df[inds,i]=NA
    }else if(charAsFactor && is.character(x)){
      df[[i]]=as.factor(x)
    }
  }
  df
}


#' plot prediction probability histogram
#'
#' Produce back-to-back histogram of prediction probability distributions for
#' positive and negative labels
#'
#' @param predProbs data.frame; a one column data.frame where the column name is
#'   the positive label and rownames are subject IDs.
#' @param trueLabels char; the true class label
#' @param posLabel char; the positive label
#'
#' @return none
#' @export
#' @importFrom Hmisc histbackback
#'
#' @examples #none
plotPredProbs=function(predProbs, trueLabels,posLabel){
  if(!pacman::p_exists(Hmisc,TRUE)) pacman::p_install(Hmisc)

  defaultDigit = getOption('digits')
  options(digits = 2)
  out <- Hmisc::histbackback(
    split(predProbs, trueLabels),
    probability = FALSE,
    brks = seq(0, 1, 0.1),
    main = paste("distribution of predicted", posLabel, "probability", sep =' '),
    axes = TRUE,
    ylab = paste("predicted", posLabel, "probability", sep =' ')
  )
  #add color
  barplot(-out$left, col = "blue", horiz = TRUE, space = 0, add = TRUE, axes = FALSE)
  barplot(out$right, col = "red",  horiz = TRUE, space = 0, add = TRUE, axes = FALSE)
  options(digits = defaultDigit)
}



#' write the best-tune parameters to a file
#'
#' @param mFit the final model
#' @param fileName char; output file name
#' @param statTable the statistic table
#' @param aucVal NA, numeric; prediction AUC
#'
#' @return none
#'
#' @examples # none
writePredStats_bestTunes=function(mFit,fileName,statTable,aucVal=NA){
  sink(file = fileName)
  if(!is.na(aucVal)) cat('AUC:', aucVal,'\n')
  print(statTable)

  cat('\nTuned parameters and their optimal values:\n')
  if(class(mFit)=='randomForest') print(mFit[c('mtry','ntree')])
  else if(length(mFit$bestTune)>0) {
    if(is.data.frame(mFit$bestTune))
      rownames(mFit$bestTune)=''
    print(mFit$bestTune)
  }else  print(mFit$finalModel$params)
  sink()
}




#' compute the prediction stats of a model
#'
#' This function computes the
#' prediction stats of a model on the target variable of type `factor`.
#'
#' Following files are the output:
#'    * roc.pdf
#'    * prediction.hist.pdf
#'    * prediction.csv
#'    * confusion.table.txt
#'    * all.stats.rds
#'
#' @param mFit the model
#' @param newData data.frame; the new data
#' @param classInd integer; the index of the class column
#' @param posLabel NULL, char; the positive labels
#' @param ofPre NULL, char; the prefix of output file
#' @param predProbs NULL, data.frame; supplied prediction probability; column names
#'   should be the levels in the response variable.
#' @param nLevels NULL, the number of levels in the class column.
#' @param ... passed to `caret::predict.train()`
#'
#' @return a named numerical vector of following stats:
#'    * AUC
#'    * accuracy and Kappa statistic values
#'    * the sensitivity, specificity, positive predictive value, negative
#'    predictive value, precision, recall, F1, prevalence, detection rate,
#'    detection prevalence and balanced accuracy for each class
#' @export
#'
#' @examples # none
predStats.Categorical = function(mFit,
                                 newData,
                                 classInd,
                                 posLabel=NULL,
                                 ofPre = NULL,
                                 predProbs = NULL,
                                 nLevels=NULL,
                                 ...){
  if(is.null(nLevels)){
    k=unique(newData[[classInd]])
    nLevels=sum(!is.na(k))
  }

  predLabels = predict(mFit, newdata = newData[,-classInd])
  trueLabels = newData[, classInd]
  names(trueLabels) = rownames(newData)
  if (length(names(trueLabels)) != length(trueLabels))
    stop("newData does not have rownames!")

  confTable = caret::confusionMatrix(data = predLabels,
                                     reference = trueLabels,
                                     positive = posLabel)

  if (is.null(predProbs)){
    predProbs = try(predict(mFit, newdata = newData[,-classInd], type = 'prob', ...),
                    silent = TRUE)
  }

  predAUC=NA
  if(inherits(predProbs, "try-error")) {
    warning('The train model cannot predict probability, thus AUC cannot be computed.')
    predProbs=NA
  }else if(nLevels==2) {
    predProbs=predProbs[,posLabel]
    if(!pacman::p_exists(pROC,TRUE)) pacman::p_install(pROC)
    rocObj=pROC::roc(trueLabels,predProbs)
    predAUC=pROC::auc(rocObj)
  }


  predTable = cbind(newData[, classInd,FALSE],pred.Label=predLabels, pred.Prob=predProbs)
  allStats = list('predStats' = c(AUC=predAUC,confTable$overall,confTable$byClass),
                  'predTable' = predTable)

  if (!is.null(ofPre)) {
    pred.fn = paste(ofPre, "prediction.csv", sep = '.')
    confTable.fn = paste(ofPre, "confusion.table.txt", sep = '.')
    all.stats.fn = paste(ofPre, "all.stats.rds", sep = '.')

    writePredStats_bestTunes(mFit,confTable.fn,confTable,predAUC)
    write.csv(predTable,file = pred.fn)
    saveRDS(allStats, file = all.stats.fn)

    if((!identical(predProbs,NA)) && nLevels==2){
      roc.fn = paste(ofPre, "roc.pdf", sep = '.')
      probHist.fn = paste(ofPre, "prediction.hist.pdf", sep = '.')

      pdf(file = roc.fn)
      pROC::plot.roc(rocObj,print.auc=TRUE)
      dev.off()

      pdf(file = probHist.fn)
      plotPredProbs(predProbs, trueLabels,posLabel)
      dev.off()
    }
  }
  allStats$predStats
}





#' compute the prediction stats of a model
#'
#' This function computes the
#' prediction stats of a model on the target variable of type `numeric`.
#'
#' Following files are the output:
#'    * cor.png
#'    * residule.png
#'    * prediction.csv
#'    * model.stats.txt
#'    * all.stats.rds
#'
#' @param mFit the model
#' @param newData data.frame; the new data
#' @param classInd integer; the index of the class column
#' @param posLabel NULL, char; the positive labels
#' @param ofPre NULL, char; the prefix of output file
#' @param predVals NULL, data.frame; supplied prediction.
#' @param ... passed to `caret::predict.train()`
#'
#' @return a named numerical vector of `R2, RMSE, MAE`
#' @export
#'
#' @examples # none
predStats.numerical = function(mFit,
                               newData,
                               classInd,
                               ofPre = NULL,
                               predVals = NULL,
                               ...) {
  if(!pacman::p_exists(pROC,TRUE)) pacman::p_install(pROC)

  if (is.null(predVals))
    predVals = predict(mFit, newdata = newData[,-classInd], ...)

  predTable = cbind(newData[, classInd,FALSE], predicted.value=predVals)
  predTable$residule=predTable[,1]-predTable[,2]
  predTable$std.residule=sqrt(abs(scale(predTable$residule)))

  allStats = c(R2 = R2(predTable[,2], predTable[,1]),
               RMSE = RMSE(predTable[,2], predTable[,1]),
               MAE = MAE(predTable[,2], predTable[,1]))

  if (!is.null(ofPre)) {
    cor.fn = paste(ofPre, "pred.cor.png", sep = '.')
    residule.fn = paste(ofPre, "pred.residule.png", sep = '.')
    pred.fn = paste(ofPre, "pred.csv", sep = '.')
    model.stats.fn = paste(ofPre, "model.stats.txt", sep = '.')
    all.stats.fn = paste(ofPre, "all.stats.rds", sep = '.')

    write.csv(predTable,file = pred.fn)
    saveRDS(allStats, file = all.stats.fn)
    writePredStats_bestTunes(mFit,model.stats.fn,allStats)

    p1=ggpubr::ggscatter(predTable,colnames(predTable)[2],colnames(predTable)[1],
                         alpha=0.5,
                         add = "reg.line",  # Add regressin line
                         add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                         conf.int = TRUE, # Add confidence interval
                         cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                         cor.coeff.args = list(method = "pearson", label.sep = "\n"))
    ggsave(cor.fn,p1,height = 7, width = 10)

    p2=ggpubr::ggscatter(predTable,'predicted.value','residule',alpha=0.5)+
      ggplot2::geom_hline(yintercept = 0,linetype='dashed')
    p3=ggpubr::ggscatter(predTable,'predicted.value','std.residule',
                         ylab = "Sqrt(|Standardized residuals|)",
                         title = 'Scale-Location Plot',alpha=0.5)
    p4=ggpubr::gghistogram(predTable,'residule',title = 'histogram of residule',alpha=0.5)
    p5=ggplot2::ggplot(predTable, aes(sample = residule)) +
      ggplot2::stat_qq(alpha=0.5) +
      ggplot2::stat_qq_line() +
      ggplot2::labs(title = "QQ plot of residule", x = "Theoretical Quantiles", y = "Sample Quantiles")+
      ggplot2::theme_classic()
    #p5=ggpubr::ggqqplot(predTable,'residule',title = 'QQ plot of residule',alpha=0.5)
    p1=ggpubr::ggarrange(p2,p3,p4,p5)
    ggsave(residule.fn,p1,height = 14, width = 20)
  }

  return(allStats)
}



#' draw classic MDS plots
#'
#' @param phenoDF data.frame; row: subject; column: features to be used as color in the plot
#' @param fMat NULL, matrix; row: subjects at the same order as `phenoDF`; column: features
#' @param distMat distance matrix; subjects at the same order as `phenoDF`;
#' @param PC 1:3, integer vector; dimensions to plot
#' @param figMain NULL, char; title of the plots
#' @param ofPre NULL, char; prefix of output file name (including path)
#' @param colorCol 'black', char; the color of each subject in the plot; can also be
#'   a column in `phenoDF`
#' @param doReturn TRUE, logical; should a list of ggplot objects be returned?
#' @param ... passed to `ggpubr::ggscatter`
#'
#' @return a list of ggplot objects if `doReturn = TRUE`
#' @export
#' @importFrom ggpubr ggscatter
#' @importFrom ggplot2 ggsave
#'
#' @examples # none
draw.classic.MDS=function(phenoDF,fMat=NULL,distMat=NULL,PC=1:3,figMain=NULL,
                          ofPre=NULL, colorCol="black",doReturn=TRUE,...){
  if(!is.null(fMat)) {    disMat<- dist(fMat)  }
  mds1 <- as.data.frame(cmdscale(distMat,k=3))
  colnames(mds1)=sub('V','dimension.',colnames(mds1),fixed = TRUE)
  mds1=cbind(phenoDF[rownames(mds1),,FALSE],mds1)

  pcInd=combn(PC,2)
  pList=list()
  for(i in 1:ncol(pcInd)){
    pcx=paste0("dimension.",pcInd[1,i])
    pcy=paste0("dimension.",pcInd[2,i])

    pList[[i]]=ggpubr::ggscatter(mds1,pcx,pcy,color = colorCol,title = figMain,...)
    if(!is.null(ofPre)){
      ggplot2::ggsave(paste(ofPre,".MDS",pcInd[1,i],pcInd[2,i],'.png',sep=''),pList[[i]],dpi=300,
                      height = 10,width = 16)
    }
  }

  if(doReturn) return(pList)
}



#' compare the unique values of non-numerical columns of two data.frames
#'
#' Two data frames are matched if the unique values of each of their non-numerical columns are same.
#' Then a model trained from one data frame can predict the response in the other.
#'
#' @param df1,df2 two data frames with identical column names and orders
#'
#' @returns a matrix of following columns; NULL means matched.
#'   * column: column names of mis-match
#'   * df1-df2: additional unique values in df1
#'   * df2-df1: additional unique values in df1
#' @export
#'
#' @examples # none
df.matched=function(df1,df2){
  k=NULL
  for (i in colnames(df1)) {
    if(!is.numeric(df1[[i]])){
      a=unique(df1[[i]])
      a=sort(a[!is.na(a)])
      b=unique(df2[[i]])
      b=sort(b[!is.na(b)])
      if(!identical(a,b)){
        kk=c(i,paste(setdiff(a,b),collapse = ','),paste(setdiff(b,a),collapse = ','))
        if(is.null(k)) k=kk else k=rbind(k,kk)
      }
    }
  }

  if(!is.null(k)){
    if(is.vector(k)) k=matrix(k,ncol = 3)
    colnames(k)=c('column','df1-df2','df2-df1')
  }

  return(k)
}



#' data preprocessing before training
#'
#' This function does the preprocessing before training
#'
#' options for preprocessing:
#'    + dummy variable encoding
#'    + modification to the whole dataset before training, including
#'      - imputation using `missForest` and `KNN`
#'      - detection and removal of `Near Zero Variance (NZV)` and `ConditionalX` variables.
#'      Note that these modifications apply only in situations where `caret::train`
#'      is not used in the model building; for example
#'        - ML method is `glm` where no cross validation (CV) is needed due to the absence of hyper-parameters
#'        - ML method is `randomForest` where the training is done using [randomForest::tuneRF()]
#'      Otherwise, those modification should be specified in `caret::train(preProc)` to avoid
#'      information leakage during training. Note that `missForest` is not available
#'      in `preProc`.
#'
#' @param allDF NULL, data.frame; the data.frame of all the data
#' @param trainDF NULL, data.frame; the data.frame of the training data
#' @param testDF NULL, data.frame; the data.frame of the testing data
#' @param prior.nzvCondX NULL, char; remove NZV and conditionalX variables before training.
#'   This can be used only if `caret::train` is not used in the model building.
#'   Following are the available options:
#'    * `NULL`: do not remove before training
#'    * one of `n, nx`:
#'      + n: NZV
#'      + nx: NZV and ConditionalX
#'   if detected, removed variables are written to files `nzv.csv` and `conditionalX.txt`
#' @param dummyEncoding `cMethod %in% c('xgbTree','svmRadial')`,logical; is dummy variable encoding needed before training;
#' @param prior.impute `c(NA,'missForest','KNN')`, char; impute missing values before training.
#'   This can be used only if `caret::train` is not used in the model building.
#'   Following are the available options:
#'    * `NA`: do not impute before training
#'    * one of `missForest, KNN`: impute using [missForest::missForest()] or [DMwR2::knnImputation()]
#' @param classCol char; the column name of the class, i.e. the target variable
#' @param posLabel,negLabel NULL, char; the positive and negative labels for two-class classification
#' @param oDir NULL, char; output directory
#'
#' @return a list containing `trainDF`, `testDF`, and `allDF` post processing.
#' @export
#' @importFrom pacman p_exists p_install
#' @importFrom missForest missForest
#' @importFrom DMwR2 knnImputation
#'
#' @examples # none
df.preprocess = function(allDF = NULL,
                         trainDF = NULL,
                         testDF = NULL,
                         prior.nzvCondX = c('n','nx'),
                         dummyEncoding=FALSE,
                         prior.impute=c(NA,'missForest','KNN'),
                         classCol,
                         posLabel = NULL,
                         negLabel = NULL,
                         oDir=NULL){

  onlyAll=is.null(trainDF) && is.null(testDF)
  onlyTrain=is.null(allDF) && is.null(testDF)
  bothTT=!(is.null(trainDF) || is.null(testDF))


  if(is.null(allDF)){
    if(is.null(trainDF)) stop('allDF and trainDF cannot be all NULL')
    allDF=`if`(is.null(testDF),trainDF,rbind(trainDF,testDF))
  }

  # move class column to the 1st
  classInd=which(colnames(allDF)==classCol)
  if(classInd != 1){
    k = setdiff(colnames(allDF),classCol)
    allDF=allDF[c(classCol,k)]
    classInd = 1
  }


  #no space allowed in labels
  if (!is.null(posLabel) && any(grepl("\\s", c(posLabel, negLabel)))) {
    posLabel = gsub("\\s+", '', posLabel)
    negLabel = gsub("\\s+", '', negLabel)
    allDF[, classCol] = gsub("\\s+", '', allDF[, classCol])
  }

  # change Inf to NA and character to factor, Inf and char columns also trigger errors in missForest
  allDF=df.reset(allDF)
  # remove rows where class column is NA
  rInds=which(is.na(allDF[,classCol]))
  if(length(rInds)>0) {
    allDF=allDF[-rInds,]
    warning('Following rows were removed due to NA in response varible:\n',
            paste(rownames(allDF)[rInds],collapse = '\n'))
  }

  if(!is.numeric(allDF[, classCol]) && !is.null(posLabel)){
    allDF[, classCol] = factor(allDF[, classCol],levels = c(negLabel,posLabel))
  }

  #since NA in classCol were removed, no imputation in classCol here.
  if(!is.na(prior.impute)){
    if(substr(prior.impute[1],1,1)=='m'){
      if(!pacman::p_exists(missForest,TRUE)) pacman::p_install(missForest)
      k=missForest::missForest(allDF)
      allDF=k$ximp
      rm(k)
    }else if(substr(prior.impute[1],1,1)=='K'){
      if(!pacman::p_exists(DMwR2,TRUE)) pacman::p_install(DMwR2)
      allDF=DMwR2::knnImputation(allDF)
    }else{
      stop(prior.impute[1], ' not implemented yet!')
    }
  }

  if(dummyEncoding){
    k=dummy.encode(allDF[-classInd])
    allDF=cbind(allDF[,classInd,FALSE],k)
    classInd=1
  }

  # remove NZV columns
  if (!is.null(prior.nzvCondX)) {
    rmInds = rm.NZV.condX(allDF,
                         `if`(prior.nzvCondX[1]=='n',NULL,classCol),
                         oDir)
    if (length(rmInds) > 0) {
      warning(length(rmInds), ' columns are removed due to Near Zero Variance:\n',
              paste(colnames(allDF)[rmInds],sep = '\n'))
      if(classInd %in% rmInds){
        stop('class column was removed due to NZV or condX')
      }else{
        allDF = allDF[,-rmInds]
      }
    }
  }

  # create train and test
  repeat{
    k=NULL
    if(onlyAll){ # only allDF is provided
      k <- createDataPartition(y = allDF[, classCol], p = 0.75, list = FALSE)
    }else if(bothTT){ # allDF is not provided, but both trainDF and testDF are provided
      k <- 1:nrow(trainDF)
    }

    if(onlyTrain) { # only trainDF is provided
      trainDF=allDF
      break
    }else {
      trainDF <- allDF[k,]
      testDF <- allDF[-k,]

      kk=df.matched(trainDF,testDF)
      if(is.null(kk)) break
      if(bothTT) {
        write.csv(kk,file.path(oDir,'mismatch.csv'),row.names = FALSE)
        stop('The supplied training and testing datasets do not match! see
             mismatch.csv for details.')
      }
    }
  }

  list(train=trainDF,test=testDF,all=allDF)
}





#' concatenate two lists
#'
#' The final list contains items in both lists; but items in the left list that
#' share the same name as those in the right list are overwritten by the latter.
#'
#' @param list1 the left list
#' @param list2 the right list
#'
#' @return the final list
#' @export
#'
#' @examples # none
list.concat=function(list1,list2){
  k=intersect(names(list1),names(list2))
  if(length(k)>0){
    list1[k]=NULL
  }
  c(list1,list2)
}



#' run machine learning models
#'
#' This function compiles tools in `caret` packages to provide a machine learning (ML)
#' interface where users only need to provide the name of the ML method and the rest
#' are handled automatically.
#'
#' The capabilities of this function include the following:
#'    * preprocessing of the whole dataset
#'      + dummy variable encoding
#'      + modification to the whole dataset before training, including
#'        - imputation using `missForest` and `KNN`
#'        - detection and removal of `Near Zero Variance (NZV)` and `ConditionalX` variables
#'        Note that these modifications apply only in situations where `caret::train`
#'        is not used in the model building; for example
#'
#'          ✦ ML method is `glm` where no cross validation (CV) is needed due to the absence of hyper-parameters
#'
#'          ✦ ML method is `randomForest` where the training is done using [randomForest::tuneRF()]
#'
#'        Otherwise, those modification should be specified in `preProc` to avoid
#'        information leakage during training. Note that `missForest` is not available
#'        in `preProc`.
#'    * training and testing
#'    * the output of various files of the training and testing stats:
#'      +  *nzv.csv*: columns removed due to NZV and the details of each columns
#'      +  *train.all.stats.rds*: stats of the prediction of training model on the training dataset
#'      +  *train.cv.pdf*: curve of prediction performance during CV for upto four tuning parameters
#'      +  *train.model.rds*
#'      +  *train.prediction.csv*: prediction for each subject in training set
#'      +  *train.var.imp.pdf*: plot of variable importance
#'      +  *train.var.imp.csv*: importance score of each variable
#'      +  *test.all.stats.rds*
#'      +  *test.prediction.csv*
#'      +  *$cMethod.image.rdata*: running environment
#'      for classification
#'      +  *train.confusion.table.txt*
#'      +  *train.prediction.hist.pdf*: prediction histogram
#'      +  *train.roc.pdf*: ROC curve
#'      +  *train.MDS12.png, train.MDS13.png, train.MDS23.png*: MDS plots of subjects if trained via `randomForest`
#'      +  *test.confusion.table.txt*
#'      +  *test.prediction.hist.pdf*
#'      +  *test.roc.pdf*
#'      +  *test.MDS12.png, test.MDS13.png, test.MDS23.png*
#'      +  *conditionalX.txt*: columns removed due to conditionalX
#'      for regression
#'      +  *train.pred.cor.png*: scatter plot of raw and predicted values
#'      +  *train.pred.residual.png*: various diagnostic residual plots
#'      +  *test.pred.cor.png*: scatter plot of raw and predicted values
#'      +  *test.pred.residual.png*: plots of raw and standardized residuals
#'    * errors are handled to avoid crush.
#'
#'
#' @param allDF NULL, data.frame; the data.frame of all the data where rows are
#'   observations and columns are features; training and testing will be sampled
#'   at the ratio of 75 vs 25.
#' @param trainDF NULL, data.frame; the data.frame of the training data
#' @param testDF NULL, data.frame; the data.frame of the testing data
#' @param preProc `c("center", "scale",'nzv')`, char vector; preprocessing options; see [caret::preProcess()]
#' @param classCol char; the column name of the class, i.e. the target variable
#' @param posLabel,negLabel NULL, char; for classification only; the positive and negative labels for two-class classification
#' @param cMethod char; the name of the ML method
#' @param packName NULL, char; the name of the package containing the ML method.
#'   It's not needed if the `cMethod` is among those preset in [method2package]
#' @param trainInd NULL, list of integers; the indices of the resampling iteration in CV; see `index` in [caret::trainControl()]
#' @param dummyEncoding `cMethod %in% c('xgbTree','svmRadial')`,logical; is dummy variable
#'   encoding needed on the combined training and testing datasets before training?
#'   Although `caret::train()` handles dummy variable encoding automatically during CV and
#'   prediction on testing. It's possible that there are levels in the holdout or
#'   testing samples that are absent from the training data, causing errors.
#' @param prior.nzvCondX NULL, char; remove NZV and conditionalX variables before training.
#'   This can be used only if `caret::train` is not used in the model building.
#'   Following are the available options:
#'    * `NULL`: do not remove before training
#'    * one of `n, nx`:
#'      + n: NZV
#'      + nx: NZV and ConditionalX
#'   if detected, removed variables are written to files `nzv.csv` and `conditionalX.txt`
#' @param prior.impute `c(NA,'missForest','KNN')`, char; impute missing values before training.
#'   This can be used only if `caret::train` is not used in the model building.
#'   Following are the available options:
#'    * `NA`: do not impute before training
#'    * one of `missForest, KNN`: impute using [missForest::missForest()] or [DMwR2::knnImputation()]
#' @param oDir NULL, char; output directory
#' @param trainCtrlParas NULL, list; parameters for train control in [caret::trainControl()].
#'   Here are its default values if `method` is not `oob`:
#'    * `savePredictions = 'final'`,
#'    * if `trainInd` is provided, `index = trainInd`
#'    * otherwise `method = "repeatedcv",repeats = 3,number = 10`, which has the best bias-variance balance based on this
#'    [post](http://appliedpredictivemodeling.com/blog/2014/11/27/vpuig01pqbklmi72b8lcl3ij5hj2qm)
#'    from Max Kuhn, the author of `caret`.
#'
#'    additional defaults for classification:
#'    * `classProbs = TRUE`,
#'    * `summaryFunction = twoClassSummary` or `multiClassSummary` depending on the class column
#' @param trainParas NULL, list; parameters for train in [caret::train()].
#'   Here are its default values:
#'    * `tuneLength = 10`
#'
#'      `tuneLength` is the number of levels for each tuning parameters that should
#'   be generated by train. If trainControl has the option `search = "random"`, this
#'   is the maximum number of tuning parameter combinations that will be generated
#'   by the random search. if `search = "grid"` which is the default, this is the maximum number of levels
#'   of each tuning parameter. The actual number of levels used may be less than tuneLength.
#'    additional defaults for classification:
#'    * `metric = "ROC"` or `'Accuracy'` depending on the class column
#'
#' @return a list of the following items
#'    * `imp`: data.frame, variable importance scores
#'    * `trainStats`: a named numerical vector containing AUC and various stats
#'         in the confusion table from the prediction on the training dataset.
#'    * `testStats`: similar, but from the testing dataset.
#' @export
#'
#' @examples # see https://blueskypie.github.io/ezML/articles/ezML-intro.html
ml.run = function(allDF = NULL,
                  trainDF = NULL,
                  testDF = NULL,
                  preProc = c("center", "scale",'nzv'),
                  classCol,
                  posLabel = NULL,
                  negLabel = NULL,
                  cMethod,
                  packName = NULL,
                  trainInd = NULL,
                  dummyEncoding= cMethod %in% c('xgbTree','svmRadial'),
                  prior.nzvCondX = NULL,
                  prior.impute=c(NA,'missForest','KNN'),
                  oDir = '.',
                  trainCtrlParas = NULL,
                  trainParas=NULL){
  tryCatch({
    if(is.null(packName)) {
      packName=method2package(cMethod)
      if(is.na(packName)) packName=NULL
    }
    pacman::p_load(char=c('caret', packName))


    oDir=file.path(oDir,cMethod)
    dir.create(oDir,recursive = TRUE)
    ofPre=file.path(oDir,cMethod)


    # preprocessing -----
    k=df.preprocess(allDF = allDF,
                    trainDF = trainDF,
                    testDF = testDF,
                    prior.nzvCondX = prior.nzvCondX,
                    dummyEncoding=dummyEncoding,
                    prior.impute=prior.impute[1],
                    classCol = classCol,
                    posLabel = posLabel,
                    negLabel = negLabel,
                    oDir=oDir)
    trainDF=k$train
    testDF=k$test
    allDF=k$all
    classInd = 1
    # end of preprocessing -------

    yNumerical=is.numeric(allDF[[classInd]])
    nClass=ifelse(yNumerical,NA,length(unique(allDF[[classInd]])))

    # beginning of training-------
    trainFR=`if`(is.null(oDir),NULL,paste0(ofPre,'.train'))
    if(cMethod=='randomForest'){
      mtry.fig.fn = paste(trainFR, "cv.mtry.pdf", sep = '.')
      pdf(mtry.fig.fn,height = 7, width = 10)
      mFit=tuneRF(x = trainDF[,-classInd],y = trainDF[,classInd], doBest=TRUE,
                  importance=TRUE,proximity=TRUE)
      dev.off()
    }else{
      # train control para--------
      isOOB=identical(trainCtrlParas$method,'oob')
      h=list()
      if(!is.null(trainInd)){
        h=c(h,index = trainInd)
      }else if(is.null(trainCtrlParas$method)){
        h=c(h,method = "repeatedcv",repeats = 3,number = 10)
      }

      if(!(yNumerical || isOOB)){
        # summaryFunction is for CV, i.e. training and holdout samples
        #  it's ignored for OOB since OOB doesn't have CV.
        h=c(h, classProbs = TRUE,
            summaryFunction = `if`(nClass==2,twoClassSummary,multiClassSummary))
      }

      trainCtrlParas=list.concat(h,trainCtrlParas)
      trainCtrl=do.call(trainControl,trainCtrlParas)

      # train para --
      h=list(tuneLength = 10)
      if(!yNumerical){
        h=c(h,metric = ifelse(nClass>2 || isOOB,'Accuracy','ROC'))
      }
      trainParas=list.concat(h,trainParas)


      mFit <- do.call(train,
                      c(list(x = trainDF[,-classInd],
                             y = trainDF[, classInd],
                             trControl   =   trainCtrl,
                             method   =   cMethod,
                             preProc = preProc
                      ),
                      trainParas))
    }


    trainStats = `if`(yNumerical, predStats.numerical(mFit, trainDF, classInd, trainFR),
                      predStats.Categorical(mFit, trainDF, classInd, posLabel, trainFR,nLevels=nClass))

    if(!is.null(testDF)){
      testFR=`if`(is.null(oDir),NULL,paste0(ofPre,'.test'))
      testStats = `if`(yNumerical, predStats.numerical(mFit, testDF, classInd, testFR),
                       predStats.Categorical(mFit, testDF, classInd, posLabel, testFR,nLevels=nClass))
    }


    varScore=NULL
    mFit.fn = paste(trainFR, "model.rds", sep = '.')
    cv.fig.fn = paste(trainFR, "cv.pdf", sep = '.')
    imp.fn = paste(trainFR, "var.imp.csv", sep = '.')
    imp.fig.fn = paste(trainFR, "var.imp.pdf", sep = '.')

    saveRDS(mFit, file = mFit.fn)

    if(cMethod=='randomForest'){
      pdf(paste(trainFR, "cv.ntree.pdf", sep = '.'))
      plot(mFit)
      if(!yNumerical){
        k=ncol(mFit$err.rate)
        legend("top", legend=colnames(mFit$err.rate),col=1:k,cex=0.8,fill=1:k,horiz=TRUE)
      }
      dev.off()

      varScore=importance(mFit)
      k=ifelse(yNumerical,'%IncMSE','MeanDecreaseGini')
      viScore=varScore[,k]
      write.csv(varScore,file = imp.fn)
      pdf(imp.fig.fn)
      varImpPlot(mFit)
      dev.off()

      draw.classic.MDS(phenoDF = trainDF[, classInd,FALSE],distMat=1-mFit$proximity,
                       colorCol = classCol,ofPre = trainFR)

      if(!is.null(testDF)){
        testFit=predict(mFit,testDF[,-classInd], type = ifelse(yNumerical,'response','prob'),proximity=TRUE)
        draw.classic.MDS(phenoDF = testDF[, classInd,FALSE],distMat=1-testFit$proximity,
                         colorCol = classCol,ofPre = testFR)
        saveRDS(testFit, file = paste(testFR, "model.rds", sep = '.'))
      }
    }else{
      #fail if there is >4 tuning parameters
      try({
        p=ggplot(mFit)+ggtitle('train CV performance')
        ggplot2::ggsave(cv.fig.fn,p)
      }, silent = TRUE)

      try({
        varScore = varImp(mFit, scale = TRUE)
      }, silent = TRUE)
      viScore=varScore$importance

      if(!is.null(viScore)){
        nTopVar=min(c(nrow(viScore),30))
        p=ggplot(varScore,top =nTopVar) +
          ggtitle("Variable Importance for top predictors")+
          theme_minimal()
        ggplot2::ggsave(imp.fig.fn,p)

        write.csv(viScore,file = imp.fn)
      }else{
        write.csv(varScore,file = imp.fn)
        warning('variable importance not plotted because the column name is not called "importance"')
      }
    }


    reList = list('imp' = viScore,'trainStats' = trainStats)
    if (!is.null(testDF))
      reList[['testStats']] = testStats

    return(reList)
  }, error = function(cond) {
    message("***Error caught and image saved:")
    message(cond,'\n')
  }, finally = {
    save(
      list = ls(envir = environment(), all.names = TRUE),
      file = paste(ofPre, 'image.rdata', sep = '.'),
      envir = environment()
    )
    #save(list = ls(all.names = TRUE), file = ".RData", envir = .GlobalEnv)
  })
}










#' remove low variance/varying and conditionalX features
#'
#' low varying features are the non-numeric features where the most frequent value
#' accounts for >95% of all values. low variance and conditionalX features are those defined in
#' [caret::nearZeroVar()] and [caret::checkConditionalX()]
#'
#' @param df1 data.frame; the input
#' @param y NULL, char; the column name of the response variable
#' @param ofPre NULL, char; the prefix of output file names:
#'      * conditionalX.txt
#'      * nzv.csv
#'
#' @return integer vector; the removed indices
#' @export
#'
#' @examples #none
rm.NZV.condX = function(df1, y=NULL,ofPre = NULL) {
  pacman::p_load(char='caret')
  # doMC does not apply to Windows
  # if(!require('doMC')){
  #   pacman::p_load(char='doMC')
  #   registerDoMC(cores = 5)
  #   set.seed(107)
  # }

  xInd = NULL
  nIndn = NULL
  nIndf = NULL
  percentUnique = NULL
  df1=as.data.frame(df1)

  # NZV begin--------
  cInds=which(!(sapply(df1,is.numeric)))
  if(length(cInds)>0){
    Xf=df1[,cInds,FALSE]
    Xn=df1[,-cInds,FALSE]

    percentUnique=sapply(Xf, function(x){
      k=sort(table(x))
      if(length(k)>1){
        uniCount=sum(k[-length(k)]) #k[length(k)] is the most common value
        uniPerc=uniCount/sum(k)
        return(uniCount/sum(k))  #uniCount < minUniCount |
      }
      return(0)
    })

    nIndf = which(names(percentUnique)[percentUnique<0.05] %in% colnames(df1))
  }else{
    Xn=df1
  }
  nzvDF <- nearZeroVar(Xn, saveMetrics = TRUE, foreach = TRUE)
  nIndn = match(rownames(nzvDF)[nzvDF$nzv], colnames(df1))
  if (!is.null(ofPre)) {
    fn=paste(ofPre, 'nzv.csv', sep = '.')
    if(length(nIndn) > 0){
      write.csv(nzvDF,file = fn)
    }

    if(length(nIndf) > 0){
      write.csv(data.frame(percentUnique),file = fn,append=TRUE)
    }
  }

  # NZV end-----

  if(!is.null(y)){ #conditionX ------
    xNames=setdiff(colnames(df1),y)
    if (!is.factor(df1[[y]]))  df1[,y] = as.factor(df1[,y])

    xInd = checkConditionalX(df1[xNames], df1[[y]])
    if (length(xInd) > 0 && !is.null(ofPre)) {
      writeLines(xNames[xInd], con = paste(ofPre, 'conditionalX.txt', sep ='.'))
      xInd=which(xNames[xInd] %in% colnames(df1))
    }
  }

  rmInd = union(xInd, c(nIndn,nIndf))
  return(rmInd)
}



#' encode categorical variables
#'
#' convert categorical columns into numerical ones of full rank
#'
#' It does the following:
#'    1. for nominal character variable of n categories, n-1 binary variables are created.
#'    2. for nominal factor variable of n levels and m categories (m<=n),
#'        * if `useLevel = TRUE`,  n-1 binary variables are created.
#'        * otherwise, m-1 binary variables are created.
#'    3. for ordinal factor variable of n levels and m categories,
#'        * if m equals 2,  one variable assuming equal distance among levels are created
#'        * otherwise, n-1 variable assuming linear and 2 to n-1 degree of polynomial
#'        relationship among levels are created.
#'    4. cells of NA are kept.

#' @param df1 data frame; columns are variables.
#' @param useLevel FALSE, logical; if TRUE, for factor columns, the unique values
#'   will be factor levels, instead of the actual levels present in the data.
#' @param ... passed to `caret::dummyVars()`
#'
#' @returns a new data.frame where all categorical columns are split into dummy
#'   columns of values between -1 and 1.
#' @export
#'
#' @examples
#' \dontrun{
#' k=matrix(1:10,nrow = 5)
#' colnames(k)=c('n1','n2')
#' k=as.data.frame(k)
#' k$c5=LETTERS[1:5]
#' k$c2na=c('a','a',NA,'b','b')
#' k$f2na=factor(k$c2na)
#' k$f2naL2O=factor(k$f2na,ordered = TRUE)
#' k$f2naL3=factor(k$c2na,levels = c('a','b','c'))
#' k$f2naL3O=factor(k$f2naL3,ordered = TRUE)
#' k$f3na=factor(c('a','a',NA,'b','c'))
#' k$f3naL3O=factor(k$f3na,ordered = TRUE)
#' k$f3naL4O=factor(k$f3na,levels = c('a','b','c','d'),ordered = TRUE)
#' k$f3naL5O=factor(k$f3na,levels = c('a','b','c','d','e'),ordered = TRUE)
#' k
#' dummy.encode(k)
#' }
dummy.encode=function(df1,useLevel=FALSE,...){
  k = caret::dummyVars(" ~ .", data = df1,fullRank = TRUE,...)
  k = data.frame(caret:::predict.dummyVars(k, newdata = df1))

  cInds=NULL
  if(!useLevel){
    u1=sapply(k, function(x){length(unique(x[is.finite(x)]))==1})
    cInds=which(u1)
  }

  if(length(cInds)>0) k[-cInds] else k
}
