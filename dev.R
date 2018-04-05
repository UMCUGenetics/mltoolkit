#%%%%%%%%% Inputs %%%%%%%%%#
#========= Paths =========#
localPath <- '/Users/lnguyen/hpc/cog_bioinf/cuppen/project_data/Luan_PCAGW/results/hr_detect/models/logReg_finalModel/'
if( dir.exists(localPath) ){
   base_dir <- localPath
} else {
   base_dir <- sub('/Users/lnguyen','',localPath)
}

#========= Donor white list =========#
## donors that are CERTAIN to be BRCA proficient or deficient
donorWhitelist <- read.table( '/Users/lnguyen/hpc/cog_bioinf/cuppen/project_data/Luan_PCAGW/ML_data/BRCA-EU/donorWhitelist.tsv', sep='\t',header=T)
donorWhitelistNames <- rownames(donorWhitelist)

#========= Input: UMCU data =========#
# #--------- Norm signature data ---------#
# df_umcuNormData <- read.table('/Users/lnguyen/hpc/cog_bioinf/cuppen/project_data/Luan_PCAGW/ML_data/BRCA-EU/df_umcuNormData_BRCAannot.tsv', sep='\t',header=T)
# df_umcuNormData$response <- df_umcuNormData$response %>% get_simpleResponse(.,c('BRCA1','BRCA2'),1,'none',0)
#
# df_umcuNormData_whitelist <- df_umcuNormData %>% .[rownames(.) %in% donorWhitelistNames,]

#--------- Raw signature data ---------#
df_umcuData <- read.table('/Users/lnguyen/hpc/cog_bioinf/cuppen/project_data/Luan_PCAGW/ML_data/BRCA-EU/df_umcuData_BRCAannot.tsv', sep='\t', header=T)
df_umcuData$response <- df_umcuData$response %>% toBinaryResponse(.,c('BRCA1','BRCA2'),'BRCA','none','none')

df_umcuData_whitelist <- df_umcuData %>% .[rownames(.) %in% donorWhitelistNames,]

#========= Set input =========#
## norm data
#df <- df_umcuNormData_whitelist
#df <- df_sangerNormData_whitelist

## un-normalized data
#df <- df_sangerData_whitelist
df <- df_umcuData_whitelist

## context data
# df <- df_umcuDataContextRel_whitelist
# df <- df_umcuDataContextRelNorm_whitelist


#%%%%%%%%% Logistic regression %%%%%%%%%####
# #========= CV and plots =========#
# set.seed(1) ## set.seed returns vector of integers; for reproducibility, run script starting from set.seed()
# 
# lrCV <- logRegCV(df, colname.response = 'response', positive.response = 'BRCA',
#                  standardize = T, balance = 'up')
# 
# ## Aggregate information
# agg_lambda <- aggregateLogRegCV(lrCV,'lambda.min')
# agg_coef <- aggregateLogRegCV(lrCV,'coef')
# 
# agg_pred <- aggregateLogRegCV(lrCV,'pred')
# agg_pred$response <- agg_pred$response %>% toBinaryResponse(.,'BRCA',1,'none',0)
# 
# ## Plot performance
# lrCV_summaryPlots <- list(
#    plot_logRegCoefs(agg_coef),
#    plot_performanceRates(agg_pred$BRCA, agg_pred$response, metrics = c('tpr','tnr'), show.intersection = T),
#    plot_ROC(agg_pred$BRCA, agg_pred$response),
#    plot_PR(agg_pred$BRCA, agg_pred$response)
# )
# 
# pdf(paste0(base_dir,'lrCV_summaryPlots.pdf'),10,10)
# grid.arrange(grobs = lrCV_summaryPlots, nrow = 2, ncol = 2)
# dev.off()
# 
# #========= Create and export final model =========#
# lrModelAndPred <- logRegTrainAndTest(train = df, test = df,
#                                      colname.response = 'response', positive.response = 'BRCA',
#                                      standardize = T, balance = 'up')
# lrModel <- lrModelAndPred$logReg
# lrPred <- lrModelAndPred$pred
# save(lrModel, file = paste0(base_dir,'lrModel.rda'))
# 
# ## Aggregate information
# final_lambda <- lrModel$lambda.min
# final_coef <- lrModel %>% coef()
# 
# final_pred <- lrPred
# final_pred$response <- final_pred$response %>% toBinaryResponse(.,'BRCA',1,'none',0)
# 
# ## Plot performance
# lrModel_summaryPlots <- list(
#    plot_logRegCoefs(final_coef),
#    plot_performanceRates(final_pred$prediction, final_pred$response, metrics = c('tpr','tnr'), show.intersection = T),
#    plot_ROC(final_pred$prediction, final_pred$response),
#    plot_PR(final_pred$prediction, final_pred$response)
# )
# 
# pdf(paste0(base_dir,'lrModel_summaryPlots.pdf'),10,10)
# grid.arrange(grobs = lrModel_summaryPlots, nrow = 2, ncol = 2)
# dev.off()




#%%%%%%%%% Random forest %%%%%%%%%####
#========= CV and plots =========#
set.seed(1) ## set.seed returns vector of integers; for reproducibility, run script starting from set.seed()
trainSamples <- sample(nrow(df)) %>% .[1:round(length(.)*0.8)]

df_train <- df[trainSamples,]
df_test <- df[-trainSamples,]

rfCV <- randomForestCV(df_train, 'response', balance = 'up', mtry = 6)

## Aggregate information
agg_pred <- aggregateRandomForestCV(rfCV, 'pred')
agg_pred$response <- toBinaryResponse(agg_pred$response,'BRCA',1,'none',0)
agg_imp <- aggregateRandomForestCV(rfCV, 'importance')

confusionMatrix(agg_pred$BRCA, agg_pred$response, cutoff = 0.5) 
#%>% misclassError()

## Plot performance
rfCV_summaryPlots <- list(
   plot_MeanDecreaseAccuracy(agg_imp),
   plot_performanceRates(agg_pred$BRCA, agg_pred$response, metrics = c('tpr','tnr'), show.intersection = T),
   plot_ROC(agg_pred$BRCA,agg_pred$response),
   plot_PR(agg_pred$BRCA,agg_pred$response)
)

# ## Export
# pdf(paste0(base_dir,'rfCV_summaryPlots.pdf'),10,10)
# grid.arrange(grobs = rfCV_summaryPlots, nrow = 2, ncol = 2)
# dev.off()

#=========
confusionMatrix <- function(probs.predicted, logicals.expected, cutoff = 0.5){
   
   df <- data.frame(probs.predicted, logicals.expected)
   
   tp <- sum(df$probs.predicted >= cutoff & df$logicals.expected == 1)
   tn <- sum(df$probs.predicted < cutoff & df$logicals.expected == 0)
   
   fp <- sum(df$probs.predicted >= cutoff & df$logicals.expected != 1)
   fn <- sum(df$probs.predicted < cutoff & df$logicals.expected != 0)
   
   c(tp=tp, tn=tn, fp=fp, fn=fn)
}

misclassError <- function(confusion.matrix){
   posError <- confusion.matrix['fp']/(confusion.matrix['tp'] + confusion.matrix['fp'] )
   negError <- confusion.matrix['fn']/(confusion.matrix['tn'] + confusion.matrix['fn'] )
   
   sum(posError, negError)
}

## Testing numerical params
param_values <- 1:20
cvError <- sapply(param_values, function(param){
   message(paste0('CV for param value: ', param))
   
   rf <- randomForestCV(df_train, 'response', balance = 'up', mtry = 4)
   
   ## cv predictions
   agg_pred <- aggregateRandomForestCV(rf, 'pred')
   agg_pred$response <- toBinaryResponse(agg_pred$response,'BRCA',TRUE,'none',FALSE) %>% as.logical()
   cvMissclassError <- confusionMatrix(agg_pred$BRCA, agg_pred$response, cutoff = 0.5) %>% misclassError()
   
   ## oob
   cvOob <- sapply(rf, function(i){ i$RF$err.rate[,'OOB'] }) %>% mean()

   c(
      cvMissclassError = cvMissclassError,
      cvOob = cvOob
   )
})
errorByParamValue <- data.frame(param_values = param_values, cvError = cvError['cvMissclassError',], cvOob = cvError['cvOob',])

error_plots <- list(
   ggplot( errorByParamValue, aes(x=param_values, y=cvError) ) + geom_point() + geom_line(),
   ggplot( errorByParamValue, aes(x=param_values, y=cvOob) ) + geom_point() + geom_line()
)
error_plots <- grid.arrange(grobs = error_plots, nrow = 1, ncol = 2)

pdf('/Users/lnguyen/hpc/cog_bioinf/cuppen/project_data/Luan_PCAGW/results/hr_detect/random_forest_training/param_optimization/nodesize_1-20.pdf',10,5)
plot(error_plots)
dev.off()

## Feature selection
features <- colnames(df_train) %>% .[. != 'response']

#Number of features to still include for each iteration
nFeatures <- length(features)
topnFeatures <- c()
while(nFeatures > 1){
   topnFeatures <- c(topnFeatures, nFeatures)
   nFeatures <- round(nFeatures*0.7)
}

featuresInclude <- list(features)

featSelCvOobError <- c()
featSelAggPred <- list()
featSelAggImp <- list()

for( i in 2:length(topnFeatures) ){
   message(paste0('Running RF CV with top ', topnFeatures[i-1], ' features...'))
   
   df_train_featuresInclude <- df_train[, colnames(df_train) %in% c(featuresInclude[[i-1]],'response') ]
   
   rf <- randomForestCV(df_train_featuresInclude, 'response', balance = 'up', mtry = 4, randomForest.ntree = 300)
   
   ## cv predictions
   agg_pred <- aggregateRandomForestCV(rf, 'pred')
   agg_pred$response <- toBinaryResponse(agg_pred$response,'BRCA',TRUE,'none',FALSE) %>% as.logical()
   
   
   featSelAggPred[[i-1]] <- agg_pred
   
   ## oob
   cvOobError <- sapply(rf, function(i){ i$RF$err.rate[,'OOB'] }) %>% mean()
   featSelCvOobError <- c(featSelCvOobError, cvOobError)
   
   ## importance
   agg_imp <- aggregateRandomForestCV(rf, 'importance') %>% .[order(.$MeanDecreaseAccuracy, decreasing = T),]
   featSelAggImp[[i-1]] <- agg_imp
   
   ## exclude features with lowest importance
   featuresInclude[[i]] <- agg_imp[1:topnFeatures[i],'feature']
}

featSelCvMissclassError <- lapply(featSelAggPred, function(i){
   confusionMatrix(i$BRCA, i$response, cutoff = 0.5) %>% misclassError()
}) 

iter <- 5
plot_PR(featSelAggPred[[iter]]$BRCA,featSelAggPred[[iter]]$response, auc.only = T)

# calcAUC <- function(probs.predicted, logicals.expected, metrics = c('tnr', 'tpr')){
#    performanceAsDf(featSelAggPred[[iter]]$BRCA,featSelAggPred[[iter]]$response, metrics = c('tpr','ppv'))
# }
# 
# 
# confusionMatrix(featSelAggPred[[iter]]$BRCA,featSelAggPred[[iter]]$response, 1.0)

errorByParamValue <- data.frame(param_values = param_values, cvError = cvError['cvMissclassError',], cvOob = cvError['cvOob',])


# rfModel_summaryPlots <- list(
#    plot_MeanDecreaseAccuracy(final_imp),
#    plot_performanceRates(final_pred$BRCA, final_pred$response, metrics = c('tpr','tnr'), show.intersection = T),
#    plot_ROC(final_pred$BRCA,final_pred$response),
#    plot_PR(final_pred$BRCA,final_pred$response)
# )


#========= Create and export final model =========#
rfModelAndPred <- randomForestTrainAndTest(df_train, df_test, colname.response = 'response', balance = T)

rfModel <- rfModelAndPred$RF
rfPred <- rfModelAndPred$pred
#save(rfModel, file = paste0(base_dir,'rfModel.rda'))

## Calculate final performance metrics on the original data set
final_imp <- importance(rfModel, type = 1) %>% as.data.frame()
final_imp$feature <- rownames(final_imp)

final_pred <- rfPred
final_pred$response <- df_test$response %>% toBinaryResponse(.,'BRCA',1,'none',0)


plot_PR(final_pred$BRCA,final_pred$response)

## Plot prob distribution
plot_probDistribution <- function(df, colname.pos.response, cutoff = NULL, title = ''){
   df <- df[,colname.pos.response] %>% order() %>% df[.,]
   df$index <- 1:nrow(df)

   df$response <- toBinaryResponse(df$response, '1', 'BRCA deficient', '0', 'BRCA proficient') %>% relevel(.,'BRCA deficient')


   plot <- ggplot(data = df, aes_string(x = 'index',
                                        y = colname.pos.response,
                                        colour = 'response') ) +
      geom_point(shape = 1) +
      geom_hline(yintercept = cutoff, linetype = 2) +

      ggtitle(title) +
      xlab('Patient rank') +
      ylab('P(BRCA)') +
      theme(plot.title = element_text(hjust = 0.5)) +

      scale_color_discrete(name = "Annotation") +

      annotate('text', x=0, y=cutoff,
               hjust = 0.2, vjust = -1,
               label=paste0('P = ',cutoff))

   return(plot)
}

plot_probDistribution(final_pred, colname.pos.response = 'BRCA', cutoff = 0.5, title = 'BRCA-EU (training set)')

# pdf('/Users/lnguyen/Dropbox/MCLS Master/Cancer genomics/Meetings/20180316_work_meeting/brcaDefDist_sortedScatter_brcaEu.pdf',8.5,4)
# plot_probDistribution(final_pred, colname.pos.response = 'BRCA', cutoff = 0.614, title = 'BRCA-EU (training set)')
# dev.off()


## Plot performance
rfModel_summaryPlots <- list(
   plot_MeanDecreaseAccuracy(final_imp),
   plot_performanceRates(final_pred$BRCA, final_pred$response, metrics = c('tpr','tnr'), show.intersection = T),
   plot_ROC(final_pred$BRCA,final_pred$response),
   plot_PR(final_pred$BRCA,final_pred$response)
)

# ## Export
# pdf(paste0(base_dir,'rfModel_summaryPlots.pdf'),10,10)
# grid.arrange(grobs = rfModel_summaryPlots, nrow = 2, ncol = 2)
# dev.off()