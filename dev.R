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
#========= CV and plots =========#
set.seed(1) ## set.seed returns vector of integers; for reproducibility, run script starting from set.seed()

lrCV <- logRegCV(df, colname.response = 'response', positive.response = 'BRCA', 
                 standardize = T, balance = 'up')

## Aggregate information
agg_lambda <- aggregateLogRegCV(lrCV,'lambda.min')
agg_coef <- aggregateLogRegCV(lrCV,'coef')

agg_pred <- aggregateLogRegCV(lrCV,'pred')
agg_pred$response <- agg_pred$response %>% toBinaryResponse(.,'BRCA',1,'none',0)

## Plot performance
lrCV_summaryPlots <- list(
   plot_logRegCoefs(agg_coef),
   plot_performanceRates(agg_pred$BRCA, agg_pred$response, metrics = c('tpr','tnr'), show.intersection = T),
   plot_ROC(agg_pred$BRCA, agg_pred$response),
   plot_PR(agg_pred$BRCA, agg_pred$response)
)

pdf(paste0(base_dir,'lrCV_summaryPlots.pdf'),10,10)
grid.arrange(grobs = lrCV_summaryPlots, nrow = 2, ncol = 2)
dev.off()

#========= Create and export final model =========#
lrModelAndPred <- logRegTrainAndTest(train = df, test = df,
                                     colname.response = 'response', positive.response = 'BRCA',
                                     standardize = T, balance = 'up')
lrModel <- lrModelAndPred$logReg
lrPred <- lrModelAndPred$pred
save(lrModel, file = paste0(base_dir,'lrModel.rda'))

## Aggregate information
final_lambda <- lrModel$lambda.min
final_coef <- lrModel %>% coef()

final_pred <- lrPred
final_pred$response <- final_pred$response %>% toBinaryResponse(.,'BRCA',1,'none',0)

## Plot performance
lrModel_summaryPlots <- list(
   plot_logRegCoefs(final_coef),
   plot_performanceRates(final_pred$prediction, final_pred$response, metrics = c('tpr','tnr'), show.intersection = T),
   plot_ROC(final_pred$prediction, final_pred$response),
   plot_PR(final_pred$prediction, final_pred$response)
)

pdf(paste0(base_dir,'lrModel_summaryPlots.pdf'),10,10)
grid.arrange(grobs = lrModel_summaryPlots, nrow = 2, ncol = 2)
dev.off()




#%%%%%%%%% Random forest %%%%%%%%%####
#========= CV and plots =========#
set.seed(1) ## set.seed returns vector of integers; for reproducibility, run script starting from set.seed()

rfCV <- randomForestCV(df, 'response', balance = 'up')

## Aggregate information
agg_pred <- aggregateRandomForestCV(rfCV, 'pred')
agg_pred$response <- toBinaryResponse(agg_pred$response,'BRCA',1,'none',0)
agg_imp <- aggregateRandomForestCV(rfCV, 'importance')

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

#========= Create and export final model =========#

rfModelAndPred <- randomForestTrainAndTest(df, df, colname.response = 'response', balance = T)

rfModel <- rfModelAndPred$RF
rfPred <- rfModelAndPred$pred
#save(rfModel, file = paste0(base_dir,'rfModel.rda'))

## Calculate final performance metrics on the original data set
final_imp <- importance(rfModel, type = 1) %>% as.data.frame()
final_imp$feature <- rownames(final_imp)

final_pred <- rfPred
final_pred$response <- df$response %>% toBinaryResponse(.,'BRCA',1,'none',0)

## Plot prob distribution
# plot_probDistribution <- function(df, title = ''){
#    df <- df %>% .[order(.$BRCA),]
#    df$index <- 1:nrow(df)
#    
#    ggplot(data = df, aes(x = index, y = BRCA)) + 
#       geom_point() + 
#       ggtitle(title)
# }
# plot_probDistribution(final_pred, 'BRCA-EU')

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