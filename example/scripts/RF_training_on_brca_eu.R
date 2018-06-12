library(mltoolkit)


#========= Paths =========#
base_dir <- '/Users/lnguyen/hpc/cog_bioinf/cuppen/project_data/Luan_PCAGW/scripts/mltoolkit/example/'

in_data_path <- paste0(base_dir,'/','data/brca_eu_data.tsv')
model_out_path <- paste0(base_dir,'/','data/RF_BRCA_EU.rds')
plots_dir <- paste0(base_dir,'/','plots/')


#========= Input data frame =========#
df <- read.table(in_data_path, check.names = F, stringsAsFactors = T)


#========= Cross-validation =========#
rf_seed <- 1234
set.seed(rf_seed)

rfCV <- randomForestCV(df = df, 
                       colname.response = 'response', 
                       stratify = T, 
                       balance = T)

## Aggregate information
rf_agg_pred <- aggregateRandomForestCV(rfCV, 'pred')
rf_agg_pred$response <- toBinaryResponse(rf_agg_pred$response,'BRCA',1,'none',0)

rf_agg_imp <- aggregateRandomForestCV(rfCV, 'mdg')


#========= Create and export final model =========#
set.seed(rf_seed)
rfModelAndPred <- randomForestTrainAndTest(train = df, 
                                           colname.response = 'response', 
                                           balance = T)

rfModel <- rfModelAndPred$RF

rfModel$cutoff <- plot_performanceRates(rf_agg_pred$BRCA, rf_agg_pred$response, metrics = c('tpr','tnr'), intersection.only = T)
rfModel$seed <- rf_seed
saveRDS(rfModel, file = model_out_path)


#========= Performance plots =========#
## Calculate final performance metrics on the original data set
rf_final_imp <- importance(rfModel, type = 2) %>% as.data.frame()
rf_final_imp$feature <- rownames(rf_final_imp)


rf_final_imp$feature <- rownames(rf_final_imp)
cv_ranked_feature_names <- plot_featureImportance(rf_agg_imp, export.features = T)
final_feature_imp <- sapply(cv_ranked_feature_names, function(i){ rf_final_imp[rf_final_imp$feature == i, 1] })

## Create plots
rfCV_summaryPlots <- list(
   mdg = plot_featureImportance(
      rf_agg_imp, 
      final.model.imp = final_feature_imp),
   
   roc = plot_ROC(
      rf_agg_pred$BRCA,
      rf_agg_pred$response),
   
   pr = plot_PR(
      rf_agg_pred$BRCA,
      rf_agg_pred$response),
   
   tpr_tnr = plot_performanceRates(
      rf_agg_pred$BRCA, 
      rf_agg_pred$response, 
      metrics = c('tpr','tnr'), 
      show.intersection = T),
   
   sortedProbs = plot_sortedPredictedProbs(
      probs.predicted = rf_agg_pred$BRCA,
      logicals.expected = rf_agg_pred$response,
      annotations = c('BRCA deficient', 'BRCA proficient'),
      cutoff = rfModel$cutoff, show.confusion = T, title = 'Random forest')
)

## Export plots
# Feature importance
pdf(paste0(plots_dir,'cv_featureImportance.pdf'),4,5.2)
plot(rfCV_summaryPlots$mdg)
dev.off()

# Precision recall
pdf(paste0(plots_dir,'cv_precisionRecall.pdf'),4,4)
rfCV_summaryPlots$pr
dev.off()

# ROC
pdf(paste0(plots_dir,'cv_ROC.pdf'),4,4)
rfCV_summaryPlots$roc
dev.off()

# tpr tnr
pdf(paste0(plots_dir,'cv_truePos_trueNeg_rate.pdf'),5,4)
rfCV_summaryPlots$tpr_tnr
dev.off()

# Sorted probs
pdf(paste0(plots_dir,'cv_sortedPredictionProbs.pdf'),6,4)
rfCV_summaryPlots$sortedProbs 
dev.off()