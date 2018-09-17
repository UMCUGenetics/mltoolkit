library(mltoolkit)

base_dir <- '/Users/lnguyen/hpc/cog_bioinf/cuppen/project_data/Luan_PCAGW/scripts/mltoolkit/example/'
model_out_path <- paste0(base_dir,'/','data/RF_BRCA_EU.rds')

rf_model <- readRDS(model_out_path)

predict(object = rf_model, 
        newdata = m, 
        type = "prob")

#randomForestTrainAndTest()