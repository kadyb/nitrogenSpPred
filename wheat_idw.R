library("sf")
library("mlr")
library("dismo")
library("gstat")
library("stars")
library("raster")


# read data
data = readRDS("data/nitrogen_wheat.RDS")
wheat_points = st_read("data/vectors/wheat_points.gpkg")
wheat_polygon = st_read("data/vectors/wheat_polygon.gpkg")
wheat_points$field_1 = as.numeric(levels(wheat_points$field_1))[wheat_points$field_1]

# grid
grid = read_stars("data/rasters/gridWheat.tif")

# mineral nitrogen [mg/dm^3]
data$min = data$NH4 + data$NO3
# mineral nitrogen [kg/ha]
data$min = data$min * 5 * 4.5

# mineral nitrogen content in the entire soil profile (total from 0 to 90 cm)
Nmin_sum =  aggregate(data$min, by = list(data$pkt), FUN = sum)[, 2]
data = data.frame(pkt = 1:60, Nmin_sum = Nmin_sum)

# combine spatial and numerical data
data = merge(wheat_points, data, by.x = "field_1", by.y = "pkt")
colnames(data)[1] = "pkt"
data = data[, -(2:3)]

# split data to test and train
idx = c(2, 5, 9, 20, 25, 34, 38, 40, 52, 57)
testset = data[idx, ]
trainset = data[-idx, ]

# tuning (10 fold cross-validation)
tune_idp = seq(1, 3, by = 0.25)
tune_nmax = seq(5, nrow(trainset) - 5, by = 5)
tuning_grid = expand.grid(nmax = tune_nmax, idp = tune_idp)
vRMSE = vector()
foldRMSE = vector()
set.seed(5)
folds = kfold(trainset, k = 10)

for(i in 1:nrow(tuning_grid)) {
   for(n in 1:length(unique(folds))) {
      foldTest = trainset[folds == n, ]
      foldTrain = trainset[folds != n, ]
      idw = idw(Nmin_sum~1, foldTrain, newdata = foldTest,
                nmax = tuning_grid$nmax[i], idp = tuning_grid$idp[i])
      foldRMSE = append(foldRMSE, measureRMSE(foldTest$Nmin_sum, idw$var1.pred))
   }
   vRMSE = append(vRMSE, mean(foldRMSE))
}


# select best hyperparametrs
df_tuned = cbind(tuning_grid, vRMSE)
nmax = df_tuned[which.min(df_tuned$vRMSE), 1]
idp = df_tuned[which.min(df_tuned$vRMSE), 2]

# train model on all train data
idw = idw(Nmin_sum~1,
          locations = trainset,
          newdata = testset,
          nmax = nmax,
          idp = idp)

# testset validation
measureRSQ(data$Nmin_sum[idx], idw$var1.pred)
measureMAPE(data$Nmin_sum[idx], idw$var1.pred)*100
measureRMSE(data$Nmin_sum[idx], idw$var1.pred)
# Results:
# R^2 = 0.59  MAPE = 17.51%  RMSE = 22.36


# final idw
idwFinal = idw(Nmin_sum~1,
               locations = trainset,
               newdata = grid,
               nmax = nmax,
               idp = idp)

plot(idwFinal["var1.pred"])


pred = raster("data/rasters/gridWheat.tif")
pred = setValues(pred, as.vector(idwFinal$var1.pred))
spplot(pred)

# create spatial df with predictions
pred_df <- as(pred, "SpatialPixelsDataFrame")
pred_df <- as.data.frame(pred_df)
colnames(pred_df)[1] = "value"

# create spatial df with residuals
results_test = idw$var1.pred
results_train = raster::extract(pred, data[-idx, ])

df_results = data.frame(val = results_test - data$Nmin_sum[idx], 
                        set = "test", Nmin_sum = data$Nmin_sum[idx])
df_results = rbind(df_results, 
                   data.frame(val = results_train - data$Nmin_sum[-idx], 
                              set = "train", Nmin_sum = data$Nmin_sum[-idx]))

spatialDiff = merge(data[, 1:2], df_results, by = "Nmin_sum")
spatialDiff = spatialDiff[, -1]

# save
saveRDS(pred_df, "figures/data/wheatIdwPred.RDS")
saveRDS(spatialDiff, "figures/data/wheatIdwSpErr.RDS")
