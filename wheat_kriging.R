library("sf")
library("mlr")
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


# variogram modeling
# note that data are logarithmized
vario = variogram(log(Nmin_sum)~1, locations = trainset, cutoff = 450, width = 45)
plot(vario)

vario_map = variogram(log(Nmin_sum)~1, locations = data, cutoff = 450, width = 60, map = TRUE)
plot(vario_map)

# model 1
model1 = vgm(psill = 0.24, model = "Sph", range = 90)
plot(vario, model = model1)

# model 2
model2 = vgm(psill = 0.25, model = "Bes", range = 20)
plot(vario, model = model2)

# tuning
tune_nmax = 3:12
ok1RMSE = vector()
ok2RMSE = vector()

set.seed(5)
for (i in tune_nmax) {
   ok1 = krige.cv(Nmin_sum~1,
                  locations = trainset,
                  model = model1,
                  nfold = 10,
                  nmax = i)
   ok2 = krige.cv(Nmin_sum~1,
                  locations = trainset,
                  model = model2,
                  nfold = 10,
                  nmax = i)
   ok1RMSE = append(ok1RMSE, measureRMSE(ok1$observed, ok1$var1.pred))
   ok2RMSE = append(ok2RMSE, measureRMSE(ok2$observed, ok2$var1.pred))
}

# select best hyperparametr and model
df_tuned = data.frame(nmax = tune_nmax, ok1RMSE = ok1RMSE, ok2RMSE = ok2RMSE)
if (min(df_tuned$ok1RMSE) > min(df_tuned$ok2RMSE)) {
   sel_model = model2
   nmax = df_tuned[which.min(df_tuned$ok2RMSE), 1]
} else {
   sel_model = model1
   nmax = df_tuned[which.min(df_tuned$ok1RMSE), 1]
}

# train model on all train data
ok = krige(Nmin_sum~1,
           locations = trainset,
           newdata = testset,
           model = sel_model,
           nmax = nmax)

# testset validation
measureRSQ(data$Nmin_sum[idx], ok$var1.pred)
measureMAPE(data$Nmin_sum[idx], ok$var1.pred)*100
measureRMSE(data$Nmin_sum[idx], ok$var1.pred)
# Results:
# R^2 = 0.71  MAPE = 14.42%  RMSE = 18.86

# final kriging
okFinal = krige(Nmin_sum~1,
                locations = trainset,
                newdata = grid,
                model = sel_model,
                nmax = nmax)

plot(okFinal["var1.pred"])
plot(okFinal["var1.var"])


pred = raster("data/rasters/gridWheat.tif")
pred = setValues(pred, as.vector(okFinal$var1.pred))
spplot(pred)

# create spatial df with predictions
pred_df <- as(pred, "SpatialPixelsDataFrame")
pred_df <- as.data.frame(pred_df)
colnames(pred_df)[1] = "value"

# create spatial df with residuals
results_test = ok$var1.pred
results_train = raster::extract(pred, data[-idx, ])

df_results = data.frame(val = results_test - data$Nmin_sum[idx], 
                        set = "test", Nmin_sum = data$Nmin_sum[idx])
df_results = rbind(df_results, 
                   data.frame(val = results_train - data$Nmin_sum[-idx], 
                              set = "train", Nmin_sum = data$Nmin_sum[-idx]))

spatialDiff = merge(data[, 1:2], df_results, by = "Nmin_sum")
spatialDiff = spatialDiff[, -1]

# save
saveRDS(pred_df, "figures/data/wheatKrigePred.RDS")
saveRDS(spatialDiff, "figures/data/wheatKrigeSpErr.RDS")