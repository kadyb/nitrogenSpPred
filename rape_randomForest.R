library("sf")
library("mlr")
library("ranger")
library("raster")
source("functions/dist.R")


# read data
data = readRDS("data/nitrogen_rape.RDS")
rape_points = st_read("data/vectors/rape_points.gpkg")
rape_polygon = st_read("data/vectors/rape_polygon.gpkg")
rape_points$field_1 = as.numeric(levels(rape_points$field_1))[rape_points$field_1]

# mineral nitrogen [mg/dm^3]
data$min = data$NH4 + data$NO3
# mineral nitrogen [kg/ha]
data$min = data$min * 5 * 4.5

# mineral nitrogen content in the entire soil profile (total from 0 to 90 cm)
Nmin_sum =  aggregate(data$min, by = list(data$pkt), FUN = sum)[, 2]
data = data.frame(pkt = 1:60, Nmin_sum = Nmin_sum)

# combine spatial and numerical data
data = merge(rape_points, data, by.x = "field_1", by.y = "pkt")
colnames(data)[1] = "pkt"
data = data[, -(2:4)]
if (any(data$pkt != 1:60)) stop("Invalid order of vector objects")

# read satellite data (spatial resolution 20 m)
s2img_list = list.files("data/rasters/satellite_rape", pattern = ".tif$", full.names = TRUE)
s2img_stack = stack(s2img_list)

# read DEM and create geomorphological variables
DEM = raster("data/rasters/DEM_rape.tif")
slope = terrain(DEM, "slope", unit = "degrees")
aspect = terrain(DEM, "aspect", unit = "radians")
TPI = terrain(DEM, "TPI")
TRI = terrain(DEM, "TRI")
roughness = terrain(DEM, "roughness")
northness = cos(aspect)
eastness = sin(aspect)

morfo_stack = stack(DEM, slope, TPI, TRI, roughness, northness, eastness)
morfo_stack = mask(morfo_stack, s2img_stack[[1]])
names(morfo_stack) = c("DEM", "slope", "TPI", "TRI", "roughness", "northness", "eastness")

# create X and Y coordinates rasters
x_vals = matrix(xFromCell(s2img_stack, c(1:ncell(s2img_stack))), nrow(s2img_stack), byrow = TRUE)
y_vals = matrix(yFromCell(s2img_stack, c(1:ncell(s2img_stack))), nrow(s2img_stack), byrow = TRUE)
x_ras = raster(s2img_stack[[1]])
x_ras = setValues(x_ras, x_vals)
y_ras = raster(s2img_stack[[1]])
y_ras = setValues(y_ras, y_vals)

pos_stack = stack(x_ras, y_ras)
names(pos_stack) = c("coord_x", ("coord_y"))
pos_stack = mask(pos_stack, s2img_stack[[1]])

# create distnace rasters
dist_stack = dist(data, morfo_stack[["DEM"]], "XYZ")

# create a data frame with all input data
dist = extract(dist_stack, data, df = TRUE)
colnames(dist)[1] = "pkt"
data = merge(data, dist, by = "pkt")

coords = extract(pos_stack, data, df = TRUE)
colnames(coords)[1] = "pkt"
data = merge(data, coords, by = "pkt")

spectral = extract(s2img_stack, data, df = TRUE)
colnames(spectral)[1] = "pkt"
data = merge(data, spectral, by = "pkt")

morfo = extract(morfo_stack, data, df = TRUE)
colnames(morfo)[1] = "pkt"
data = merge(data, morfo, by = "pkt")

# prepare data frame
data_mdl = data[-1]
st_geometry(data_mdl) = NULL

# split data to test and train
idx = c(2, 6, 14, 19, 25, 28, 41, 45, 48, 56)
trainset = data_mdl[-idx, ]
testset = data_mdl[idx, ]

# define regression task
regr_task = makeRegrTask(data = trainset, 
                         target = "Nmin_sum", 
                         coordinates = as.data.frame(st_coordinates(data[-idx, ])))
# select model
learner = makeLearner("regr.ranger", 
                      importance = "impurity", 
                      num.threads = 3)
# define hyperparametrs search space
par_set = makeParamSet(
  makeIntegerParam("num.trees", lower = 50L, upper = 500L),
  makeIntegerParam("mtry", lower = 3L, upper = 8L),
  makeIntegerParam("min.node.size", lower = 4L, upper = 15L)
)
# 10-fold spatial cross-validation repeated 4 times
rdesc = makeResampleDesc("SpRepCV", folds = 10, reps = 4, predict = "both")
# root mean square error as performance measure
meas = rmse
# choose tuning method
ctrl = makeTuneControlRandom(maxit = 120)
# model tuning
set.seed(2)
tuned_model = tuneParams(learner = learner,
                         task = regr_task,
                         resampling = rdesc,
                         measures = list(meas, setAggregation(meas, train.mean)),
                         par.set = par_set,
                         control = ctrl,
                         show.info = FALSE)
tuned_model

# set best hyperparametrs and train model
set.seed(2)
model_rf = setHyperPars(learner = learner, par.vals = tuned_model$x)
model_rf = train(learner = model_rf, task = regr_task)

# variable importance
sort(getFeatureImportance(model_rf)$res, decreasing = TRUE)[1:15]

# resample data
resample_data = generateHyperParsEffectData(tuned_model, partial.dep = TRUE)$data


# stack all rasters for prediction
all_rasters = stack(dist_stack, pos_stack, s2img_stack, morfo_stack)

# transform rasters do data frame
newdata = as.data.frame(as.matrix(all_rasters))
complete_data = complete.cases(newdata)

# predict
mdl = predict(model_rf, newdata = na.omit(newdata)) 

# prepare empty vector to complete with predicted data
output = rep(NA, nrow(newdata))
output[complete_data] = mdl$data$response
pred = s2img_stack[[1]]
pred = setValues(pred, output)

spplot(pred)
#writeRaster(pred, "rape_results.tiff")


# check results on testset
results_test = raster::extract(pred, data[idx, ])
summary(results_test - data$Nmin_sum[idx])
measureRSQ(data$Nmin_sum[idx], results_test)
measureMAPE(data$Nmin_sum[idx], results_test)*100
measureRMSE(data$Nmin_sum[idx], results_test)
# Results:
# R^2 = 0.30  MAPE = 38.60%  RMSE = 24.12

# create spatial df with predictions
pred_df <- as(pred, "SpatialPixelsDataFrame")
pred_df <- as.data.frame(pred_df)
colnames(pred_df)[1] = "value"

# create spatial df with residuals
results_train = raster::extract(pred, data[-idx, ])

df_results = data.frame(val = results_test - data$Nmin_sum[idx], 
                        set = "test", Nmin_sum = data$Nmin_sum[idx])
df_results = rbind(df_results, 
                   data.frame(val = results_train - data$Nmin_sum[-idx], 
                              set = "train", Nmin_sum = data$Nmin_sum[-idx]))

spatialDiff = merge(data[, 1:2], df_results, by = "Nmin_sum")
spatialDiff = spatialDiff[, -1]

# save
saveRDS(pred_df, "figures/data/rapeSpPred.RDS")
saveRDS(spatialDiff, "figures/data/rapeRfSpErr.RDS")