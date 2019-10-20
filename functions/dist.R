dist = function(points, raster, method) {
   library("sf")
   library("raster")
   
   if(!(method %in% c("XY", "XYZ"))) stop("Incorrect method. Select 'XY' or 'XYZ'.")
   
   P_list = st_coordinates(points)
   
   ras_list = list() # initialize list to store output
   
   x_vec = seq(1, nrow(raster))
   y_vec = seq(1, ncol(raster))
   z_vec = getValues(raster)
   
   if(method == "XY" && anyNA(z_vec)) {
      mask = is.na(z_vec)
   }
   
   for(p in 1:nrow(P_list)) {
      P_col = colFromX(raster, P_list[p, 1])
      P_row = rowFromY(raster, P_list[p, 2])
      
      # reference all coordinates to input point
      P_x = (P_row - x_vec) * xres(raster)
      P_y = (P_col - y_vec) * yres(raster)
      P_z = raster[P_row, P_col] - z_vec
      
      grd = expand.grid(Y = P_y, X = P_x)
      dist = sqrt(grd$X^2 + grd$Y^2) # dist XY
      if(exists("mask", inherits = FALSE)) {
         dist[mask] = NA
      }
      
      if(method == "XYZ") {
         dist = sqrt(grd$X^2 + grd$Y^2 + P_z^2) # dist XYZ
      }
      
      ras_dist = setValues(raster, dist)
      names(ras_dist) = paste0("P", p, "_", "dist", method)
      ras_list = append(ras_list, ras_dist)
   }
   
   return(stack(ras_list))
}

# Info:
### Calculate the XY(Z) Euclidean distance between vector points.
### If you want to include the terrain elevation, use DTM. 
### Otherwise, only raster with area is enough.
# Input:
### points - sf point layer
### raster - raster in the metric coordinate system
### method - "XY" or "XYZ" ("XY" with elevation)

# Example:
# p = st_read("points.gpkg")
# r = raster("DTM.tif")
# dist_XYZ = dist(p, r, "XYZ")
# plot(dist_XYZ[[1:5]])

