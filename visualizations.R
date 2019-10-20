library("sf")
library("ggplot2")
library("cowplot")
library("viridis")

# read data
files = list.files("figures/data", pattern = "*.RDS", full.names = TRUE)
files_names = list.files("figures/data", pattern = "*.RDS")
files_names = substr(files_names, 1, nchar(files_names) - 4)
i = 1
for(name in files_names) {
   assign(name, readRDS(files[i]))
   i = i + 1
}

# read polygons
wheat_polygon = st_read("data/vectors/wheat_polygon.gpkg")
rape_polygon = st_read("data/vectors/rape_polygon.gpkg")

# MAPS WITH PREDICTIONS
# 1. rape - random forest
p1 = ggplot(rapeRfPred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value [kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9))

# 2. rape - kriging
p2 = ggplot(rapeKrigePred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value [kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9))

# 3. rape - IDW
p3 = ggplot(rapeIdwPred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value [kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9))

# 4.wheat - random forest
p4 = ggplot(wheatRfPred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value [kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9))

# 5.wheat - krigin
p5 = ggplot(wheatKrigePred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value [kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9))

# 6. wheat - IDW
p6 = ggplot(wheatIdwPred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value [kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9))

# MAPS WITH RESIDUALS
# 1. rape - random forest 
p11 = ggplot(rapeRfSpErr) +
   geom_sf(data = rape_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training")) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   theme(legend.title = element_text(size = 9))

# 2. rape - kriging
p12 = ggplot(rapeKrigeSpErr) +
   geom_sf(data = rape_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training")) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   theme(legend.title = element_text(size = 9))

# 3. rape - IDW
p13 = ggplot(rapeIdwSpErr) +
   geom_sf(data = rape_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training")) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   theme(legend.title = element_text(size = 9))

# 4.wheat - random forest 
p14 = ggplot(wheatRfSpErr) +
   geom_sf(data = wheat_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training")) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   theme(legend.title = element_text(size = 9))

# 5.wheat - krigin
p15 = ggplot(wheatKrigeSpErr) +
   geom_sf(data = wheat_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training")) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   theme(legend.title = element_text(size = 9))

# 6. wheat - IDW
p16 = ggplot(wheatIdwSpErr) +
   geom_sf(data = wheat_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training")) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   theme(legend.title = element_text(size = 9))

# final plots
x1 = plot_grid(p1, p11, p2, p12, p3, p13, ncol = 2, nrow = 3)
x2 = plot_grid(p4, p14, p5, p15, p6, p16, ncol = 2, nrow = 3)
plot_grid(x1, x2, ncol = 2)


# TODO
# 1. Usunac powtorzenia legendy 'Set'
# 2. Wspolrzedne X i Y sprowadzic do tych samych jednostek
#  a) moze usunac na mapach predykcyjnych i zostawic na mapach bledow ("theme_map")
#  b) usunac na mapach bledow i zostawic tylko na predykcyjncyh w ukladzie metrycznym
# 3. Dodac opis w kolumnach (tj. rzepak/pszenica) i w wierszach (tj. RF/IDW/KRIGE)