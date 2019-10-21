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
   labs(fill = "Value\n[kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# 2. rape - kriging
p2 = ggplot(rapeKrigePred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value\n[kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# 3. rape - IDW
p3 = ggplot(rapeIdwPred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value\n[kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# 4.wheat - random forest
p4 = ggplot(wheatRfPred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value\n[kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# 5.wheat - krigin
p5 = ggplot(wheatKrigePred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value\n[kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# 6. wheat - IDW
p6 = ggplot(wheatIdwPred) +  
   geom_tile(aes(x = x, y = y, fill = value)) +
   scale_fill_viridis() +
   coord_equal() +
   labs(fill = "Value\n[kg/ha]") +
   theme_light() +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# MAPS WITH RESIDUALS
# 1. rape - random forest 
p11 = ggplot(rapeRfSpErr) +
   geom_sf(data = rape_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   coord_sf(datum = sf::st_crs(2180)) +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training"),
                      guide = FALSE) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   scale_y_continuous(breaks = c(376000, 375750, 375500, 375250), 
                      labels = c(376000, 375750, 375500, 375250)) +
   scale_x_continuous(breaks = c(265500, 266000, 266500), 
                      labels = c(265500, 266000, 266500)) +
   theme_light() +
   theme(legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))
   

# 2. rape - kriging
p12 = ggplot(rapeKrigeSpErr) +
   geom_sf(data = rape_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   coord_sf(datum = sf::st_crs(2180)) +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training"),
                      guide = FALSE) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   scale_y_continuous(breaks = c(376000, 375750, 375500, 375250), 
                      labels = c(376000, 375750, 375500, 375250)) +
   scale_x_continuous(breaks = c(265500, 266000, 266500), 
                      labels = c(265500, 266000, 266500)) +
   theme_light() +
   theme(legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# 3. rape - IDW
p13 = ggplot(rapeIdwSpErr) +
   geom_sf(data = rape_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   coord_sf(datum = sf::st_crs(2180)) +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training"),
                      guide = FALSE) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   scale_y_continuous(breaks = c(376000, 375750, 375500, 375250), 
                      labels = c(376000, 375750, 375500, 375250)) +
   scale_x_continuous(breaks = c(265500, 266000, 266500), 
                      labels = c(265500, 266000, 266500)) +
   theme_light() +
   theme(legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# 4.wheat - random forest 
p14 = ggplot(wheatRfSpErr) +
   geom_sf(data = wheat_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   coord_sf(datum = sf::st_crs(2180)) +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training"),
                      guide = FALSE) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   scale_y_continuous(breaks = c(376200, 376000, 375800, 375600), 
                      labels = c(376200, 376000, 375800, 375600)) +
   scale_x_continuous(breaks = c(268000, 268400, 268800, 269200), 
                      labels = c(268000, 268400, 268800, 269200)) +
   theme_light() +
   theme(legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# 5.wheat - krigin
p15 = ggplot(wheatKrigeSpErr) +
   geom_sf(data = wheat_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   coord_sf(datum = sf::st_crs(2180)) +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training"),
                      guide = FALSE) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   scale_y_continuous(breaks = c(376200, 376000, 375800, 375600), 
                      labels = c(376200, 376000, 375800, 375600)) +
   scale_x_continuous(breaks = c(268000, 268400, 268800, 269200), 
                      labels = c(268000, 268400, 268800, 269200)) +
   theme_light() +
   theme(legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# 6. wheat - IDW
p16 = ggplot(wheatIdwSpErr) +
   geom_sf(data = wheat_polygon, fill = NA) +
   geom_sf(aes(color = val, size = abs(val), shape = set), show.legend = "point") +
   coord_sf(datum = sf::st_crs(2180)) +
   scale_color_gradient2(midpoint = 0, low = "#ca0020", 
                         mid = "white", high = "#0571b0") +
   scale_size_continuous(guide = FALSE) +
   scale_shape_manual(values = c(16, 17), labels = c("Test", "Training"), 
                      guide = FALSE) +
   labs(color = "Difference\n[kg/ha]", shape = "Set") +
   scale_y_continuous(breaks = c(376200, 376000, 375800, 375600), 
                      labels = c(376200, 376000, 375800, 375600)) +
   scale_x_continuous(breaks = c(268000, 268400, 268800, 269200), 
                      labels = c(268000, 268400, 268800, 269200)) +
   theme_light() +
   theme(legend.title = element_text(size = 9),
         axis.text.x = element_text(angle = 10, hjust = 1))

# legend
leg = get_legend(ggplot(wheatIdwSpErr) +
                    geom_sf(aes(shape = set), show.legend = "point") +
                    scale_shape_manual(values = c(16, 17), 
                                       labels = c("Test", "Training")) +
                    labs(shape = "Set") +
                    theme(legend.position = "bottom",
                          legend.title = element_text(face = "bold")))

# final plot
g = plot_grid(p1, p11, p4, p14,
              p2, p12, p5, p15,
              p3, p13, p6, p16,
              ncol = 4, nrow = 3, labels = "AUTO")
plot_grid(g, leg, nrow = 2, rel_heights = c(1, 0.05))

