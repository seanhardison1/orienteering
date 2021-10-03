library(ggplot2)

# If you want to draw arbitrary rectangles, use geom_tile() or geom_rect()
df <- data.frame(
  x = c(1,2,3,4,5),
  y = c(1,2,3,4,5),
  z = factor(c(1,2,3,4,5)))

neighb_col <- "#960f0fA3"
# forest_col <- "#77BF7266"
forest_col <- "#4E4F615e"
# forest_col <- "#5E5E5E5e"
# open_col <- "#a9bf7266"
open_col <- "#493D2A40"
res_col <- "grey20"
# grav_col <- "grey60"
grav_col <- "#645145"
# water_col <- "lightblue"
water_col <- "#7A0C0D"
# bound_col <- "#5B202A"
bound_col <- "#ff5900"
tert_col <- "#000000FF"
stripe_col <- "#faebd780"
# trail_col <- "blue"
# trail_col <- "#E65100"
trail_col <- "#8d21ff"
# pl_col <- "indianred"
pl_col <- "#503435"


leg <- ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = z)) +
  geom_line(aes(x,y, color = "black")) +
  geom_line(aes(x,y, color = "blue")) +
  geom_line(aes(x,y,color = "#654321BF"), linetype = "41") +
  geom_line(aes(x,y,color = "red")) +
  geom_line(aes(x,y,color = "p")) +
  scale_fill_manual(name = "Legend",
                    values = c(neighb_col, 
                               forest_col,
                               open_col,
                               water_col,
                               "black"),
                    guide = "legend",
                    label = c("Neighborhood",
                              "Forest",
                              "Cleared land",
                              "Water",
                              "f") ) +
  scale_colour_manual(name = '', 
                      values = c("black",
                                 bound_col,
                                 trail_col,
                                 pl_col,
                                 grav_col),
                      labels = c("Elevation (ft.)",
                                 "Park Boundary",
                                 "Trail",
                                 "Power line",
                                 "Gravel/dirt road")) +
  theme(legend.key.width = unit(1.5,"cm"))+
  guides(color = guide_legend(override.aes = list(size = c(1,1,2,1,1),
                                                  linetype = c("solid",
                                                               "solid",
                                                               "41",
                                                               "solid",
                                                               "solid")
  )
  )
  ) +
  theme(legend.key = element_rect(colour = NA, fill = NA)) 
leg
ggsave(leg,
       device = "pdf",
       filename = file.path("pdf/legend.pdf"),
       dpi = 400,
       bg = "transparent")


leg2 <- ggplot() +
  geom_point(data  = df,
             aes(x = x,
                 y = y, color = "#eb42f4"), shape = 1,
             size = 2, stroke = 1.5) +
  geom_point(data  = df,
             aes(x = x,
                 y = y, color = "red"), shape = 1,
             size = 2, stroke = 1.5) +
  geom_point(data  = df,
             aes(x = x,
                 y = y, color = "blue"), shape = 1,
             size = 2, stroke = 1.5) +
  scale_color_manual(name = '', 
                     values = c("#ffeb7c",
                                "red",
                                "blue"),
                     labels = c("Elf",
                                "Dwarf",
                                "Human")) +
  theme(legend.key = element_rect(colour = NA, fill = NA)) 

ggsave(leg2,
       device = "pdf",
       filename = file.path("pdf/legend2.pdf"),
       dpi = 500,
       bg = "transparent")
