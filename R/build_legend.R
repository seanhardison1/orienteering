
library(ggplot2)

# If you want to draw arbitrary rectangles, use geom_tile() or geom_rect()
df <- data.frame(
  x = c(1,2,3,4),
  y = c(1,2,3,4),
  z = factor(c(1,2,3,4)))

leg <- ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = z)) +
  geom_line(aes(x,y, color = "black")) +
  geom_line(aes(x,y, color = "blue")) +
  geom_line(aes(x,y,color = "#654321BF"), linetype = "41") +
  geom_line(aes(x,y,color = "red")) +
  scale_fill_manual(name = "Legend",
                      values = c("#77BF72CC", 
                                 "#bfa772CC",
                                 "#bfa772CC",
                                 "#ff1307CC"),
                      guide = "legend",
                      label = c("Mixed Forest",
                                "Private Land",
                                "Meadow",
                                "Scrubland") ) +
  scale_colour_manual(name = '', 
                      values = c("black",
                                 "blue",
                                 "#654321BF",
                                 "red"),
                      labels = c("Elevation (ft.)",
                                 "Trail",
                                 "Railway",
                                 "Power line")) +
  theme(legend.key.width = unit(1.5,"cm"))+
  guides(color = guide_legend(override.aes = list(size = c(1,1,2,1),
                                                  linetype = c("solid",
                                                               "solid",
                                                               "41",
                                                               "solid")
                                                  )
                              )
         ) +
  theme(legend.key = element_rect(colour = NA, fill = NA)) 


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
