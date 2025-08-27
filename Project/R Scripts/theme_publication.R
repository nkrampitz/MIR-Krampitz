

theme_Publication <- function(base_size=14, base_family="Calibri") {
  # base_family="Calibri" could be added above in function set up...
  # and below, next to base_size = base_size
  library(grid)
  library(ggthemes)
  library(extrafont)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(face = "bold",
            size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            panel.spacing.x = unit(8, "mm"),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = "grey50", size = 1),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line.x = element_line(colour="grey50", size = 1),
            axis.line.y = element_line(colour="grey50", size = 1),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_line(colour="#f0f0f0"),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.5, "cm"),
            legend.margin = margin(0,0,0,0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_blank(),
            strip.text = element_text(face="plain")
    ))

}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

scale_fill_Publication_BW <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("grey30","grey80","white")), ...)

}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}
