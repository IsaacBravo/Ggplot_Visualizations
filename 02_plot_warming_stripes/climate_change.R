remotes::install_github("cortinah/hockeystick")



warming_stripes()

x <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"


GLB_Ts_dSST <- read_csv("data/GLB.Ts+dSST.csv")

# Convert Year variable to Date class
GLB_Ts_dSST$Year <- as.Date(paste0(GLB_Ts_dSST$Year, "-01-01"))

# Convert J-D variable to numeric
GLB_Ts_dSST$`J-D` <- as.numeric(GLB_Ts_dSST$`J-D`)

GLB_Ts_dSST <- GLB_Ts_dSST %>% select(Year, `J-D`) 
GLB_Ts_dSST <- GLB_Ts_dSST[1:(nrow(GLB_Ts_dSST)-1), ]


plot <- ggplot(GLB_Ts_dSST, aes(x = Year, y = 0, fill = `J-D`)) +
  geom_tile(height = 2.6) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y", expand = c(0, 0),
               name = "AVG. TEMPERATURE SCORE\n(across all countries)"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdBu")), limits = c(-0.48, 1.02),
                       breaks = c(-0.48, 0, 1.02),
                       labels = c("Lower\n\ntemperature",
                                  "Average worldwide temperature score\n\n(1899-2022)", "Higher\n\ntemperature"),
                       guide = guide_colourbar(nbin = 300,
                                               ticks.colour = "black", 
                                               ticks.linewidth = 0.5,
                                               frame.colour = "black")) +
  guides(fill = guide_colorbar(barwidth = 35)) +
  labs(title = "Global surface temperature anomaly",
       subtitle = "Relative to 1880-1980 average",
       caption = 'Source: NASA Goddard Institute for Space Studies\n\nhttps://data.giss.nasa.gov/gistemp/ | Plot: @Isaac Bravo') +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "white"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 20, b = 0),
                                    colour = "white"),
        plot.title = element_text(colour = "white"),
        plot.subtitle = element_text(colour = "white"),
        plot.caption = element_text(colour = "white", margin = margin(t = 20)),
        legend.position = "bottom",
        legend.key.width = unit(15, units = "cm"),
        legend.key.height = unit(.9, units = "cm"),
        
        legend.title = element_blank(), legend.text = element_text(colour = "white"),
        legend.margin = margin(t = 0), legend.background = element_blank(),
        plot.background = element_rect(fill = "black", colour = "black"),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) 


animation <- plot + transition_time(Year) +
  shadow_mark(past = TRUE, future = FALSE, color = "transparent") +
  ease_aes('linear') +
  exit_reset()


output_file <- "animation.gif"
animate(animation, nframes = 200, width = 900, height = 600, 
        renderer = gifski_renderer(loop = FALSE), fps = 10)
anim_save(output_file)


ggsave(filename= "warming_striples.png",
       plot=plot,
       pointsize = 24, 
       width = 18 ,
       height = 14,
       scale = 0.5,
       dpi = 1200)





warming_stripes <- function(dataset = GLB_Ts_dSST, stripe_only = FALSE,
                            col_strip = RColorBrewer::brewer.pal(11, "RdBu"),
                            print = TRUE) {
  
  if (is.null(dataset)) return(invisible(NULL))
  
  # Convert Year variable to Date class
  dataset$Year <- as.Date(paste0(dataset$Year, "-01-01"))
  
  # Convert J-D variable to numeric
  dataset$`J-D` <- as.numeric(dataset$`J-D`)
  

  if (!stripe_only) {
    plot <- ggplot(dataset %>% filter(Year != 2023), aes(x = Year, y = 0, fill = `J-D`)) +
      geom_tile(height = 2.6) +
      scale_x_date(date_breaks = "20 years", date_labels = "%Y", expand = c(0, 0),
                   name = "AVG. TEMPERATURE SCORE\n(across all countries)"
                   ) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_gradientn(colours = rev(col_strip), limits = c(-0.48, 1.02),
                           breaks = c(-0.48, 0, 1.02),
                           labels = c("Lower temperature",
                                      "Average worldwide temperature score\n(1899-2022)", "Higher temperature"),
                           guide = guide_colourbar(nbin = 300,
                                                   ticks.colour = "black", 
                                                   ticks.linewidth = 0.5,
                                                   frame.colour = "white")) +
      guides(fill = guide_colorbar(barwidth = 35)) +
      labs(title = "Global surface temperature anomaly",
           subtitle = "Relative to 1880-1980 average",
           caption = 'Source: NASA Goddard Institute for Space Studies\nhttps://data.giss.nasa.gov/gistemp/ | Plot: @Isaac Bravo') +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(margin = margin(t = 20, b = 0),
                                        colour = "white"),
            plot.title = element_text(colour = "white"),
            plot.subtitle = element_text(colour = "white"),
            plot.caption = element_text(colour = "white", margin = margin(t = 20)),
            legend.position = "bottom",
            legend.key.width = unit(15, units = "cm"),
            legend.key.height = unit(.9, units = "cm"),
            
            legend.title = element_blank(), legend.text = element_text(colour = "white"),
            legend.margin = margin(t = 0), legend.background = element_blank(),
            plot.background = element_rect(fill = "grey10", colour = NA),
            plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
            )    
  }
    
  if (print) suppressMessages(print(plot))
}

warming_stripes() + transition_states(Year, wrap = FALSE) +
  shadow_mark()

################################################################################
library(tidyverse)
library(scales)

t_data <- GLB_Ts_dSST %>% 
  select(year = Year, t_diff = `J-D`) %>% 
  drop_na()

annotation <- t_data %>% 
  arrange(year) %>% 
  slice(1, n()) %>% 
  mutate(t_diff = 0,
         x = year + c(-500,2500))

plot_02 <- t_data %>% 
  ggplot(aes(x=year, y=t_diff, fill = t_diff)) +
  geom_col() +
  geom_text(data = annotation, aes(x=x, label=lubridate::year(year)), color="white",
            hjust = 1) +
  geom_text(x=1880, y=1, label="Global temperatures have increased by over 1.2C since 1880",
            color="white", hjust=1.5) +
  scale_fill_stepsn(colors = rev(RColorBrewer::brewer.pal(11, "RdBu")),
                    values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                    limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                    n.breaks = 9) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="black"),
    legend.text = element_text(color="white")
  )

library(gganimate)

animation <- plot_02 + transition_time(year) +
  shadow_mark(past = TRUE, future = FALSE, color = "transparent") +
  ease_aes('linear') +
  exit_reset()

output_file <- "animation_2.gif"
animate(animation, nframes = 200, width = 900, height = 600, 
        renderer = gifski_renderer(loop = FALSE), fps = 10)
anim_save(output_file)









