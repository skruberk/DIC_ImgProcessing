#cell turning and directional change calculation 
# Directional Change ------------------------------------------------------
# Calculate Cell Turning aka Angular Speed --------------------------------------------------
# calculate the smallest difference between two angles using vectors instead
library(zoo)
custom_colors <- c(
  #"#5662B2",
  "#6389c9",
  "#9FCAFF",
  "#d1d1d1",
  "#D3BB9C",
  "#D7A465",
  "#F09900",
  "#e9881a",
  "#CD782E",
  "#C1532D",
  "#AD372A" 
)
df<-data
angle_difference <- function(angle1, angle2) { 
  diff <- (angle2 - angle1 + 540) %% 360 - 180
  return(diff)
}
#could smooth with moving aves for x and y pos before angle calculations if #s too noisy
df_direction <- df %>% 
  arrange(group,ID,Frame) %>%
  group_by(group,ID) %>% 
  mutate(
    dx = Xpos - lag(Xpos, default = Xpos[1]),
    dy = Ypos - lag(Ypos, default = Ypos[1]), # displacement vector
    angle = atan2(dy, dx) * (180 / pi), # convert to degrees
    # use function to compute angle changes
    angle_change_deg = c(NA, unlist(mapply(angle_difference, head(angle, -1), tail(angle, -1)))  # angle change calculation
  )) %>% 
  ungroup()


#plotting custom angle labels
p <- ggplot(df_direction, aes(x = angle_change_deg, fill = as.factor(temp))) +
  geom_histogram(binwidth = 8, aes(y = ..density..), alpha = 0.7, position = "identity") +
  coord_polar(theta = "x") +  # Converts to circular plot
  #scale_fill_viridis_d() +
  scale_fill_manual(values = custom_colors)+
  theme_minimal() +
  facet_wrap(~temp) +  # Facet by temperature
  labs(title = "Distribution of Turning Angles by Temperature",
       x = "Angle Change (degrees)",
       y = "Density",
       fill = "Temperature") +
  theme(
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.y = element_blank(),  # Remove y-axis title
    strip.background = element_blank(),  # Remove background color from facet titles
    strip.text = element_text(size = 12, face = "bold"),  # Facet title text style
    panel.spacing = unit(2, "lines"),  # Increase space between facets
    panel.background = element_blank(),  # Remove background fill of the entire plot
    plot.background = element_blank(),  # Make entire plot background transparent
    plot.margin = margin(1, 1, 1, 1)  # Add extra margin around the plot
  ) +
  # Custom angle labels (0°, 90°, -90°, 180°, -180°)
  scale_x_continuous(
    breaks = c(0, 90, -90, -180),
    #labels = c("0°", "90°", "-90°", "180°", "-180°"),
    limits = c(-180, 180)  # Ensure the angles are correctly displayed
  )

p


View(df_direction)
df_plot <- df_direction %>%
  group_by(temp,group,ID) %>%
  summarize(
    dir_rate=median(dir_rate)
  )%>%
  #filter(dir_change_rate > 0) %>% 
  ungroup()
#View(df_plot)

df_plot$temp <- as.factor(df_plot$temp)  # Convert to factor
p<- ggplot(df_plot, aes(x=temp, y=dir_rate,group=temp))+ ylim(0,0.5)+
  geom_boxplot(outlier.shape = NA)+ geom_beeswarm(dodge.width=0.15,aes(color = temp), size = 0.5, alpha = 1,cex=0.45) + scale_color_viridis_d(option = "D", direction = 1)#color by group eventully
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("Direction Change Rate Revolutions per Minute") 
p
write.csv(df_plot, "dir-changes-median.csv",row.names=TRUE)


