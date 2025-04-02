#cell turning and directional change calculation 

# Directional Change ------------------------------------------------------
# Calculate Cell Turning aka Angular Speed --------------------------------------------------
# calculate the smallest difference between two angles using vectors instead
angle_difference <- function(angle1, angle2) { 
  diff <- (angle2 - angle1 + 540) %% 360 - 180
  return(diff)
}
#could smooth with moving aves for x and y pos before angle calculations if #s too noisy
df_direction <- df %>% 
  arrange(group,ID,frame) %>%
  group_by(group,ID) %>% 
  mutate(
    dx = Xpos - lag(Xpos, default = Xpos[1]),
    dy = Ypos - lag(Ypos, default = Ypos[1]), # displacement vector
    angle = atan2(dy, dx) * (180 / pi), # convert to degrees
    # use function to compute angle changes
    angle_change_deg = c(NA, mapply(angle_difference, head(angle, -1), tail(angle, -1)))  # angle change calculation
  ) %>% 
  ungroup()
#View(df_direction)
df_direction <- df_direction %>%
  group_by(temp,group,ID) %>%
  summarize(
    tot_time = (max(time, na.rm = TRUE) - min(time, na.rm = TRUE)) / 60,
    angle_change = mean(abs(angle_change_deg), na.rm = TRUE),
    dir_rate=(angle_change/tot_time)/360
  )%>%
  #filter(dir_change_rate > 0) %>% 
  ungroup()
#View(df_direction)
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

