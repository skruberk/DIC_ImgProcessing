library(dplyr)
library(viridis) #color aesthetics

# Summary Stats -----------------------------------------------------------
#combine all the output files: first set WD
path <- getwd()
csvcomb <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
datacomb <- bind_rows(lapply(csvcomb, read_csv))
# Save the combined data to a new CSV file
write_csv(datacomb, "57-70_comb_output.csv")
#reimport the summary combined data as df 
df<-datacomb
#datacomb$temp <- "70"  # name group in df
#write_csv(df, "66_comb_output_named.csv")
# Motility Mode -----------------------------------------------------------
#motility mode cutoff
# Define AR cutoff
#df$temp <- "70"  # name group in df
df<-data
time_int<-5
epsilon <- 1e-8 #prevent div by 0 error 
motility_mode <- df %>%
  mutate(
    AR_bin = case_when(
      AR >= 1 & AR < 1.1 ~ "1-1.1",
      AR >= 1.1 & AR < 1.25 ~ "1.1-1.25",
      AR >= 1.25 & AR < 1.5 ~ "1.25-1.5",
      AR >= 1.5 & AR < 2.0 ~ "1.5-2",
      AR >= 2.0 & AR < 2.5 ~ "2-2.5",
      AR >= 2.5 & AR < 3 ~ "2.5-3",
      AR >= 3  ~ "3.0",
      TRUE ~ "Other"  # AR values outside of range
    )
  ) 
motility_mode <- motility_mode %>%
  #counts total time for each ID in the bins
  group_by(temp,group, ID, AR_bin) %>%  #temp
  summarise(total_time = n()*time_int,     
            .groups = "drop") %>%
  group_by(group) %>% 
  mutate(
    percent_time = (total_time / sum(total_time + epsilon)) * 100  # normalize per ID
  ) %>%
  ungroup()

motility_mode$temp <- as.factor(motility_mode$temp)# Convert temp to categorical 
p<-ggplot(motility_mode, aes(x = temp, y = percent_time, fill = AR_bin)) +
  geom_bar(stat = "identity", position = "fill") + #change dodge to fill for AR_bin, dodge not normalized
  scale_fill_viridis_d(option = "D", direction = -1) +  # color scale
  labs(x = "Temperature", y = "Percentage of Total Time", 
       title = "Motility Mode") +
  #facet_wrap(~ temp) +  # indiv plots by temperature
  theme_minimal() +
  theme(panel.grid = element_blank())+
  #coord_cartesian(ylim = c(0, 0.5)) + # zoom
  geom_hline(yintercept = c(.25, .50, .75), linetype = "dotted", color = "black") 
p

# Motility Mode Ave per ID ------------------------------------------------
df<-data
#df$temp <- "66"  # name group in df

summary_stats <- df %>%
  group_by(temp, group,ID) %>%  # group by group and ID
  #filter(stepwise_speed > 0) %>%  # quality control
  summarise(
    mean_speed = (mean(stepwise_speed, na.rm = TRUE)),
    median_speed=median(stepwise_speed,na.rm=TRUE),
    sd_speed = sd(stepwise_speed, na.rm = TRUE),
    mean_AR = mean(AR, na.rm = TRUE),
    median_AR=median(AR,na.rm=TRUE),
    sd_AR = sd(AR, na.rm = TRUE),
    mean_PARatio = mean(PARatio, na.rm = TRUE),
    sd_PARatio = sd(PARatio, na.rm = TRUE),
    distance=(mean(stepwise_displacement,na.rm=TRUE))/5*10,
    n = n()  # Count of observations
  ) %>%
  ungroup() %>%
mutate(
  ci_speed_upper = median_speed + (1.96 * sd_speed / sqrt(n)),
  ci_speed_lower = median_speed - (1.96 * sd_speed / sqrt(n))
) %>%
  filter(mean_speed >= ci_speed_lower & mean_speed <= ci_speed_upper) 
View(summary_stats)


#summary_stats$temp <- "57"  # name group in df
write.csv(summary_stats, paste0(name, "_summary_stats.csv"))
#clear console -> cat("\014")

# Visualization -----------------------------------------------------------
summary_stats$temp <- as.factor(summary_stats$temp)  # Convert to factor
p<- ggplot(summary_stats, aes(x=temp, y=median_speed,group=temp))+ ylim(0,0.5)+ geom_boxplot(outlier.shape = NA)+ geom_beeswarm(dodge.width=1,aes(color = temp), size = 0.75, alpha = 0.5,cex=0.5) + scale_color_viridis_d(option = "D", direction = 1)#color by group eventully
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("Median Velocity") 
p

p<- ggplot(summary_stats, aes(x=temp, y=mean_speed,group=temp))+ ylim(0,25)+ geom_boxplot(outlier.shape = NA)+ geom_beeswarm(dodge.width=1,aes(color = temp), size = 0.75, alpha = 0.5,cex=0.65) + scale_color_viridis_d(option = "D", direction = 1)#color by group eventully
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("Mean Velocity um/min") 
p

p<- ggplot(summary_stats, aes(x=temp, y=distance,group=temp))+ ylim(0,5)+geom_boxplot(outlier.shape = NA)+ geom_beeswarm(dodge.width=1,aes(color = temp), size = 0.75, alpha = 0.7,cex=0.5) + scale_color_viridis_d(option = "D", direction = 1)#color by group eventully
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("Distance") 
p

library(ggplot2)

summary_stats$temp <- as.factor(summary_stats$temp)  # Convert to factor
#scatter plots for AR vs mean_speed in minutes
p <- ggplot(summary_stats, aes(x = median_speed, y = median_AR, color = temp, group = temp)) + 
  ylim(0, 5) + 
  geom_point(size = 1.5, alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Best fit lines using linear regression
  scale_color_viridis_d(option = "D", direction = 1) +  # Viridis color scale
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
        #axis.text.x = element_blank()) + 
  ylab("Median AR")+
  xlab("Median Velocity um/min")+
  facet_wrap(~temp)  # Facet by temperature
p


# Mean Velocity per track length ------------------------------------------
# Calculate mean velocity for each ID across groups, have to do each temp separately
track_speed <- data %>%
  arrange(group, ID, time) %>%  # Ensure data is ordered by group, ID, and time
  group_by(group, ID) %>%
  mutate(
    X_shift = lead(Xpos),  # Get the X position of the next frame
    Y_shift = lead(Ypos),  # Get the Y position of the next frame
    time_shift = lead(time) # Get the time of the next frame
  ) %>%
  filter(!is.na(X_shift), !is.na(Y_shift), !is.na(time_shift)) %>%  # Remove rows with missing shifts
  mutate(
    speed = sqrt((X_shift - Xpos)^2 + (Y_shift - Ypos)^2) / (time_shift - time)  # Calculate speed (displacement / time)
  ) %>%
  summarise(median_velocity = median(speed, na.rm = TRUE))  # Calculate mean velocity for each ID

# view it
print(track_speed)
track_speed$temp<-"70" #name group in df
write_csv(track_speed, "70_comb_medtrackspeed.csv")

#combine files
path <- getwd()
csvcomb <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
datacomb <- bind_rows(lapply(csvcomb, read_csv))
# Save the combined data to a new CSV file
write_csv(datacomb, "57-70_comb_mediantrackspeed.csv")

#make micron/min
datacomb <- datacomb %>%
  mutate(median_velocity = median_velocity * 60)

datacomb$temp <- as.factor(datacomb$temp)  # Convert to factor
p<- ggplot(datacomb, aes(x=temp, y=median_velocity,group=temp))+ ylim(0,25)+ geom_boxplot(outlier.shape = NA)+ geom_beeswarm(dodge.width=1,aes(color = temp), size = 0.75, alpha = 1,cex=0.35) + scale_color_viridis_d(option = "D", direction = 1)#color by group eventully
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("Median Velocity per Track um/min") 
p



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
# Summary Stats -----------------------------------------------------------
p <- ggplot(summary_stats, aes(y = mean_PARatio, x = mean_speed, color = group)) + 
  geom_point(size = 1, alpha = 0.7) +  # Scatter plot points
  scale_colour_brewer(palette = "Set2") +  # Color by group
  #ylim(0, 5) +  # Set Y-axis limits
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank() 
    #axis.text.x = element_blank()
  ) 
p

p <- ggplot(df, aes(x = stepwise_speed, y = PARatio)) + 
  geom_line(alpha = 0.35) +  # Draw lines for each ID
  #geom_point(alpha = 0.7) +  # Optional: Show points for clarity
  geom_smooth(method = "lm", se = FALSE, alpha = 0.7) +
  #xlim(0, 1) +
  #ylim(1, 5) +
  labs(
    title = "Speed vs. Perimter/Area Ratio One Movie at 57",
    x = "speed",
    y = "Perimeter to Area Ratio"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
p

df_avg <- df %>%
  group_by(group, ID) %>%  # G
  summarise(
    mean_speed = mean(mean_speed, na.rm = TRUE),  # 
    mean_AR = mean(mean_AR, na.rm = TRUE),  # 
    sd_speed = sd(mean_speed, na.rm = TRUE),  # 
    sd_AR = sd(mean_AR, na.rm = TRUE)  # 
  ) %>%
  ungroup() %>%  # remove grouping after summarizing
  group_by(temp) %>%  # group again by temperature to get overall averages
  summarise(
    avg_mean_speed = mean(mean_speed, na.rm = TRUE),  # ave across IDs for each temperature
    avg_mean_AR = mean(mean_AR, na.rm = TRUE),  # ave AR across IDs for each temperature
    avg_sd_speed = mean(sd_speed, na.rm = TRUE),  # ave SD of speed
    avg_sd_AR = mean(sd_AR, na.rm = TRUE)  # ave SD of AR
  )


df$temp <- as.factor(df$temp)
p <- ggplot(df, aes(x = mean_speed, y =mean_AR, group = temp, color = temp)) + 
  geom_line(alpha = 0.7) +  # Average line
  geom_ribbon(
    aes(ymin = avg_mean_speed - avg_sd_speed, ymax = avg_mean_speed + avg_sd_speed), 
    alpha = 0.3  # Shaded region to show SD
  ) + 
  xlim(0, 4) + 
  ylim(0, 4) + 
  labs(
    title = "Speed vs. Aspect Ratio by Temperature (Averaged across IDs)",
    x = "Average Aspect Ratio",
    y = "Average Speed"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
p

p <- ggplot(df, aes(x = stepwise_speed, y = PARatio, group = ID, color = ID)) + 
  geom_line(alpha = 0.7) +  # Draw lines for each ID
  geom_point(alpha = 0.7) +  # Optional: Show points for clarity
  #xlim(0, 2.0) +
  #ylim(0, 4) +
  labs(
    title = "Speed vs. Aspect Ratio",
    x = "Speed",
    y = "Perimeteter to Aspect Ratio"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
p

# Change in AR vs Change in Speed -----------------------------------------
df <- df %>%
  arrange(ID, Frame) %>%  # Order by ID and time
  group_by(ID) %>%
  mutate(
    delta_AR = AR - lag(AR),  # change in Aspect Ratio
    delta_speed = abs(stepwise_speed - lag(stepwise_speed))  # Absolute change in Speed
  ) %>%
  ungroup() #%>%  # apply filtering to the full dataset

#average speed vs average shape 
p<-ggplot(summary_stats, aes(x = median_speed, y = mean_AR)) +
  geom_point(alpha = 0.6) +
  #ylim(0,15)+
  #xlim(0,2.5)+
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  #trend line
  labs(title = "Median Speed vs Aspect Ratio at 57C FOV1",
       x = "median speed",
       y = "mean Aspect Ratio") +
  theme_minimal()
p
cor_test <- cor.test(summary_stats$median_speed, summary_stats$mean_AR, use = "complete.obs")
print(cor_test)