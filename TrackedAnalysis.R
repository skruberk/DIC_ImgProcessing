library(dplyr)
library(viridis) #color aesthetics
library(ggbeeswarm) 
library(tidyr)
library(readr)

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


# Summary Stats -----------------------------------------------------------
#combine all the output files: first set WD
path <- getwd()
csvcomb <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
datacomb <- bind_rows(lapply(csvcomb, read_csv))
# Save the combined data to a new CSV file
write_csv(datacomb, "filename.csv")
#reimport the summary combined data as df 
df<-data
#datacomb$temp <- "70"  # name group in df
#write_csv(df, "66_comb_output_named.csv")
# Motility Mode -----------------------------------------------------------
#motility mode cutoff
# Define AR cutoff
#df$temp <- "70"  # name group in df
#df<-data
time_int<-1
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
  #scale_fill_viridis_d(option = "D", direction = -1) +  # color scale
  scale_fill_manual(values = custom_colors) + 
  labs(x = "Temperature", y = "Percentage of Total Time", 
       title = "Motility Mode") +
  #facet_wrap(~ temp) +  # indiv plots by temperature
  theme_minimal() +
  theme(panel.grid = element_blank())+
  #coord_cartesian(ylim = c(0, 0.5)) + # zoom
  geom_hline(yintercept = c(.25, .50, .75), linetype = "dotted", color = "black") 
p



# lamelli vs worm ---------------------------------------------------------
df<-data

# Bin the aspect ratio values
motility_mode <- df %>%
  filter(!is.na(AR)) %>% 
  mutate(
    AR_bin = case_when(
      AR >= 1 & AR < 1.25 ~ "cyst",
      AR >= 1.25 & AR < 2.25 ~ "lamelli",
      AR >= 2.25 & AR < 10 ~ "worm",
      TRUE ~ "Other"
    )
  ) %>%
  count(temp, AR_bin, name = "n") %>%  # count occurrences
  group_by(temp) %>%
  mutate(
    total_n = sum(n),
    percent = (n / total_n) * 100
  ) %>%
  ungroup()

# AR_bin is ordered logically
motility_mode$AR_bin <- factor(motility_mode$AR_bin, levels = c("cyst", "lamelli", "worm"))

# Plot
p<-ggplot(motility_mode, aes(x = as.factor(temp), y = percent, fill = AR_bin)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_viridis_d(name = "AR Category", option = "D", direction = -1) +
  labs(
    x = "Temperature",
    y = "Percent Time",
    title = "Motility Mode"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())
p
View(motility_mode)

# Motility Mode Ave per ID ------------------------------------------------
df<-data
#df$temp <- "66"  # name group in df

summary_stats <- df %>%
  group_by(temp, group,ID) %>%  # group by temp group and ID to get particle track
  filter(stepwise_speed > 0, stepwise_speed < 2.5) %>%  # quality control for detached cells
  summarise(
    mean_speed = (mean(stepwise_speed, na.rm = TRUE))*60, #per minute
    mean_area = (mean(area, na.rm = TRUE)),
    median_area = (median(area, na.rm = TRUE)),
    median_speed=median(stepwise_speed,na.rm=TRUE)*60,
    sd_speed = sd(stepwise_speed, na.rm = TRUE),
    mean_AR = mean(AR, na.rm = TRUE),
    median_AR=median(AR,na.rm=TRUE),
    sd_AR = sd(AR, na.rm = TRUE),
    mean_major=mean(Major,na.rm=TRUE),
    mean_minor=mean(Minor,na.rm=TRUE),
    mean_PARatio = mean(PARatio, na.rm = TRUE),
    sd_PARatio = sd(PARatio, na.rm = TRUE),
    distance=(mean(stepwise_speed,na.rm=TRUE))*60, #per minute
    n = n()  # Count of observations
  ) %>%
  ungroup() %>%
mutate(
  mean_speed = mean_speed,
  ci_speed_upper = mean_speed + (1.645 * sd_speed / sqrt(n)),
  ci_speed_lower = mean_speed - (1.645 * sd_speed / sqrt(n))
) %>%
  filter(mean_speed >= ci_speed_lower & mean_speed <= ci_speed_upper) 

IQRstats <- summary_stats %>%
  group_by(temp) %>% 
  summarise(
    median_speed2=median(median_speed,na.rm=TRUE),
    speed_IQR = IQR(median_speed, na.rm = TRUE),
    speed_Q1 = quantile(median_speed, 0.25, na.rm = TRUE),
    speed_Q3 = quantile(median_speed, 0.75, na.rm = TRUE),
    speed_p5 = quantile(median_speed, 0.05, na.rm = TRUE),
    speed_p95 = quantile(median_speed, 0.95, na.rm = TRUE)
  ) %>%
  ungroup()
View(IQRstats)

write.csv(IQRstats,"IQR_Velocity_stats_final.csv")

View(summary_stats)
mean_stats <- summary_stats %>%
  group_by(temp) %>%  # group by temp, this should already be reported per track 
  summarise(
    mean_speed = (mean(mean_speed, na.rm = TRUE)),
    mean_area = (mean(mean_area, na.rm = TRUE)),
    median_area = (median(median_area, na.rm = TRUE)),
    median_speed=median(median_speed,na.rm=TRUE),
    mean_major=mean(mean_major,na.rm=TRUE),
    mean_minor=mean(mean_minor,na.rm=TRUE),
    mean_AR = mean(mean_AR, na.rm = TRUE),
    median_AR=median(median_AR,na.rm=TRUE),
    n = n()  # count of observations
  ) %>%
  ungroup() 
View(mean_stats)

#summary_stats$temp <- "57"  # name group in df
write.csv(means_stats, paste0(name, "_meansummary_stats_final.csv"))
#clear console -> cat("\014")

# Visualization -----------------------------------------------------------
summary_stats$temp <- as.factor(summary_stats$temp)  # Convert to factor
p<- ggplot(summary_stats, aes(x=temp, y=median_speed,group=temp))+ coord_cartesian(ylim=c(0,45)) + geom_boxplot(outlier.shape = NA)+ 
  geom_beeswarm(dodge.width=1,aes(color = temp), size = 0.5, alpha = 0.5,cex=0.25) + 
  #scale_color_viridis_d(option = "D", direction = 1)#color by group eventully
  scale_color_manual(values = custom_colors)
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("Median Velocity µm/min") 
p

p<- ggplot(summary_stats, aes(x=temp, y=mean_speed,group=temp))+ coord_cartesian(ylim=c(0,50)) + geom_boxplot(outlier.shape = NA)+ 
  geom_beeswarm(dodge.width=1,aes(color = temp), size = 0.5, alpha = 0.5,cex=0.25) + 
  #scale_color_viridis_d(option = "D", direction = 1)#color by group eventully
  scale_color_manual(values = custom_colors)
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("Mean Velocity um/min") 
p


  ylab("Mean Velocity (µm/min)")


data$temp <- as.factor(data$temp)
p<- ggplot(summary_stats, aes(x=temp, y=median_area,group=temp))+ coord_cartesian(ylim=c(0,675)) + geom_boxplot(outlier.shape = NA)+ 
  geom_beeswarm(dodge.width=1,aes(color = temp), size = 0.5, alpha = 0.5,cex=0.25) + 
  #scale_color_viridis_d(option = "D", direction = 1)
  scale_color_manual(values = custom_colors)
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("Median Area um^2") 
p

# Sample 30% of points from each group to reduce overplotting
sampled_points <- summary_stats %>%
  group_by(temp) %>%
  sample_frac(0.6)

p <- ggplot(summary_stats, aes(x = temp, y = median_area, group = temp)) +
  coord_cartesian(ylim = c(0, 750)) +
  geom_boxplot(outlier.shape = NA) +
  geom_beeswarm(data = sampled_points, aes(color = temp),
                dodge.width = 0.5, size = 1.0, alpha = 0.5, cex = 0.35) +
  scale_color_manual(values = custom_colors) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()
  ) +
  ylab("Mean Area µm²")

p


p<- ggplot(summary_stats, aes(x=temp, y=distance,group=temp))+ ylim(0,10)+geom_boxplot(outlier.shape = NA)+ geom_beeswarm(dodge.width=1,aes(color = temp), size = 0.75, alpha = 0.7,cex=0.5) + scale_color_viridis_d(option = "D", direction = 1)#color by group eventully
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("Distance") 
p


# axes length by cyst, lamelli, worm --------------------------------------
#cyst 1-1.25 from 25, 70C 
# worm form AR: >3 pull exclude 44,50
# lamelli form 1.25-3 exclude 44, 50
#range, average, length width, AR, 
#which dataframe? Raw or aggregated? 
group_by(temp,group, ID, AR_bin) %>%
  
df<- data #raw  
df <- summary_stats #summarized 
motility_mode2 <- df %>%
  filter(!is.na(mean_AR)) %>%
  mutate(
    AR_bin = case_when(
      mean_AR >= 1 & mean_AR < 1.25 & (temp == 25 | temp == 70) ~ "cyst",
      mean_AR >= 1.25 & mean_AR < 3 & (temp == 57 | temp == 60 | temp == 62 | temp == 64) ~ "lamelli",
      mean_AR >= 3 & (temp == 57 | temp == 60 | temp == 62 | temp == 64) ~ "worm",
      TRUE ~ NA_character_  
    )
  )

motility_summary <- motility_mode2 %>%
  group_by(AR_bin) %>%
  summarise(
    mean_length = mean(mean_major, na.rm = TRUE),
    mean_width = mean(mean_minor, na.rm = TRUE),
    mean_AR = mean(mean_AR, na.rm = TRUE),
    sd_length = sd(mean_major, na.rm = TRUE),
    sd_width = sd(mean_minor, na.rm = TRUE),
    sd_AR = sd(median_AR, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()
View(motility_summary)
write.csv(motility_summary, "shape_binned_final.csv")

# MEDIAN AR VS MEDIAN TRACK VEL MICRON/MIN --------------------------------

#make micron/min
#summary_stats <- summary_stats %>%
 # mutate(mean_speed = mean_speed * 60)
#summary_stats <- summary_stats %>% filter(mean_speed > 0)

# Median AR vs Median Track Velocity in um/min ----------------------------

summary_stats$temp <- as.factor(summary_stats$temp)  # Convert to factor
#scatter plots for AR vs mean_speed in minutes
p <- ggplot(summary_stats, aes(x = mean_speed, y = median_AR, color = temp, group = temp)) + 
  #ylim = (0, 5) + 
  geom_point(size = 1.5, alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE,color = "black", linewidth = 1.1) +  # Best fit lines using linear regression
  scale_color_viridis_d(option = "D", direction = 1) +  # Viridis color scale
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
        #axis.text.x = element_blank()) + 
  ylab("Median Aspect Ratio")+
  xlab("Median Track Velocity (um/min)")+
  facet_wrap(~ temp, ncol = 4, scales = "free") +
  coord_cartesian(ylim=c(0,6),xlim = c(0, 30)) 
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

# major minor axes --------------------------------------------------------
#reshape
long_data <- summary_stats %>%
  mutate(ID = row_number()) %>%  # Add unique ID if not already present
  pivot_longer(cols = c(mean_major, mean_minor), 
               names_to = "axis_type", 
               values_to = "axis_length")

p<-ggplot(long_data, aes(x = axis_type, y = axis_length, group = ID)) +
  geom_line(color = "gray70", alpha = 0.5) +  # Connect major/minor axes for each particle
  geom_point(aes(color = axis_type), size = 2) +
  scale_color_manual(values = c("mean_major" = "red", "mean_minor" = "blue")) +
  facet_wrap(~ temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Axis") +
  ylab("Mean Axis Length (μm)") +
  ggtitle("Aspect Ratio")
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
