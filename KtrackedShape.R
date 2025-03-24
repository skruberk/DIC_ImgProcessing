#K-Tracked Particles

#restart
rm(list = ls())
rm(list = ls(all.names = TRUE))

##########
library(dplyr)
library(FNN)  # Fast Nearest Neighbors package
library(viridis) #color aesthetics

#DO DATA IMPORT HERE##################dataimport 

# NAME OF DATASET 
name <- "66C_120_360_FOV_1" 
df<-data
# Define parameters
k_neighbors <- 2 #start with 3
#number of neighbors to check 1-3 seems to work for well sep particles, low noise
#low noise, not lots of neighbors, 4-6 and higher: fast and erratic movement lots of 
#neighbors noisy tracking 
max_distance <- 7.5  # Maximum allowed displacement, is it moving fast? up this #:25 isn't unreasonable for v fast
gap_tolerance <- 3  # Number of frames a particle can disappear before being reassigned, this number is very sensitive

# Initialize Particle_ID column
df$ID <- NA
particle_counter <- 1

# assign initial particle IDs for first frame
df$ID[df$Frame == min(df$Frame)] <- 1:sum(df$Frame == min(df$Frame))

# Loop through frames
for (frame in unique(df$Frame)[-1]) {
  current <- df %>% filter(Frame == frame)
  #also search for matches in previous frames which is gap tolerance
  previous <- df %>% filter(Frame >= (frame - gap_tolerance) & Frame < frame)
  
  if (nrow(previous) == 0) next  # skip if no particles in previous frame
  
  # Use KNN to find the closest matches
  nn_results <- get.knnx(data = previous[, c("Xpos", "Ypos")], #frames previous particles
                         query = current[, c("Xpos", "Ypos")], #match particles in current
                         k = k_neighbors) #finds k nearest matches in prev frame
  assigned_IDs <- c()  # track already used IDs in this frame
  for (i in 1:nrow(current)) {
    distances <- nn_results$nn.dist[i, ]  # get distances for k neighbors
    indices <- nn_results$nn.index[i, ]  # get indices of k neighbors in previous frame
    
    valid_match <- which(distances < max_distance)  # filter valid matches
    if (length(valid_match) > 0) {
      best_match <- indices[which.min(distances)]  # choose first valid match, could be optimization point 
      matched_ID <- previous$ID[best_match]
      
      # has ID already been assigned to frame?>
      if (!(matched_ID %in% assigned_IDs)) {
        df$ID[df$Frame == frame & df$Xpos == current$Xpos[i] & df$Ypos == current$Ypos[i]] <- matched_ID
        assigned_IDs <- c(assigned_IDs, matched_ID)  # mark ID as used
      } else {
        # if ID already taken assign new one
        df$ID[df$Frame == frame & df$Xpos == current$Xpos[i] & df$Ypos == current$Ypos[i]] <- particle_counter
        particle_counter <- particle_counter + 1
      }
    } else {
      # if no match is found assign new ID
      df$ID[df$Frame == frame & df$Xpos == current$Xpos[i] & df$Ypos == current$Ypos[i]] <- particle_counter
      particle_counter <- particle_counter + 1
    }
  }
}
#filter short lived particles, QC 
df <- df %>%
  group_by(ID) %>%
  filter(n() > 6)%>%   # Remove particles appearing in fewer than x frames
  ungroup()

t_int=5 #seconds/frame
df$time <- df$Frame * t_int
#get instantaneous displacement at each step, euclidean distance
df <- df %>%
  arrange(ID, Frame) %>%
  group_by(ID) %>%
  mutate(
    # Calculate stepwise displacement
    stepwise_displacement = sqrt((Xpos - lag(Xpos))^2 + (Ypos - lag(Ypos))^2)
  ) %>%
  # Replace NA in stepwise displacement with 0 (only in a separate mutate step)
  mutate(
    stepwise_displacement = replace_na(stepwise_displacement, 0)
  ) %>%
  # Filter out rows where stepwise displacement is greater than 20
  filter(stepwise_displacement <= 10 | stepwise_displacement == 0) %>%
  mutate(
    # Calculate stepwise speed
    stepwise_speed = stepwise_displacement * (1 / t_int)  # Speed per step
  ) %>%
  ungroup()


#see the trajectories
p<-ggplot(df, aes(x = Xpos, y = Ypos, color = factor(ID), group = factor(ID))) +
  geom_path() +  # Connect points to form trajectories
  labs(title = "Particle Trajectories", x = "X Position", y = "Y Position", color = "Particle ID") +
  theme_minimal()+
  theme(legend.position = "none")
p

#track quality control
# Track Length (sum of all displacements for each ID)
track_length <- df %>%
  group_by(ID) %>%
  summarise(track_length = sum(stepwise_displacement))

# Number of ID changes (count of unique IDs for each particle across frames)
id_changes <- df %>%
  group_by(ID) %>%
  summarise(id_changes = n_distinct(ID))

# Merging track length and ID changes into a single data frame
track_quality_metrics <- track_length %>%
  left_join(id_changes, by = "ID")

# output QC results
write.csv(df, paste0(name, "_trajectoryQC.csv"))

#convert from microns back to pixels for image comparison 
pixel_width <- 0.16  # image scale is this right?
pixel_height <- 0.16  # 

#pixel micron space conversion for direct overlap  
df <- df %>%
  mutate(
    X_pixel = Xpos / pixel_width,
    Y_pixel = Ypos / pixel_height
  )
#does this trajectory look somewhat like your max ip? if v much no, go change settings
p <- ggplot(df, aes(x = Xpos, y = Ypos, group = ID, color = AR)) +    
  #geom_path(size = 1) +    # Plot trajectory
  geom_point(size = 2) +   # Mark each recorded position
  scale_color_viridis_c(option = "plasma", name = "Aspect Ratio") +  
  labs(title = "Particle Trajectories by Aspect Ratio 64C", 
       x = "X Position", 
       y = "Y Position") +  
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove all grid lines
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"))   # Keep axis lines
p

#perimeter to area ratio
df<- df %>%
  mutate(
    sqrtarea = sqrt(area),
    PARatio = perim/sqrtarea 
  )

# Output csv ----------------------------------------------------------
df$group <- paste0(name)  # name group in df
# save velocity aspect ratio data
write.csv(df, paste0(name, "_shape.csv"))

# Summary Stats -----------------------------------------------------------
#combine all the files: first set WD
path <- getwd()
csvcomb <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
datacomb <- bind_rows(lapply(csvcomb, read_csv))
# Save the combined data to a new CSV file
write_csv(datacomb, "comb_output.csv")
#reimport the summary combined data as df 
df<-data
df$temp <- "66"  # name group in df

# Motility Mode -----------------------------------------------------------
#motility mode cutoff
# Define AR cutoff
time_int<-5
epsilon <- 1e-8 #prevent div by 0 error 
motility_mode <- df %>%
  mutate(
    AR_bin = case_when(
      AR >= 1 & AR < 1.5 ~ "1-1.5",
      AR >= 1.5 & AR < 2.0 ~ "1.5-2",
      AR >= 2.0 & AR < 2.5 ~ "2-2.5",
      AR >= 2.5 & AR < 3 ~ "2.5-3",
      AR >= 3 & AR < 3.5 ~ "3-3.5",
      AR >= 3.5 & AR < 4.0 ~ "3.5-4",
      AR >= 4 ~ "+4",
      TRUE ~ "Other"  # For AR values outside of the range, if needed
    )
  ) %>%
  group_by(temp, AR_bin) %>%
  summarise(
    total_time = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Percentage = (total_time / sum(total_time)) * 100
  )
p<-ggplot(motility_mode, aes(x = AR_bin, y = Percentage, fill = AR_bin)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "D", direction = -1) +  # Apply viridis color scale
  labs(x = "Aspect Ratio Range", y = "Percentage of Total Time", 
       title = "Motility Mode") +
  facet_wrap(~ temp) +  # Separate plots by temperature
  theme_minimal() +
  theme(panel.grid = element_blank())  
p


# Motility Mode Ave per ID ------------------------------------------------
df<-data
df$temp <- "66"  # name group in df
ave_motility<- df %>%
  group_by(group, ID,temp) %>%  # group and ID
           #filter(stepwise_speed <= 25) %>%  # quality control
           summarise(
             mean_AR = mean(AR, na.rm = TRUE),
             n = n() 
           ) %>%
  ungroup()
p<-ggplot(ave_motility, aes(x = temp, y = mean_AR, color = mean_AR)) +
  geom_jitter(width = 0.1, height = 0.1, size = 1)+
  scale_color_viridis(option = "C", direction = 1) +
  labs(x = "temp", y = "average Aspect Ratio", title = "Aspect Ratio by Temperature") +
  theme_minimal()+
  theme(panel.grid = element_blank()) 
p

summary_stats <- df %>%
  group_by(group, ID) %>%  # group by group and ID
  filter(stepwise_speed <= 25) %>%  # quality control
  summarise(
    mean_speed = mean(stepwise_speed, na.rm = TRUE),
    median_speed=median(stepwise_speed,na.rm=TRUE),
    sd_speed = sd(stepwise_speed, na.rm = TRUE),
    mean_AR = mean(AR, na.rm = TRUE),
    sd_AR = sd(AR, na.rm = TRUE),
    mean_PARatio = mean(PARatio, na.rm = TRUE),
    sd_PARatio = sd(PARatio, na.rm = TRUE),
    n = n()  # Count of observations
  ) %>%
  ungroup()

View(summary_stats)
summary_stats$temp <- "57"  # name group in df
write.csv(summary_stats, paste0(name, "_summary_stats.csv"))
#clear console -> cat("\014")

# Visualization -----------------------------------------------------------
df$temp <- as.factor(df$temp)  # Convert to factor

p<- ggplot(summary_stats, aes(x=group, y=mean_PARatio))+ ylim(0,0.25)+ geom_boxplot(outlier.shape = NA)+ geom_beeswarm(dodge.width=0.15,aes(color=temp)) + scale_colour_brewer(palette = "Set2") #color by group eventully
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("aspect ratio") 
p

p<-ggplot(summary_stats, aes(x = mean_PARatio, y = mean_speed, color = group)) +
  geom_point() +
  xlim(6,10)+
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines for each temperature group
  labs(x = "Perimeter to Area", y = "Speed", title = "Speed vs Aspect Ratio by Temperature") +
  theme_minimal()
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

# Directional Change ------------------------------------------------------
# Calculate Cell Turning aka Angular Speed --------------------------------------------------
# calculate the smallest difference between two angles using vectors instead
angle_difference <- function(angle1, angle2) { 
  diff <- angle2 - angle1
  diff <- atan2(sin(diff), cos(diff))  # Ensures result is between -π and π
  return(diff * (180 / pi))  # Convert to degrees
}
#could smooth with moving aves for x and y pos before angle calculations if #s too noisy 
df_direction <- df %>% 
  group_by(ID) %>% 
  mutate(
    dx = Xpos - lag(Xpos, default = Xpos[1]),
    dy = Ypos - lag(Ypos, default = Ypos[1]), # displacement vector
    angle = atan2(dy, dx), # compute angle in radians btw pos x axis and the vector dx,dy
    # use function to compute angle changes
    angle_change_deg = c(NA, mapply(angle_difference, head(angle, -1), tail(angle, -1)))  # angle change calculation
  ) %>% 
  ungroup()

# plot direction changes by angle 
p<-ggplot(df_direction, aes(x = Xpos, y = Ypos, color = abs(angle_change_deg))) +
  geom_point(size = 2) +
  scale_color_gradient(low = "blue", high = "red",limits = c(0, 360)) +  # can make this a better scale too
  labs(title = "Cell Directional Changes",
       x = "X Position",
       y = "Y Position",
       color = "Angle Change (°)") +
  theme_minimal()
p

#output as total changes per minute, total time per min in revolutions 
df_direction <- df_direction %>%
  group_by(ID) %>%
  summarize(
    tot_time = (max(time, na.rm = TRUE) - min(time, na.rm = TRUE)) / 60,
    angle_change = sum(abs(angle_change_deg), na.rm = TRUE), #make sure not cancel each other out 
    full_rev = angle_change / 360, 
    dir_change_rate = (full_rev / tot_time) # compute within the same step
  )%>%
  filter(dir_change_rate > 0) %>% 
  ungroup()
View(df_direction)
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
