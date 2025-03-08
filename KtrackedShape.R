#K-Tracked Particles
#restart 120-899, 120-314, 315-505, 506-696, 697-899
rm(list = ls())
rm(list = ls(all.names = TRUE))

##########
library(dplyr)
library(FNN)  # Fast Nearest Neighbors package
library(viridis) #color aesthetics

#dataimport 
df<-data
# Define parameters
k_neighbors <- 3 #start with 3
#number of neighbors to check 1-3 seems to work for well sep particles, low noise
#low noise, not lots of neighbors, 4-6 and higher: fast and erratic movement lots of 
#neighbors noisy tracking 
max_distance <- 15  # Maximum allowed displacement, is it moving fast? up this #:25 isn't unreasonable for v fast
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
      best_match <- indices[which.min(distances)]  # Choose first valid match
      matched_ID <- previous$ID[best_match]
      
      # has ID already been assigned to frame?>
      if (!(matched_ID %in% assigned_IDs)) {
        df$ID[df$Frame == frame & df$Xpos == current$Xpos[i] & df$Ypos == current$Ypos[i]] <- matched_ID
        assigned_IDs <- c(assigned_IDs, matched_ID)  # Mark ID as used
      } else {
        # if ID already taken assign new one
        df$ID[df$Frame == frame & df$Xpos == current$Xpos[i] & df$Ypos == current$Ypos[i]] <- particle_counter
        particle_counter <- particle_counter + 1
      }
    } else {
      # if no match is found assign a new ID
      df$ID[df$Frame == frame & df$Xpos == current$Xpos[i] & df$Ypos == current$Ypos[i]] <- particle_counter
      particle_counter <- particle_counter + 1
    }
  }
}
#filter short lived particles 
df <- df %>%
  group_by(ID) %>%
  filter(n() > 3)%>%   # Remove particles appearing in fewer than x frames
  ungroup()

t_int=5 #seconds/frame
#get instantaneous displacement at each step, euclidean distance
df <- df %>%
  arrange(ID, Frame) %>%
  group_by(ID) %>%
  mutate(
    stepwise_displacement = sqrt((Xpos - lag(Xpos))^2 + (Ypos - lag(Ypos))^2),
    stepwise_displacement = replace_na(stepwise_displacement, 0),  # Replace first NA with 0
    stepwise_speed = stepwise_displacement * (1/t_int)  # Speed per step
  ) %>%
  ungroup() 
  
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
  labs(title = "Particle Trajectories by Aspect Ratio", 
       x = "X Position", 
       y = "Y Position") +  
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove all grid lines
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"))   # Keep axis lines
p
#high perimeter to area ratio would tell you how lamellipodial like the outline is
df$group <- "group"
p<- ggplot(df, aes(x=group, y=AR))+ ylim(0,5)+ geom_boxplot(outlier.shape = NA)+ geom_beeswarm(dodge.width=0.15,aes()) + scale_colour_brewer(palette = "Set2") +#color by group eventully
 theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p


# APR vs AR ----------------------------------------------------------
df<- df %>%
  mutate(
    sqrtarea = sqrt(area),
    APRatio = sqrtarea / perim
  )

p <- ggplot(df, aes(x = stepwise_speed, y = AR, group = ID, color = ID)) + 
  geom_line(alpha = 0.7) +  # Draw lines for each ID
  geom_point(alpha = 0.7) +  # Optional: Show points for clarity
  xlim(0, 2.0) +
  ylim(0, 4) +
  labs(
    title = "Speed vs. Aspect Ratio",
    x = "Speed",
    y = "Aspect Ratio"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
p

df$group <- "1_240_57Cto65C_FOV1"  # name group in df
# save velocity aspect ratio data
write.csv(df, "1_240_57Cto65C_FOV1_shape.csv", row.names = FALSE)
#if importing into MultiTrackmateAnalysis run
df<-data
#df$temp <- "64"  # name group in df
summary_stats <- df %>%
  group_by(group, ID) %>%  # group by group and ID
  filter(stepwise_speed <= 15) %>%  # quality control
  summarise(
    mean_speed = mean(stepwise_speed, na.rm = TRUE),
    sd_speed = sd(stepwise_speed, na.rm = TRUE),
    mean_AR = mean(AR, na.rm = TRUE),
    sd_AR = sd(AR, na.rm = TRUE),
    mean_APRatio = mean(APRatio, na.rm = TRUE),
    sd_APRatio = sd(APRatio, na.rm = TRUE),
    n = n()  # Count of observations
  ) %>%
  ungroup()


View(summary_stats)
summary_stats$temp <- "57"  # name group in df
write.csv(summary_stats, "57temp_summary.csv", row.names = FALSE)
#clear console -> cat("\014")

# Visualization -----------------------------------------------------------
df$temp <- as.factor(df$temp)  # Convert to factor

p<- ggplot(df, aes(x=temp, y=mean_APRatio))+ ylim(0,0.25)+ geom_boxplot(outlier.shape = NA)+ geom_beeswarm(dodge.width=0.15,aes(color=temp)) + scale_colour_brewer(palette = "Set2") #color by group eventully
p 
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) + 
  ylab("aspect ratio") 
p

library(ggplot2)
p<-ggplot(df, aes(x = mean_AR, y = mean_speed, color = temp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines for each temperature group
  labs(x = "Aspect Ratio", y = "Speed", title = "Speed vs Aspect Ratio by Temperature") +
  theme_minimal()
p

# Change in AR vs Change in Speed -----------------------------------------
df <- df %>%
  arrange(ID, Frame) %>%  # Order by ID and time
  group_by(ID) %>%
  mutate(
    delta_AR = AR - lag(AR),  # change in Aspect Ratio
    delta_speed = stepwise_speed - lag(stepwise_speed)  # Absolute change in Speed
  ) %>%
  ungroup() #%>%  # Ungroup to apply filtering to the full dataset
  #filter(delta_speed >= -2.5, delta_speed <= 2.5)   # Remove extreme speed changes


p<-ggplot(df, aes(x = delta_AR, y = delta_speed)) +
  geom_point(alpha = 0.6) +
  ylim(-2.5,2.5)+
  xlim(-2.5,2.5)+
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  #trend line
  labs(title = "Change in Aspect Ratio vs. Change in Speed",
       x = "Δ Aspect Ratio",
       y = "Δ Speed") +
  theme_minimal()
p
cor_test <- cor.test(df$delta_AR, df$delta_speed, use = "complete.obs")
print(cor_test)

# Summary Stats -----------------------------------------------------------
p <- ggplot(summary_stats, aes(y = mean_APRatio, x = mean_speed, color = group)) + 
  geom_point(size = 1, alpha = 0.7) +  # Scatter plot points
  scale_colour_brewer(palette = "Set2") +  # Color by group
  ylim(0, 5) +  # Set Y-axis limits
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank()
  ) + 
  ylab("y") + 
  xlab("x")
p

p <- ggplot(df, aes(x = mean_speed, y = mean_AR, group = temp, color = temp)) + 
  geom_line(alpha = 0.35) +  # Draw lines for each ID
  #geom_point(alpha = 0.7) +  # Optional: Show points for clarity
  geom_smooth(method = "loess", se = FALSE, alpha = 0.7) +
  xlim(0, 14) +
  ylim(0, 4.5) +
  labs(
    title = "Speed vs. Aspect Ratio",
    x = "speed",
    y = "Aspect Ratio"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
p



df_avg <- df %>%
  group_by(temp, ID) %>%  # Group by temperature and ID
  summarise(
    mean_speed = mean(mean_speed, na.rm = TRUE),  # Average speed for each ID and temperature
    mean_AR = mean(mean_AR, na.rm = TRUE),  # Average AR for each ID and temperature
    sd_speed = sd(mean_speed, na.rm = TRUE),  # Standard deviation of speed
    sd_AR = sd(mean_AR, na.rm = TRUE)  # Standard deviation of AR
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


