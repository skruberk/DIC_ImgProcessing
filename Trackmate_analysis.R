#trackmate data analysis, currently the best spot detection is between 50-150
#frames which is between 4.5minutes and 12.5 minutes of imaging
#restart
rm(list = ls())
rm(list = ls(all.names = TRUE))

#libraries
library(dplyr) #use install.packages("dplyr") if not installed already
library(ggplot2) #install if not
library(geosphere) #install if not

# load datausing import dataset button in R studio 
df<-data
View(df)
#this df needs time, interval, Xpos, Ypos columns, you have to do this for this to work 
# Extract Data ------------------------------------------------------------
# just for Tracking ID 1, if this analysis works will change to loop through
#each ID 
df$ID <- as.numeric(as.character(data$ID))
df_filtered <- subset(data, ID == 1)
View(df_filtered)
#show the cell path
p<-ggplot(df_filtered, aes(x = Xpos, y = Ypos)) +
  geom_path(color = "purple") +
  geom_point(color = "black") +
  labs(title = "Cell Trajectory", x = "X Position", y = "Y Position") +
  theme_minimal()
p
#get instantaneous displacement at each step, euclidean distance
distance_traveled <- df_filtered %>%
  mutate(
    displacement = sqrt((Xpos - lag(Xpos, default = first(Xpos)))^2 +
                          (Ypos - lag(Ypos, default = first(Ypos)))^2),
    time_diff = time - lag(time, default = first(time)),
    speed = displacement / time_diff
  )
View(distance_traveled)

# Mean Squared Displacement -----------------------------------------------
# MSD can give us idea how much space the cell is exploring ie. if 
#cell is turning and moving randomly, if cell moves in a perfect straight line
#squared displacement at each time interval, average, plot the result against time. 
calc_MSD <- function(distance_traveled) {
  max_lag <- nrow(distance_traveled) - 1  # Get max time lag correctly
  msd_values <- numeric(max_lag)  # Storage for MSD at each lag
  
  for (lag in 1:max_lag) {
    displacements <- (distance_traveled$Xpos[(lag + 1):nrow(distance_traveled)] - distance_traveled$Xpos[1:(nrow(distance_traveled) - lag)])^2 + 
      (distance_traveled$Ypos[(lag + 1):nrow(distance_traveled)] - distance_traveled$Ypos[1:(nrow(distance_traveled) - lag)])^2
    
    msd_values[lag] <- mean(displacements, na.rm = TRUE)  # Average over all displacements
  }
  return(data.frame(lag = 1:max_lag, MSD = msd_values))
}
# call function
msd_results <- calc_MSD(distance_traveled)
mean_MSD <- mean(msd_results$MSD, na.rm = TRUE)
print(mean_MSD)
# View results
View(msd_results)
#head(msd_results)  
p<-ggplot(msd_results, aes(x = lag, y = MSD)) +
  geom_point() +  # Plot points
  geom_line() +   # Connect points with a line
  scale_x_log10() + scale_y_log10() +  # Log-log scale for both axes
  labs(title = "Mean Squared Displacement (MSD) vs. Time Lag",
       x = "Time Lag (Δt)",
       y = "MSD um^2") +
  theme_minimal()
p  

# Distance Traveled -------------------------------------------------------
# Handle missing values and sum up displacement
sum_distance <- sum(distance_traveled$displacement, na.rm = TRUE)
last_time <- last(distance_traveled$time)
step_number=last_time/5 #change this if the time interval isn't five or hard code it based on 
#time_diff variable
mean_speed=sum_distance/last_time
total_distance=(mean_speed*last_time)
print(mean_speed)
print(sum_distance)
print(total_distance) #just double checking should be same as total displacement
#total displacement straight-line distance between the initial position and 
#final position of cell
total_displacement= sqrt((last(distance_traveled$Xpos)-first(distance_traveled$Xpos))^2 + (last(distance_traveled$Ypos)-first(distance_traveled$Ypos))^2)
print(total_displacement)

# summary dataframe for all variables
summary_stats <- data.frame(
  mean_MSD = mean_MSD,
  total_displacement = total_displacement,
  mean_speed=mean_speed,
  sum_distance=sum_distance
)
View(summary_stats)

# Calculate Cell Turning --------------------------------------------------
# first calculate displacement dx,dy and the angle  in radians btw points store in DF 
# have to make the first value zero or null to keep everything the same length but that does mean the color coding is off by one
  distance_traveled <- df_filtered %>%
  mutate(
    dx = c(0, diff(Xpos)),  # Change in X position, first value set to 0 otherwise you get a shift
    dy = c(0, diff(Ypos)),   # Change in Y position, first value set to 0
    angle = atan2(dy, dx),  # Calculate angle in radians
    angle_change = c(0, diff(angle)),  # Change in angle between frames
    angle_change_deg = angle_change * (180 / pi),
    change_direction = abs(angle_change) > (5 * pi / 180)  # Apply threshold
  )

# Plot the trajectory
p <- ggplot(distance_traveled, aes(x = Xpos, y = Ypos)) +   
  geom_path(color = "purple") +   
  geom_point(aes(color = change_direction), size = 2) +  # does direction change?
  labs(title = "Cell Trajectory with Direction Changes", x = "X Position", y = "Y Position") + 
  theme_minimal() +
  scale_color_manual(values = c("black", "red"))  # Red points indicate direction change
print(p)
View(distance_traveled)
#output as directional changes per minute
# total time
tot_time <- (max(df_filtered$time, na.rm = TRUE) - min(df_filtered$time, na.rm = TRUE)) / 60  
# Count the number of direction changes
num_change <- sum(distance_traveled$change_direction, na.rm = TRUE)

# Compute direction changes per minute
dir_change_rate <- num_change / tot_time
print(dir_change_rate)
# Save the tracked data
write.csv(distance_traveled, "tracked_analysis.csv", row.names = FALSE)
