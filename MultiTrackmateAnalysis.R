#multicell trackmate analysis
#this df needs time, ID, Xpos, Ypos columns, required to work.
#if xml file was exported from trackmate delete that top left cell so you have headers
#make sure your column names match
#all plotting here is for analysis visualization purposes, i'd save everything as a csv
#and compare across groups later

#Step 1 is to import your data with the name data and with headers checked#
library(dplyr)
library(viridis)
# Data Frame Header match -------------------------------------------------
#column rename skip if you manually renamed, which can be easier
#df <- data %>%
 # rename_with(~ case_when(
  #  grepl("@t$", .) ~ "time",
   # grepl("@x$", .) ~ "Xpos",
    #grepl("@y$", .) ~ "Ypos",
    #grepl("id$", .) ~ "ID",
    #TRUE ~ .  # keep original names for other columns
  #))
colnames(data) #dbl check your column names
t_int<-5 #check and make sure this is the actual time int
# Extract Data ------------------------------------------------------------
#data<-df
#View(data)

# Plot all cell trajectories and #show the cell path
#instead of cartesian coordinate micron based system convert to pixels 
#convert from microns back to pixels for image comparison 
pixel_width <- 0.16  # image scale change this for the image**************
pixel_height <- 0.16  # 
df_all <- data %>%
  mutate(
    X_pixel = Xpos / pixel_width,
    Y_pixel = Ypos / pixel_height
  )
p <- ggplot(df_all, aes(x = X_pixel, y = Y_pixel, group = ID, color = as.factor(ID))) +
  geom_path() +  # Draws paths for each cell
  geom_point(size = 0.5) +  # Smaller points for clarity
  labs(title = "Cell Trajectories", x = "X Position", y = "Y Position", color = "Cell ID") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend too many IDs plotting aesthetics
p

#get instantaneous displacement at each step, euclidean distance
df_all <- data %>% mutate(realtime = as.numeric(time) * as.numeric(t_int)) %>%
  group_by(ID) %>%
  mutate(
    displacement = sqrt((Xpos - lag(Xpos, default = first(Xpos)))^2 +
                          (Ypos - lag(Ypos, default = first(Ypos)))^2),
    time_diff = realtime - lag(realtime, default = first(realtime)),  # use original time
    speed = displacement / (time_diff + 1e-9)  # Prevent divide by zero at first time point
  ) %>%
  ungroup()

View(df_all)

# Distance Traveled -------------------------------------------------------
# sum up displacement
df_all <- df_all %>%
  group_by(ID) %>%
  mutate(
    total_distance = sum(displacement, na.rm = TRUE),
    total_time = max(realtime, na.rm = TRUE) - min(realtime, na.rm = TRUE),
    mean_speed = total_distance / total_time,
    total_displacement = sqrt((last(Xpos) - first(Xpos))^2 + 
                                (last(Ypos) - first(Ypos))^2)
  ) %>%
  ungroup()
#View(df_all)

# Calculate Cell Turning --------------------------------------------------
# first calculate displacement dx,dy and the angle  in radians btw points store in DF 
# have to make the first value zero or null to keep everything the same length but that does mean the color coding is off by one
#does that turning threshold seem low? it has to be lower than you think bc the xy pos are centroids
#if this is less that ideal i get it, i have a more computationally $$ solution we could explore
df_all <- df_all %>%
  group_by(ID) %>%
  mutate(
    dx = c(0, diff(Xpos)),  # change in X position
    dy = c(0, diff(Ypos)),  # change in Y position
    angle = atan2(dy, dx),  # calculate angle in radians
    angle_change = c(0, diff(angle)),  # change in angle between frames, add 0 to first to maintain column length
    angle_change_deg = angle_change * (180 / pi), #calc angle in radians
    change_direction = abs(angle_change) > (15 * pi / 180)  # Apply threshold
  ) %>%
  ungroup()

# Plot the trajectory with direction changes
p <- ggplot(df_all, aes(x = Xpos, y = Ypos)) +   
  #geom_path() +   
  geom_point(aes(color = change_direction), size = 2) +  # Direction change
  labs(title = "Cell Trajectories with Direction Changes", x = "X Position", y = "Y Position") +  
  theme_minimal() +
  scale_color_manual(values = c("black", "red"))# red indicates direction change
p
#output as directional changes per minute
# total time
df_all <- df_all %>%
  group_by(ID) %>%
  mutate(
    tot_time = (max(realtime, na.rm = TRUE) - min(realtime, na.rm = TRUE)) / 60,
    num_change = sum(change_direction == TRUE, na.rm = TRUE),
    dir_change_rate = num_change / tot_time  # Compute within the same step
  ) %>%
  ungroup()

# unique directional change rates per id
dir_change_rate_summary <- df_all %>%
  select(ID, dir_change_rate) %>%
  distinct()

# output
print(dir_change_rate_summary)
View(dir_change_rate_summary)

# summarize per id
summary_stats <- df_all %>%
  select(ID, total_distance, mean_speed, total_displacement, dir_change_rate) %>%
  distinct()

View(summary_stats)

# Save the tracked data, manually set working dir
write.csv(summary_stats, "tracked_analysis.csv", row.names = FALSE)

# Perimeter to Area (Cell Shape Analysis Only) ----------------------------
df_all <- data %>%
  mutate(
    APRatio = area / perim
  )
summary_stats <- df_all %>%
  group_by(ID) %>%
  summarize(mean_APRatio = mean(APRatio, na.rm = TRUE))

# view summary of all data per id
summary_stats <- df_all %>%
  select(ID, total_distance, mean_speed, total_displacement,mean_APRatio) %>%
  distinct()
View(summary_stats)
#velocity vs Area/Perimeter ratio group by multiple groups
p <- ggplot(df_all, aes(x = APRatio, y = mean_speed)) + 
  geom_point() +  # Simple scatter plot
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

p

# Save the tracked data, manually set working dir
write.csv(summary_stats, "tracked_analysis.csv", row.names = FALSE)
