#multicell trackmate analysis
#this df needs time, ID, Xpos, Ypos columns, required to work.
#if xml file was exported from trackmate delete that top left cell so you have headers
#make sure your column names match
#all plotting here is for analysis visualization purposes, i'd save everything as a csv
#and compare across groups later
rm(list = ls())
rm(list = ls(all.names = TRUE))
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
colnames(data) #dbl check your column names need ID Ypos Xpos time all case sensitive
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
df_all <- data %>% 
  mutate(realtime = as.numeric(time) * as.numeric(t_int)) %>%
  group_by(ID) %>%
  mutate(
    #net_displacement = sqrt((Xpos - lag(Xpos, default = first(Xpos)))^2 + 
     #                         (Ypos - lag(Ypos, default = first(Ypos)))^2),
    stepwise_displacement = sqrt((Xpos - lag(Xpos))^2 + 
                                   (Ypos - lag(Ypos))^2),
    total_distance = sum(stepwise_displacement, na.rm = TRUE), # Total path length
    total_displacement = sqrt((last(Xpos) - first(Xpos))^2 + (last(Ypos) - first(Ypos))^2),  
    total_time = max(realtime, na.rm = TRUE) - min(realtime, na.rm = TRUE),  # Total duration
    speed = total_distance / (total_time + 1e-9)  # Prevent divide by zero
  ) %>%
  ungroup()


# Calculate Cell Turning aka Angular Speed --------------------------------------------------
# calculate the smallest difference between two angles using vectors instead
angle_difference <- function(angle1, angle2) { 
  diff <- angle2 - angle1
  diff <- atan2(sin(diff), cos(diff))  # Ensures result is between -π and π
  return(diff * (180 / pi))  # Convert to degrees
}

df_all <- df_all %>% 
  group_by(ID) %>% 
  mutate(
    dx = Xpos - lag(Xpos, default = Xpos[1]),
    dy = Ypos - lag(Ypos, default = Ypos[1]), # displacement vector
    angle = atan2(dy, dx), # compute angle in radians btw pos x axis and the vector dx,dy
    # use function to compute angle changes
    angle_change_deg = c(NA, mapply(angle_difference, head(angle, -1), tail(angle, -1)))  # angle change calculation
  ) %>% 
  ungroup()


# plot direction changes by angle instead 
p<-ggplot(df_all, aes(x = Xpos, y = Ypos, color = abs(angle_change_deg))) +
  geom_point(size = 2) +
  scale_color_gradient(low = "blue", high = "red",limits = c(0, 360)) +  # can make this a better scale too
  labs(title = "Cell Directional Changes",
       x = "X Position",
       y = "Y Position",
       color = "Angle Change (°)") +
  theme_minimal()
p

#output as total changes per minute
# total time
summary <- df_all %>%
  group_by(ID) %>%
  summarize(
    tot_time = (max(realtime, na.rm = TRUE) - min(realtime, na.rm = TRUE)) / 60,
    angle_change = sum(abs(angle_change_deg), na.rm = TRUE), #make sure not cancel each other out 
    full_rev = angle_change / 360, 
    dir_change_rate = (full_rev / tot_time)/60  # compute within the same step
  )%>%
  ungroup() #not sure this is the best output? it's just total degrees/min maybe divide by 360 for full turn? 
#View(dir_change_rate_summary)
# unique directional change rates per id
#summary_stats <- df_all %>%
 # select(ID, dir_change_rate) %>%
#  distinct()

# output
#View(dir_change_rate_summary)

# summarize per id
summary_stats <- df_all %>%
  select(ID, speed, total_displacement, total_distance) %>%
  distinct()
summary_stats <- summary_stats %>%  
  left_join(summary %>% select(ID, dir_change_rate) %>% distinct(), by = "ID")
View(summary_stats)


summary_stats$group <- "FOV_2_1-240_57C"  # name group in df
# Save the tracked data, manually set working dir
write.csv(summary_stats, "FOV_2_1-240_57C.csv", row.names = FALSE)

# Save the tracked data, manually set working dir
write.csv(summary_stats, "tracked_analysis.csv", row.names = FALSE)

# sum up displacement
df_all <- df_all %>%
  group_by(ID) %>%
  mutate(
    total_distance = sum(net_displacement, na.rm = TRUE),
    total_time = max(realtime, na.rm = TRUE) - min(realtime, na.rm = TRUE),
    mean_speed = total_distance / total_time,
    total_displacement = sqrt((last(Xpos) - first(Xpos))^2 + 
                                (last(Ypos) - first(Ypos))^2)
  ) %>%
  ungroup()
View(df_all)