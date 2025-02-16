#K-Tracked Particles
#restart
rm(list = ls())
rm(list = ls(all.names = TRUE))


library(dplyr)
library(FNN)  # Fast Nearest Neighbors package
library(viridis)
df<-data
# Define parameters
k_neighbors <- 3 
#start with 3
#number of neighbors to check 1-3 seems to work for well sep particles, low noise
#low noise, not lots of neighbors, 4-6 and higher: fast and erratic movement lots of 
#neighbors noisy tracking 
max_distance <- 5  # Maximum allowed displacement
gap_tolerance <- 3  # Number of frames a particle can disappear before being reassigned

# Initialize Particle_ID column
df$ID <- NA
particle_counter <- 1

# Assign initial particle IDs for the first frame
df$ID[df$Frame == min(df$Frame)] <- 1:sum(df$Frame == min(df$Frame))

# Loop through frames
for (frame in unique(df$Frame)[-1]) {
  current <- df %>% filter(Frame == frame)
  #also search for matches in previous frames which is gap tolerance
  previous <- df %>% filter(Frame >= (frame - gap_tolerance) & Frame < frame)
  
  if (nrow(previous) == 0) next  # skip if no particles in the previous frame
  
  # Use KNN to find the closest matches
  nn_results <- get.knnx(data = previous[, c("Xpos", "Ypos")], #frames previous particles
                         query = current[, c("Xpos", "Ypos")], #match particles in current
                         k = k_neighbors) #finds k nearest matches in prev frame
  assigned_IDs <- c()  # Track already used IDs in this frame
  for (i in 1:nrow(current)) {
    distances <- nn_results$nn.dist[i, ]  # get distances for k neighbors
    indices <- nn_results$nn.index[i, ]  # get indices of k neighbors in previous frame
    
    valid_match <- which(distances < max_distance)  # filter valid matches
    if (length(valid_match) > 0) {
      best_match <- indices[valid_match[1]]  # Choose first valid match
      matched_ID <- previous$ID[best_match]
      
      # Ensure ID has not already been assigned in this frame
      if (!(matched_ID %in% assigned_IDs)) {
        df$ID[df$Frame == frame & df$Xpos == current$Xpos[i] & df$Ypos == current$Ypos[i]] <- matched_ID
        assigned_IDs <- c(assigned_IDs, matched_ID)  # Mark ID as used
      } else {
        # If ID is already taken, assign a new one
        df$ID[df$Frame == frame & df$Xpos == current$Xpos[i] & df$Ypos == current$Ypos[i]] <- particle_counter
        particle_counter <- particle_counter + 1
      }
    } else {
      # If no match is found, assign a new Particle ID
      df$ID[df$Frame == frame & df$Xpos == current$Xpos[i] & df$Ypos == current$Ypos[i]] <- particle_counter
      particle_counter <- particle_counter + 1
    }
  }
}
#calculate step distance and filter 
# calc step_dist across all rows
df <- df %>%
  arrange(ID, Frame) %>%
  group_by(ID) %>%
  mutate(step_dist = sqrt((Xpos - lag(Xpos))^2 + (Ypos - lag(Ypos))^2)) %>%
  ungroup()
View(df)

#convert from microns back to pixels for image comparison 
pixel_width <- 0.1602  # image scale
pixel_height <- 0.1602  # 

#instead of cartesian coordinate micron based system convert to pixels 
df <- df %>%
  mutate(
    X_pixel = Xpos / pixel_width,
    Y_pixel = Ypos / pixel_height
  )

p <- ggplot(df, aes(x = Xpos, y = Ypos, group = ID, color = AR)) +    
  #geom_path(size = 1) +    # Plot trajectory
  geom_point(size = 2) +   # Mark each recorded position
  scale_color_viridis_c(option = "plasma", name = "Aspect Ratio") +  
  labs(title = "Particle Trajectories by Aspect Ratio", 
       x = "X Position", 
       y = "Y Position") +  
  theme_minimal()
p
#note: ggplot plot cannot be directly overlaid on original image which is
#in pixel space this is in a separate coordinate system so you have to convert



# Save the tracked data
write.csv(df, "tracked_particles_knn.csv", row.names = FALSE)

