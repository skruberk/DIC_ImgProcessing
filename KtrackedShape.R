#K-Tracked Particles
#xpos and ypos outputs are in microns, no need for conversion 
#restart
rm(list = ls())
rm(list = ls(all.names = TRUE))

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


##########
library(dplyr)
library(FNN)  # Fast Nearest Neighbors package
library(viridis) #color aesthetics

#DO DATA IMPORT HERE##################dataimport 

# NAME OF DATASET 
name <- "70C_3" 
df<-data
df$temp <- "60"  # name group in df
# Define parameters
k_neighbors <- 2 #start with 3
#number of neighbors to check 1-3 seems to work for well sep particles, low noise
#low noise, not lots of neighbors, 4-6 and higher: fast and erratic movement lots of 
#neighbors noisy tracking 
max_distance <- 3  # Maximum allowed displacement, is it moving fast? up this #:25 isn't unreasonable for v fast
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
        df$ID[df$Frame == frame & near(df$Xpos, current$Xpos[i]) & near(df$Ypos, current$Ypos[i])] <- matched_ID
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
  filter(n() > 5)%>%   # remove particles appearing in fewer than x frames
  ungroup()

t_int=1 #seconds/frame
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
  # filter out rows where stepwise displacement is greater than X
  filter(stepwise_displacement <= 5 | stepwise_displacement == 0) %>%
  mutate(
    # calculate stepwise speed
    stepwise_speed = stepwise_displacement * (1 / t_int)  # Speed per step
  ) %>%
  ungroup()
#summary(df$Xpos)
#summary(df$Ypos)
#see the trajectories (note that the origin in fiji is at the top left corner)
p <- ggplot(df, aes(x = Xpos, y = Ypos, color = factor(ID), group = factor(ID))) +
  geom_path() +
  xlim(0,325)+
  ylim(0,325)+
  labs(
    title = "Particle Trajectories",
    x = "X Position",
    y = "Y Position",
    color = "Particle ID"
  ) +
  scale_color_viridis_d(option = "D") +
  scale_y_reverse() +   # <-- This flips the Y-axis
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )

p


######track quality control#####
# order data correctly
df <- df %>% arrange(ID, frame)

# Calculate stepwise displacement sum for each track
track_length <- df %>%
  group_by(ID) %>%
  summarise(track_length = sum(stepwise_displacement, na.rm = TRUE))

# Count how often an ID switches for the same particle across consecutive frames
id_changes <- df %>%
  arrange(frame, ID) %>%  # Ensure correct time ordering
  group_by(ID) %>%
  mutate(prev_ID = lag(ID, default = first(ID)),
         id_switch = ID != prev_ID) %>%
  summarise(id_changes = sum(id_switch, na.rm = TRUE))

# Merge track length and reassignment count
track_quality_metrics <- track_length %>%
  left_join(id_changes, by = "ID")

# output QC results
write.csv(track_quality_metrics, paste0(name, "_trajectoryQC.csv"), row.names = FALSE)

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
df <- df %>%
  mutate(AR = df$Major / df$Minor)
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

