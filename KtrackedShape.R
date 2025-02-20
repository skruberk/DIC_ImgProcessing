#K-Tracked Particles
#restart
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

#calculate step distance and filter 
# calc step_dist across all rows
df <- df %>%
  arrange(ID, Frame) %>%
  group_by(ID) %>%
  mutate(step_dist = sqrt((Xpos - lag(Xpos))^2 + (Ypos - lag(Ypos))^2)) %>%
  ungroup()
#View(df)
#add time column 
#time interval ******change this if it's not 5*******
time_int=5 #is time interval 5? double check. could pull this from df alternately. this is t_int in the other script keep separate?
df <- df %>%
  mutate(time = Frame * time_int) 
df <- df %>%
  arrange(ID, Frame) %>%
  group_by(ID) %>%
  mutate(
    step_dist = sqrt((Xpos - lag(Xpos))^2 + (Ypos - lag(Ypos))^2),
    step_dist = replace_na(step_dist, 0)  # Replace first NA with 0
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
#so plot cannot be directly overlaid on original image which is
#in pixel space this is in a separate coordinate system so you have to convert?
#worth doing a conversion ?
#high perimeter to area ratio would tell you how lamellipodial like the outline is
df$group <- "60c70c"  # name group in df
p<- ggplot(df, aes(x=group, y=AR))+ ylim(0,15)+ geom_boxplot(outlier.shape = NA)+ geom_beeswarm(dodge.width=0.15,aes()) + scale_colour_brewer(palette = "Set2") +#color by group eventully
 theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p


# APR vs AR ----------------------------------------------------------
df<- data %>%
  mutate(
    APRatio = area / perim
  )
p<- ggplot(df, aes(x = AR, y = APRatio)) +
  geom_point(alpha = 0.7, color = "blue") +  #change this to be colored by group for multi comparison across temps
  labs(title = "Perimeter/Area vs. Aspect Ratio",
       x = "Aspect Ratio",
       y = "Area to Perimeter") +
  #ylim(0,15)+
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))
p

# save velocity aspect ratio data
write.csv(df, "tracked_particleshape_60c70c.csv", row.names = FALSE)
#if importing into MultiTrackmateAnalysis run
data<-df
