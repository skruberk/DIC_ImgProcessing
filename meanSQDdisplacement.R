#mean squared displacement
library(dplyr)
library(broom)
library(tidyr)
library(purrr)

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

#which dataframe: this is raw combined outputs, nothing summarized 
df<-data
# How many groups per temp before filtering?
df %>%
  group_by(temp, group, ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  count(temp, name = "n_tracks_before")

# recode ------------------------------------------------------------------
# filter and map 
#df<-data
df <- df %>% filter(stepwise_displacement < 0.55) #anything over this is detached

# How many groups per temp before filtering?
df %>%
  group_by(temp, group, ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  count(temp, name = "n_tracks_before")


# Compute the dynamic max_tau as 1/4th of the shortest track length
max_tau <- floor(median(df %>% group_by(group,ID) %>% summarise(n = n()) %>% pull(n)))
#max_tau <- floor(min(df %>% group_by(group, ID) %>% summarise(n = n()) %>% pull(n)))
#max_tau : the maximum time lag over which you calculate displacements

valid_groups <- df %>%
  group_by(group, temp,ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > max_tau)

msd_df <- df %>%
  group_by(group, temp, ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > max_tau) %>%
  mutate(group_id = paste(group, ID, sep = "_")) %>%
  pmap_dfr(function(group, temp, ID, n, group_id) {
    track <- df %>%
      filter(group == !!group, ID == !!ID) %>%
      arrange(Frame)
  #max_tau is computed based on track length to avoid bias in particle tracking 
    tibble(
      group = group,
      temp = temp,
      ID = ID,
      tau = 1:max_tau,
      MSD = map_dbl(1:max_tau, function(tau) {
        disp <- track %>%
          mutate(
            X_shift = lead(Xpos, tau),
            Y_shift = lead(Ypos, tau)
          ) %>%
          filter(!is.na(X_shift)) %>%
          mutate(sq_disp = (X_shift - Xpos)^2 + (Y_shift - Ypos)^2) %>%
          summarise(msd = mean(sq_disp, na.rm = TRUE)) %>%
          pull(msd)
        return(disp)
      })
    )
  })

msd_overall <- msd_df %>%
  group_by(temp, tau) %>%
  summarise(
    MSD_mean = mean(MSD, na.rm = TRUE),
    MSD_se = sd(MSD, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(time = tau / max(tau))  # Normalize to 0–1, only works if time interval is 1s which it is 

df %>%
  group_by(temp, group,ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  count(temp, name = "tracks_per_temp")


# net displacement --------------------------------------------------------

df<-data
# Compute net displacement for each track (group + ID)
net_disp_df <- df %>%
  group_by(temp, group, ID) %>%
  summarise(
    start_x = first(Xpos),
    start_y = first(Ypos),
    end_x = last(Xpos),
    end_y = last(Ypos),
    net_displacement = sqrt((end_x - start_x)^2 + (end_y - start_y)^2),
    .groups = "drop"
  )

# by temp
net_disp_summary <- net_disp_df %>%
  group_by(temp) %>%
  summarise(
    mean_net_disp = mean(net_displacement, na.rm = TRUE),
    median_net_disp = median(net_displacement, na.rm = TRUE),
    sd_net_disp = sd(net_displacement, na.rm = TRUE),
    n_tracks = n(),
    .groups = "drop"
  )

# summary table
net_disp<-as.data.frame(net_disp_summary)
write.csv(net_disp, "net_displacement.csv")

# straightness ratio: net displacement, path length, and straightness per track
disp_metrics_df <- df %>%
  group_by(temp, group, ID) %>%
  summarise(
    start_x = first(Xpos),
    start_y = first(Ypos),
    end_x = last(Xpos),
    end_y = last(Ypos),
    net_displacement = sqrt((end_x - start_x)^2 + (end_y - start_y)^2),
    path_length = sum(sqrt(diff(Xpos)^2 + diff(Ypos)^2), na.rm = TRUE),
    straightness = ifelse(path_length > 0, net_displacement / path_length, NA),
    .groups = "drop"
  )

# Compute reference mean straightness 
reference_mean <- disp_metrics_df %>%
  filter(temp %in% c(25, 70)) %>%
  summarise(ref_mean = mean(straightness, na.rm = TRUE)) %>%
  pull(ref_mean)

# Normalize straightness
disp_metrics_df <- disp_metrics_df %>%
  mutate(norm_straightness = straightness / reference_mean)

# normalize summ
straightness_summary <- disp_metrics_df %>%
  group_by(temp) %>%
  summarise(
    mean_straightness = mean(straightness, na.rm = TRUE),
    median_straightness = median(straightness, na.rm = TRUE),
    mean_norm_straightness = mean(norm_straightness, na.rm = TRUE),
    median_norm_straightness = median(norm_straightness, na.rm = TRUE),
    n_tracks = n(),
    .groups = "drop"
  )
View(straightness_summary)
#out
write.csv(disp_metrics_df, "raw_normalized_straightness.csv", row.names = FALSE)

write.csv(straightness_summary, "straightness.csv", row.names = FALSE)

# plot --------------------------------------------------------------------
p<-ggplot(msd_overall, aes(x = time, y = MSD_mean, color = as.factor(temp))) +
  geom_line(size = 1) + 
  ylim(0,750)+
  geom_ribbon(aes(ymin = MSD_mean - MSD_se, ymax = MSD_mean + MSD_se, fill = as.factor(temp)), alpha = 0.08) +
  geom_point(size = 0) +
  labs(
    title = "MSD Comparison Across Temperatures",
    x = "Time (seconds)",
    y = "Mean Squared Displacement (MSD)",
    color = "Temperature",
    fill = "Temperature"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  #scale_color_viridis_d() +  # Viridis color scale for lines
  #scale_fill_viridis_d() +   # Viridis color scale for ribbons
  theme_minimal() +                          
  theme(
    text = element_text(size = 14),
    panel.grid = element_blank(),    # Remove gridlines
    axis.line = element_line(color = "black"),  # Keep axis lines
    legend.title = element_blank()   # Remove legend title
  ) 
p


#facet wrap
p <- ggplot(msd_overall, aes(x = time, y = MSD_mean, color = as.factor(temp))) +
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin = MSD_mean - MSD_se, ymax = MSD_mean + MSD_se, fill = as.factor(temp)), alpha = 0.2) +
  geom_point(size = 0) +
  labs(
    title = "MSD Comparison Across Temperatures",
    x = "Time (seconds)",
    y = "Mean Squared Displacement (MSD)",
    color = "Temperature",
    fill = "Temperature"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  #scale_color_viridis_d() +
  #scale_fill_viridis_d() +
  facet_wrap(~ temp, ncol = 4, scales = "free") +
  coord_cartesian(ylim = c(0, 600)) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.title = element_blank()
  )

p

#get slopes log-transform tau and MSD, filter invalid points
msd_slopes <- msd_df %>%
  filter(MSD > 0) %>%  # skip tau=1 if noisy, avoid log(0)
  mutate(
    log_tau = log(tau),
    log_MSD = log(MSD)
  ) %>%
  group_by(temp, group, ID) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(log_MSD ~ log_tau, data = .x)),
    fit_summary = map(fit, tidy)
  ) %>%
  unnest(fit_summary) %>%
  filter(term == "log_tau") %>%
  select(temp, group, ID, estimate, std.error, p.value) %>%
  rename(
    alpha = estimate,
    alpha_se = std.error,
    alpha_p = p.value
  )
View(msd_slopes)
alpha_by_temp_group <- msd_slopes %>%
  group_by(group,temp) %>%
  summarise(
    mean_alpha = mean(alpha, na.rm = TRUE),
    se_alpha = sd(alpha, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )
View(alpha_by_temp_group)
write.csv(alpha_by_temp_group, "MSD_alpha_summary.csv",row.names=TRUE)


msd_by_temp_loglog <- msd_overall %>%
  filter(tau > 1, MSD_mean > 0) %>%
  group_by(temp, tau) %>%
  mutate(
    log_tau = log10(tau),
    log_MSD = log10(MSD_mean)
  )

p<-ggplot(msd_by_temp_loglog, aes(x = log_tau, y = log_MSD, color = as.factor(temp))) +
  geom_line(size = 1) +
  labs(
    title = "Average Log–Log MSD per Temperature",
    x = expression(log[10](tau)),
    y = expression(log[10](Mean~MSD)),
    color = "Temperature"
  ) +
  scale_color_viridis_d() +
  theme_minimal()
p


# log-log plot ------------------------------------------------------------

 
# convert to log space for each point, remove invalid values
loglog_per_point <- msd_df %>%
  filter(MSD > 0, tau > 1) %>%  # avoid log(0) and tau=1 if noisy
  mutate(
    log_tau = log10(tau),
    log_MSD = log10(MSD)
  )

# Average log–log MSD per track, tau
loglog_per_track <- loglog_per_point %>%
  group_by(temp, ID, tau) %>%
  summarise(
    log_tau = mean(log_tau),  # constant across ID/tau anyway
    log_MSD = mean(log_MSD),
    .groups = "drop"
  )

#  average across tracks for each (temp, tau)
loglog_avg <- loglog_per_track %>%
  group_by(temp, tau, log_tau) %>%
  summarise(
    mean_log_MSD = mean(log_MSD, na.rm = TRUE),
    se_log_MSD = sd(log_MSD, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

p <- ggplot(loglog_avg, aes(x = log_tau, y = mean_log_MSD, color = as.factor(temp))) +
  geom_line(size = 1) +
  geom_ribbon(aes(
    ymin = mean_log_MSD - se_log_MSD,
    ymax = mean_log_MSD + se_log_MSD,
    fill = as.factor(temp)
  ), alpha = 0.2, color = NA) +
  labs(
    title = "Mean Log–Log MSD Across Tracks",
    x = expression(log[10](tau)),
    y = expression(mean~log[10](MSD)),
    color = "Temperature",
    fill = "Temperature"
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_minimal()

p



# convex hull -------------------------------------------------------------

library(purrr)
library(tibble)

convex_metrics <- df %>%
  filter(stepwise_displacement < 0.55) %>%
  group_by(group, temp, ID) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n > max_tau) %>%
  mutate(group_id = paste(group, ID, sep = "_")) %>%
  pmap_dfr(function(group, temp, ID, n, group_id) {
    # Extract track points
    track <- df %>%
      filter(group == !!group, ID == !!ID) %>%
      arrange(Frame)
    
    # If there are fewer than 3 points, area/perimeter = NA
    if(nrow(track) < 3) {
      return(tibble(
        group = group,
        temp = temp,
        ID = ID,
        convex_area = NA_real_,
        convex_perimeter = NA_real_
      ))
    }
    
    # convex hull
    hull_idx <- chull(track$Xpos, track$Ypos) #chull finds point indices that make it convex
    hull_points <- track[c(hull_idx, hull_idx[1]), ]  # close polygon-extract rows from track
    #have to do hull_idx[1] to close it and repeat that first point
    
    # shoelace formula computes polygon area from vertex coordinates
    #get all x except 1st one, all y except last one
    x <- hull_points$Xpos #make the vectors 
    y <- hull_points$Ypos
    area <- 0.5 * abs(sum(x[-1] * y[-length(y)] - x[-length(x)] * y[-1]))
    
    # sum of distances between consecutive hull points 
    perimeter <- sum(sqrt(diff(x)^2 + diff(y)^2))
    
    tibble(
      group = group,
      temp = temp,
      ID = ID,
      convex_area = area,
      convex_perimeter = perimeter
    )
  })
View(convex_metrics)
write.csv(convex_metrics, "raw_convex_met.csv",row.names=TRUE)
convex_summary <- convex_metrics %>%
  group_by(temp) %>%
  summarise(
    mean_area = mean(convex_area, na.rm = TRUE),
    sd_area = sd(convex_area, na.rm = TRUE),
    se_area = sd_area / sqrt(sum(!is.na(convex_area))),
    
    median_area = median(convex_area, na.rm = TRUE),
    iqr_area = IQR(convex_area, na.rm = TRUE),
    
    mean_perimeter = mean(convex_perimeter, na.rm = TRUE),
    sd_perimeter = sd(convex_perimeter, na.rm = TRUE),
    se_perimeter = sd_perimeter / sqrt(sum(!is.na(convex_perimeter))),
    
    median_perimeter = median(convex_perimeter, na.rm = TRUE),
    iqr_perimeter = IQR(convex_perimeter, na.rm = TRUE),
    
    n_tracks = n(),
    .groups = "drop"
  )
View(convex_summary)
write.csv(convex_summary, "convex_metrics.csv",row.names=TRUE)
