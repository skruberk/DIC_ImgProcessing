#single MSD with normalization 
#mean squared displacement
library(dplyr)
library(purrr)

#reset
rm(list = ls())
rm(list = ls(all.names = TRUE))
#import data here#####
df<-data
df$temp<-"66"
# Remove tracking artifacts
#############################this should already be done in tracking
#df <- df %>% filter(stepwise_displacement <= 10) #this does nothing so it has

# dynamic max_tau as median track length
max_tau <- floor(median(df %>% group_by(ID) %>% summarise(n = n()) %>% pull(n)))
# MSD for varying tau
msd_df <- df %>%
  group_by(group, temp, ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > max_tau) %>%
  pull(ID) %>%
  map_df(~ {
    track <- df %>% filter(ID == .x) %>% arrange(Frame)
    tibble(
      group = first(track$group),
      temp = first(track$temp),
      tau = 1:max_tau,
      MSD = sapply(1:max_tau, function(tau) {
        disp <- track %>%
          mutate(
            X_shift = lead(Xpos, tau),
            Y_shift = lead(Ypos, tau)
          ) %>%
          filter(!is.na(X_shift)) %>%
          mutate(squared_displacement = (X_shift - Xpos)^2 + (Y_shift - Ypos)^2) %>%
          summarise(mean_squared_displacement = mean(squared_displacement, na.rm = TRUE)) %>%
          pull(mean_squared_displacement)
        return(disp)
      })
    )
  })
#filter outliers since taking the median 
msd_df <- msd_df %>%
  group_by(group, temp, tau) %>%
  mutate(
    MSD_mean = mean(MSD, na.rm = TRUE),
    MSD_sd = sd(MSD, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(MSD >= MSD_mean - 1.645 * MSD_sd & MSD <= MSD_mean + 1.645 * MSD_sd)

#visualize outliers
p<-ggplot(msd_df, aes(x = MSD)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.6) +
  labs(title = "Distribution of MSD Values", x = "MSD", y = "Count") +
  theme_minimal()
p
# tau to time (tau * time interval)
t_int <- 5 
# Compute the mean MSD for each temperature
msd_overall <- msd_df %>%
  group_by(tau) %>%
  summarise(
    MSD_mean = mean(MSD, na.rm = TRUE),
    MSD_se = sd(MSD, na.rm = TRUE) / sqrt(n()),  # Standard Error
    .groups = "drop"
  ) %>%
  mutate(time = tau * t_int)

p<-ggplot(msd_overall, aes(x = time, y = MSD_mean, color=temp)) +
  geom_line(color = "blue", size = 1) +      # Line plot
  labs(
    title = "Mean Squared Displacement Over Time",
    x = "Time (seconds)",   # Change to correct time unit
    y = "Mean Squared Displacement (um^2)",
    caption = "MSD calculated from particle tracking data"
  ) +
  theme_minimal() +                          # Clean theme
  theme(text = element_text(size = 10))      # Improve text readability
p
msd_overall$temp<-"66"
msd_overall$group<-"66C_360_615_FOV_3"
write_csv(msd_overall, "66C_120_360_FOV_2.csv")

#combine all individual files
path <- getwd()
csvcomb <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
datacomb <- bind_rows(lapply(csvcomb, read_csv))
# Save the combined data to a new CSV file
write_csv(datacomb, "57_62_64_70_comb_output.csv")