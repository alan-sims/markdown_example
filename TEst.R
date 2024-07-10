library(tidyverse)
library(ggridges)
library(rmarkdown)
library(saccades) ##this is good if you need to segment
library(png) ##this reads in the png image
library(wesanderson) ##this is just awesome!
library(graphics) ##cant remember what this is for...
library(matlab) ##this might be for the traditional heatmap colours?
library(grid)
library(Rmisc)
library(patchwork)
library(gapminder)
library(broom)
library(readxl)
library(cowplot)
library(viridis)
library(knitr)
library(psych) #used to calculate harmonic mean
library(Hmisc) #used for table summaries



# Function to import dataframes and perform renaming
import_and_rename <- function(folder_path, file_names, pattern, replacement) {
  # Create an empty list to store the dataframes
  drive_list <- list()
  
  # Loop through the file names
  for (file_name in file_names) {
    file_path <- file.path(folder_path, file_name)
    dataframe <- read.csv(file_path, sep = "\t")
    drive_list[[file_name]] <- dataframe
  }
  
  # Get the current names of the dataframes in the list
  current_names <- names(drive_list)
  
  # Create a function to rename the dataframes
  rename_dataframe <- function(name) {
    new_name <- gsub(pattern, replacement, name)
    return(new_name)
  }
  
  # Apply the renaming function to the current names
  new_names <- sapply(current_names, rename_dataframe)
  
  # Update the names of the dataframes in the list
  names(drive_list) <- new_names
  
  # Renaming the columns in each dataframe of the list
  drive_list <- lapply(drive_list, function(df) {
    colnames(df) <- c('time', 'x', 'y', 'posx', 'posy', 'screenID')  # Rename the columns
    return(df)
  })
  
  return(drive_list)
}

# Define harmonic mean function
harmonic_mean <- function(x) {
  n <- length(x)
  return(n / sum(1 / x))
}



# Set the working directory
setwd("/Volumes/AlanPhD/Alan_PhD_Data/P99")

# Import and rename drive 1 dataframes
'cond_0.00' <- import_and_rename("0.00/Eye-Tracking/", c("Section 1_ET.csv", "Section 2_ET.csv", "Section 3_ET.csv"), "Section ", "0.00_section")

# Import and rename drive 2 dataframes
'cond_0.05' <- import_and_rename("0.05/Eye-Tracking/", c("Section 1_ET.csv", "Section 2_ET.csv", "Section 3_ET.csv"), "Section ", "0.05_section")

cond_0.00 <- Map(function(df, section) {
  df$section <- section
  return(df)
}, cond_0.00, seq_along(cond_0.00))

cond_0.05 <- Map(function(df, section) {
  df$section <- section
  return(df)
}, cond_0.05, seq_along(cond_0.05))



# Combine the dataframes using rbind
combined_0.00 <- do.call(rbind, cond_0.00)
combined_0.05 <- do.call(rbind, cond_0.05)


#changing the screen ID to left, cen, right, none
for (i in 1:length(combined_0.00)) {
  # Apply the code to each dataframe
  combined_0.00$screenID <- ifelse(combined_0.00$screenID == 4, "cen",
                                   ifelse(combined_0.00$screenID == 8, "left",
                                          ifelse(combined_0.00$screenID == 9, "right",
                                                 ifelse(combined_0.00$screenID == -1, "none", combined_0.00$screenID))))
}

for (i in 1:length(combined_0.05)) {
  # Apply the code to each dataframe
  combined_0.05$screenID <- ifelse(combined_0.05$screenID == 4, "cen",
                                   ifelse(combined_0.05$screenID == 8, "left",
                                          ifelse(combined_0.05$screenID == 9, "right",
                                                 ifelse(combined_0.05$screenID == -1, "none", combined_0.05$screenID))))
}


#adjust the x values for the center and right screens
for (i in 1:length(combined_0.00)) {
  # Apply the code to each dataframe
  combined_0.00$posx <- ifelse(combined_0.00$screenID == "cen", combined_0.00$posx + 3840,
                               ifelse(combined_0.00$screenID == "right", combined_0.00$posx + 7680, combined_0.00$posx))
}
for (i in 1:length(combined_0.05)) {
  # Apply the code to each dataframe
  combined_0.05$posx <- ifelse(combined_0.05$screenID == "cen", combined_0.05$posx + 3840,
                               ifelse(combined_0.05$screenID == "right", combined_0.05$posx + 7680, combined_0.05$posx))
}


combined_0.00$trial = 'alc_0.00'
combined_0.05$trial = 'alc_0.05'

#duplicated_rows <- combined_0.05 %>% group_by(trial, time) %>% filter(n() > 1)
#print(duplicated_rows)

combined_0.00 <- combined_0.00 %>% distinct(time, .keep_all = TRUE)
combined_0.05 <- combined_0.05 %>% distinct(time, .keep_all = TRUE)

Scaner_data <- rbind(combined_0.00, combined_0.05)

Scaner_data$trial <- as.factor(Scaner_data$trial)
Scaner_data <- Scaner_data %>%
  arrange(trial, time)

Scaner_fixs <- detect.fixations(Scaner_data)
df$trial <- as.factor(df$trial)

# Combine trial and event into a single grouping variable
Scaner_fixs$trial_event <- interaction(Scaner_fixs$trial, Scaner_fixs$event)

# Use describeBy to get the summary statistics
summ <- psych::describeBy(Scaner_fixs, group = Scaner_fixs$trial_event, mat = TRUE)
psych::describeBy(Scaner_fixs, group = Scaner_fixs$trial)

Scaner_SD <- Scaner_fixs %>%
  dplyr::group_by(trial) %>%
  dplyr::summarize(
    sd_x = sd(x, na.rm = TRUE),
    sd_y = sd(y, na.rm = TRUE)
  )
print(Scaner_SD)


Scaner_SD_filtered <- Scaner_fixs %>%
  dplyr::group_by(trial) %>%
  dplyr::summarize(
    sd_x = sd(x[x != 0], na.rm = TRUE),
    sd_y = sd(y[y != 0], na.rm = TRUE)
  )
print(Scaner_SD_filtered)

Scaner_fix_dur <- subset(Scaner_fixs, Scaner_fixs$event == 'fixation')

Scaner_fix_Hmean <- Scaner_fix_dur %>%
  dplyr::group_by(trial) %>%
  dplyr::summarize(
    hmean_dur = harmonic_mean(dur)
  )
print(Scaner_fix_Hmean)

Scaner_blink_dur <- subset(Scaner_fixs, Scaner_fixs$event == 'blink')
Scaner_blink_Hmean <- Scaner_blink_dur %>%
  dplyr::group_by(trial) %>%
  dplyr::summarize(
    hmean_dur = harmonic_mean(dur)
  )
print(Scaner_blink_Hmean)



std_dev_1 <- sd(df[df$trial == "1", "x"], na.rm = TRUE)
std_dev_2 <- sd(df[df$trial == "2", "x"], na.rm = TRUE)

std_dev_1 <- sd(df[df$trial == "1", "y"], na.rm = TRUE)
std_dev_2 <- sd(df[df$trial == "2", "y"], na.rm = TRUE)



# Create a data frame
#sd_table <- data.frame(
#  Condition = c("0.00", "0.05", "0.08"),
#  Horizontal_SD = c(hor_sd_0.00, hor_sd_0.05, hor_sd_0.08),
#  Vertical_SD = c(ver_sd_0.00, ver_sd_0.05, ver_sd_0.08)
#)

# Print the table in RMarkdown using knitr::kable
#knitr::kable(sd_table, caption = "Standard Deviations of Positions")

SE_0.00 <- read.csv("0.00/Eye-Tracking/P99.log", sep = "\t") # reading in the data from SmartEYE
SE_0.00$TimeStamp <- SE_0.00$TimeStamp - SE_0.00$TimeStamp[1] #startingtimestamp from zero
SE_0.00 <- subset(SE_0.00, TimeStamp <= 17600000000) #cutting dataset off at 17600
SE_0.00$Trial = 'alc_0.00' #adding the trial name

SE_0.05 <- read.csv("0.05/Eye-Tracking/P99.log", sep = "\t")
SE_0.05$TimeStamp <- SE_0.05$TimeStamp - SE_0.05$TimeStamp[1] 
SE_0.05 <- subset(SE_0.05, TimeStamp <= 17600000000)
SE_0.05$Trial = 'alc_0.05'


# Step 2: Combine datasets
combined <- rbind(SE_0.00, SE_0.05)

# Step 3: Select and rename relevant columns
combined_ET <- combined %>% select(TimeStamp, Trial, GazeDirection.x, GazeDirection.y)
filtered_ET <- combined_ET %>% filter(GazeDirection.x != 0)

# Rename columns using dplyr::rename
filtered_ET <- filtered_ET %>% dplyr::rename(
  time = TimeStamp,
  trial = Trial,
  x = GazeDirection.x,
  y = GazeDirection.y
)

combined_ET <- combined_ET %>% dplyr::rename(
  time = TimeStamp,
  trial = Trial,
  x = GazeDirection.x,
  y = GazeDirection.y
)



# Step 4: Ensure the 'trial' column is a factor
filtered_ET$trial <- as.factor(filtered_ET$trial)

#same again but with the filtered data.
filtered_fixes <- detect.fixations(filtered_ET, lambda = 6, smooth.coordinates = T,
                                   smooth.saccades = T)

combined_fixes <- detect.fixations(combined_ET, lambda = 6, smooth.coordinates = T,
                                   smooth.saccades = T)

filtered_fixes$trial <- as.factor(filtered_fixes$trial) 

# Step 5: Calculate standard deviations
std_devs <- filtered_fixes %>%
  dplyr::group_by(trial) %>%
  dplyr::summarize(
    sd_x = sd(x, na.rm = TRUE),
    sd_y = sd(y, na.rm = TRUE)
  )

std_devs <- combined_ET %>%
  dplyr::group_by(trial) %>%
  dplyr::summarize(
    sd_x = sd(x, na.rm = TRUE),
    sd_y = sd(y, na.rm = TRUE)
  )

# Display the table of sd
kable(std_devs, caption = "Standard Deviations of x and y by Trial")

harmonic_mean <- function(x) {
  n <- length(x)
  return(n / sum(1 / x))
}

grouped_data <- filtered_fixes %>% group_by(trial)

filtered_means <- grouped_data %>% 
  summarize(
    h_mean = harmonic_mean(dur)
  )

print(filtered_means)



filtered_means <- filtered_fixes %>% 
  group_by(trial) %>% 
  summarize(
    h_mean = harmonic_mean(dur)
  )


harmonic_mean(filtered_fixes$dur)

test <- data.frame(
  trial = c(1, 1, 1, 2, 2, 2),
  dur = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
)








# Summarize by group
filtered_means <- grouped_data %>%
  dplyr::summarize(h_mean = harmonic_mean(dur))

print(filtered_means)





filtered_fixes <- filtered_fixes %>% filter(dur != 0)
summarise(filtered_fixes$dur)


d1 <- readPNG("visual2.png",native = T) ##read in image
d1 <- rasterGrob(d1,
                 width=unit(1,"npc"), ##make sure to include these so it scales
                 height=unit(1,"npc"), ##make sure to include these so it scales
                 interpolate=F)
m1<-jet.colors(n=20) ##just using a traditional heatmap
m1<-wes_palette(name='Zissou1',n=20,type='continuous') ##you can use wesanderson ones

minAlpha = 0 ##change this to change transparency of overlay
maxAlpha = 1 ##change this to change transparency of overlay

fixShape = 3 ##set to + symbols

plot_0.00 <- ggplot(SE_0.00,aes(GazeDirection.x, GazeDirection.y))+
  annotation_custom(d1, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_ellipse(colour='white',linewidth=2) +
  stat_ellipse(colour='black',linewidth=1) +
  #Inf will scale to fill the page
  ###uncomment these if you want points overlaid
  geom_point(shape=3,col='black',alpha=0)+
  geom_point(shape=3,col='white',alpha=0.7)+
  stat_density2d(geom = "raster",aes(fill = ..density..,alpha=..density..),contour=F,adjust = 10)+
  scale_x_continuous(limits = c(-0.5,0.5),expand=c(0,0))+ ##these are set by the coordinate system of the tracker
  scale_y_continuous(limits = c(-0.5,0.5),expand=c(0,0))+ ##these are set by the coordinate system of the tracker
  scale_fill_gradientn(colors = m1)+ ##change this for different color scales
  scale_alpha_continuous(range=c(minAlpha,maxAlpha))+ ##set limits for transparency of heatmap
  theme_linedraw()+ ##linedraw is a nice plotting theme
  guides(size='none',fill='none',alpha='none') + ##remove
  coord_fixed(0.1875) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  ggtitle("0.00")

plot_0.05 <- ggplot(SE_0.05,aes(GazeDirection.x, GazeDirection.y))+
  annotation_custom(d1, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_ellipse(colour='white',linewidth=2) +
  stat_ellipse(colour='black',linewidth=1) +
  #Inf will scale to fill the page
  ###uncomment these if you want points overlaid
  geom_point(shape=3,col='black',alpha=0)+
  geom_point(shape=3,col='white',alpha=0.7)+
  stat_density2d(geom = "raster",aes(fill = ..density..,alpha=..density..),contour=F,adjust = 10)+
  scale_x_continuous(limits = c(-0.5,0.5),expand=c(0,0))+ ##these are set by the coordinate system of the tracker
  scale_y_continuous(limits = c(-0.5,0.5),expand=c(0,0))+ ##these are set by the coordinate system of the tracker
  scale_fill_gradientn(colors = m1)+ ##change this for different color scales
  scale_alpha_continuous(range=c(minAlpha,maxAlpha))+ ##set limits for transparency of heatmap
  theme_linedraw()+ ##linedraw is a nice plotting theme
  guides(size='none',fill='none',alpha='none') + ##remove
  coord_fixed(0.1875) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  ggtitle("0.05")

plot_0.08 <- ggplot(SE_0.08,aes(GazeDirection.x, GazeDirection.y))+
  annotation_custom(d1, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_ellipse(colour='white',linewidth=2) +
  stat_ellipse(colour='black',linewidth=1) +
  #Inf will scale to fill the page
  ###uncomment these if you want points overlaid
  geom_point(shape=3,col='black',alpha=0)+
  geom_point(shape=3,col='white',alpha=0.7)+
  stat_density2d(geom = "raster",aes(fill = ..density..,alpha=..density..),contour=F,adjust = 10)+
  scale_x_continuous(limits = c(-0.5,0.5),expand=c(0,0))+ ##these are set by the coordinate system of the tracker
  scale_y_continuous(limits = c(-0.5,0.5),expand=c(0,0))+ ##these are set by the coordinate system of the tracker
  scale_fill_gradientn(colors = m1)+ ##change this for different color scales
  scale_alpha_continuous(range=c(minAlpha,maxAlpha))+ ##set limits for transparency of heatmap
  theme_linedraw()+ ##linedraw is a nice plotting theme
  guides(size='none',fill='none',alpha='none') + ##remove
  coord_fixed(0.1875) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  ggtitle("0.08")

combined_plot <- plot_grid(plot_0.00, plot_0.05, plot_0.08, ncol = 1)

# Display the combined plot
print(combined_plot)




ggplot(filtered_ET, aes(x = x, y = trial)) +
  geom_density_ridges(aes(fill = trial)) +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(x = "Eye tracking devation", y = "Horizontal Spread of Gaze") +
  guides(fill = guide_legend(reverse = TRUE)) +
  xlim(-0.5, 0.5)


#comparing horizontal spread between conditions
ggplot(filtered_ET, aes(x = y, y = trial)) +
  geom_density_ridges(aes(fill = trial)) +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(x = "Vertical Spread of Gaze", y = "Alcohol Dose Condition") +
  guides(fill = guide_legend(reverse = TRUE)) +
  xlim(-0.5, 0.5) + coord_flip()

