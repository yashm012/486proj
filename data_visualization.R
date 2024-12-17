library(dplyr)
library(ggplot2)

# Load datasets
alc_consumption <- read.csv('~/Downloads/archive/student-mat.csv', header = TRUE)
d2 <- read.csv('~/Downloads/archive/student-por.csv', header = TRUE)
# Merge datasets
d3 <- merge(alc_consumption, d2, by = c("Dalc", "failures", "goout", "absences", "studytime", "famrel", "Pstatus"), suffixes = c("", ".y"))

# Remove duplicated columns with `.y` suffix
d3 <- d3[, !grepl("\\.y$", colnames(d3))]

# Remove rows with missing G3
d3 <- d3[!is.na(d3$G3), ]
colnames(d3)

str(d3)




# Histogram of G3
ggplot(d3, aes(x = G3)) +
  geom_histogram(binwidth = 0.9, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Final Grades", x = "Final Grades (G3)", y = "Frequency")+
  theme_minimal() + 
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )


# Density Plot
# Density Plot
ggplot(d3, aes(x = G3)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  labs(title = "Density of Final Grades", x = "Final Grades (G3)", y = "Density")+
  theme_minimal() + 
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )



#Overlapping plot

# Add a column to distinguish Math and Portuguese datasets
alc_consumption$Subject <- "Math"
d2$Subject <- "Portuguese"

# Combine datasets with the subject label
combined_data <- rbind(alc_consumption, d2)

# Create the density plot with overlapping slopes
ggplot(combined_data, aes(x = G3, fill = Subject, color = Subject)) +
  geom_density(alpha = 0.6, size = 0.5) +
  scale_fill_manual(values = c("Math" ="lightblue","Portuguese" = "pink")) +
  scale_color_manual(values = c("Math" ="black","Portuguese" = "black")) +
  labs(
    title = "Density of Final Grades (Math vs Portuguese)",
    x = "Final Grades (G3)",
    y = "Density",
    fill = "Subject",
    color = "Subject"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )


#ALC
  
  # Jitter Plot: Workday Alcohol Consumption vs G3 with Regression Line
  ggplot(d3, aes(x = factor(Dalc), y = G3, color = factor(Dalc))) +
  geom_jitter(alpha = 0.6, size = 2) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
  labs(title = "Final Grades by Workday Alcohol Consumption (With Trend Line)", 
       x = "Workday Alcohol Consumption (1-5)", 
       y = "Final Grades (G3)", 
       color = "Workday Alcohol Level") +
  theme_minimal()+
theme(
  legend.position = "top",
  legend.title = element_text(face = "bold")
)


# Jitter Plot: Weekend Alcohol Consumption vs G3 with Regression Line
ggplot(d3, aes(x = factor(Walc), y = G3, color = factor(Walc))) +
  geom_jitter(alpha = 0.6, size = 2) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
  labs(title = "Final Grades by Weekend Alcohol Consumption (With Trend Line)", 
       x = "Weekend Alcohol Consumption (1-5)", 
       y = "Final Grades (G3)", 
       color = "Weekend Alcohol Level") +
  theme_minimal()+
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )





# Interaction Heatmap: Alcohol Consumption and Social Time on Grades
heatmap_data <- d3 %>%
  group_by(Dalc, goout) %>%
  summarize(mean_G3 = mean(G3, na.rm = TRUE))

ggplot(heatmap_data, aes(x = factor(Dalc), y = factor(goout), fill = mean_G3)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "blue", name = "Avg G3") +
  labs(title = "Impact of Alcohol Consumption and Social Time on Grades", 
       x = "Workday Alcohol Consumption (1-5)", 
       y = "Social Time (1-5)") +
  theme_minimal()





# Aggregate data for heatmap
heatmap_data <- d3 %>%
  group_by(Dalc, studytime) %>%
  summarize(mean_G3 = mean(G3, na.rm = TRUE))

# Create heatmap
ggplot(heatmap_data, aes(x = factor(Dalc), y = factor(studytime), fill = mean_G3)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "pink", high = "purple", name = "Avg G3") +
  labs(
    title = "Impact of Alcohol Consumption and Study Time on Final Grades",
    x = "Workday Alcohol Consumption (1-5)",
    y = "Study Time (1-4)"
  ) +
  theme_minimal()









# Study Time vs Final Grades: Violin and Boxplot
ggplot(d3, aes(x = factor(studytime), y = G3, fill = factor(studytime))) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.6, outlier.shape = NA) +
  labs(title = "Final Grades by Study Time", 
       x = "Study Time (1: <2h, 2: 2-5h, 3: 5-10h, 4: >10h)", 
       y = "Final Grades (G3)", 
       fill = "Study Time") +
  theme_minimal()


# Social Time vs Final Grades: Scatter Plot with Regression Line
ggplot(d3, aes(x = goout, y = G3)) +
  geom_jitter(alpha = 0.6, size = 2, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Final Grades by Social Time (Going Out)", 
       x = "Social Time (1-5)", 
       y = "Final Grades (G3)") +
  theme_minimal()

# Calculate correlation coefficient for G3 and goout
cor(d3$goout,d3$G3)





# Social Time vs Final Grades: Scatter Plot with Regression Line
ggplot(d3, aes(x = studytime, y = G3)) +
  geom_jitter(alpha = 0.6, size = 2, color = "purple") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Final Grades by Study Time (studytime)", 
       x = "Study Time (1-5)", 
       y = "Final Grades (G3)") +
  theme_minimal()

# Calculate correlation coefficient for G3 and goout
cor(d3$studytime,d3$G3)



