# --- R Code to Generate the 2-Group RMST Plot ---
library(ggplot2)

# Create sample data for two groups (Treatment and Control)
time <- 0:100
control_surv <- exp(-0.04 * time)
treatment_surv <- exp(-0.02 * time)
df <- data.frame(
  time = rep(time, 2),
  surv = c(control_surv, treatment_surv),
  group = rep(c("Control", "Treatment"), each = 101)
)

# Create the plot
rmst_causal_plot <- ggplot(df, aes(x = time, y = surv, fill = group, color = group)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(data = subset(df, time <= 70), aes(ymin = 0, ymax = surv), alpha = 0.3) +
  geom_vline(xintercept = 70, linetype = "dashed", color = "black", linewidth = 1.1) +
  annotate("text", x = 85, y = 0.8, label = "Restriction Time", size = 7, color = "black", fontface = "bold") +
  scale_fill_manual(values = c("Control" = "tomato", "Treatment" = "dodgerblue")) +
  scale_color_manual(values = c("Control" = "tomato", "Treatment" = "dodgerblue")) +
  labs(
    title = "Average Survival Between Groups",
    subtitle = "The difference in the shaded areas is the treatment benefit",
    x = "Time",
    y = "Survival Probability"
  ) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "top") +
  coord_cartesian(ylim = c(0, 1))

# Save the plot to a file
ggsave("poster/images/rmst_causal_plot.png", plot = rmst_causal_plot, width = 10, height = 7, dpi = 150)

# Load the ggplot2 library
library(ggplot2)

# 1. Define parameters and generate data
tau <- 75
time_points <- seq(0, 100, by = 0.5)
survival_data <- data.frame(
  time = time_points,
  treatment_prob = exp(-0.018 * time_points), # Slower decay
  control_prob = exp(-0.045 * time_points)   # Faster decay
)

# Filter data for shading up to the restriction time
ribbon_data <- subset(survival_data, time <= tau)

# 2. Create the plot with your specified customizations
p <- ggplot(data = survival_data, aes(x = time)) +
  
  # Add shaded ribbons for RMST area
  # The fill aesthetic maps the names to the colors defined in scale_fill_manual
  geom_ribbon(data = ribbon_data, aes(ymin = 0, ymax = control_prob, fill = "Control"), alpha = 0.4) +
  geom_ribbon(data = ribbon_data, aes(ymin = control_prob, ymax = treatment_prob, fill = "Treatment"), alpha = 0.4) +
  
  # Add the survival curves
  # The color aesthetic maps the names to the colors defined in scale_color_manual
  geom_line(aes(y = treatment_prob, color = "Treatment"), linewidth = 1.1) +
  geom_line(aes(y = control_prob, color = "Control"), linewidth = 1.1) +
  
  # Add and label the restriction time line
  geom_segment(
    aes(x = tau, y = 0, xend = tau, yend = 1),
    linetype = "solid",
    linewidth = 1.1,
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = tau - 7.5, # Position label to the left of the line
    y = 0.5,     # Position label vertically in the middle
    label = paste("Restriction \n Time "),
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  
  # Manually set the colors as requested
  scale_color_manual(
    name = "Group",
    values = c("Treatment" = "red", "Control" = "blue")
  ) +
  scale_fill_manual(
    name = "RMST Area",
    values = c("Treatment" = "red", "Control" = "blue")
  ) +
  
  # Add plot titles and axis labels
  labs(
    x = "Time",
    y = "Survival Probability"
  ) +
  
  # Use a white background theme and move the legend to the top
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16)
  )

# 3. Display the plot
print(p)