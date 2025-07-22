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
ggsave("images/rmst_causal_plot.png", plot = rmst_causal_plot, width = 10, height = 7, dpi = 150)