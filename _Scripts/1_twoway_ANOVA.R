# Checking the assumptions of ANOVA: Normality

# Baseline
library(ggplot2)
Baseline <- subset(df_PAE2, block == "Baseline")
norm_plot_baseline <- ggplot(data = Baseline,
                             aes(x = visual_angle, fill = group)) +
  geom_histogram(bins = 30) +
  xlab('Visual Angle (degrees)') + 
  ylab('Frequency') +
  facet_grid(cols = vars(group)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
norm_plot_baseline
# Data looks normal for all groups

# Post Test
PostTest <- subset(df_PAE2, block == "PostTest")
norm_plot_posttest <- ggplot(data = PostTest,
                             aes(x = visual_angle, fill = group)) +
  geom_histogram(bins = 30) +
  xlab('Visual Angle (degrees)') + 
  ylab('Frequency') +
  facet_grid(cols = vars(group)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
norm_plot_posttest
# Data looks normal for all groups

# Two-Way ANOVA
ANOVA_model <- aov_4(visual_angle_avg ~ block * group + (block | id), #block is nested in participant
                     data = df_aftereffects_E2)

ANOVA_model
# Significant effect of group F(3,86) = 9.12, p < 0.05, n2 = 0.169
# Significant effect of block F(1,86) = 633.42, p < 0.05, n2 = 0.727
# Significant interaction effect of group and block F(3,86) = 13.53, n2 = 0.146 

# Normality of the residuals
qqPlot(ANOVA_model$lm$residuals, xlab = "Standard Normal Distribution Quantiles", ylab = "Residual Quantiles")
shapiro.test(ANOVA_model$lm$residuals)
# Residuals are normally distributed W = 0.99075, p > 0.05


# Check the assumption of homogeneity of variance
check_homogeneity(ANOVA_model)
# Assumption met. Levene's Test, p > 0.05

# Contrasts
post_hoc <- emmeans(ANOVA_model,  pairwise ~ block: group)
post_hoc
post_hoc$contrasts

# Plotting interaction plot with emmeans
library(ggplot2)
pd = position_dodge(.2)

PAE2_plot <- ggplot(as_tibble(post_hoc$emmeans), aes(x = block, y = emmean, color = group, group = group)) +
  geom_errorbar(aes(ymin = lower.CL,
                    ymax = upper.CL), 
                width = 0.2,
                size = 0.7, 
                position = pd) + 
  geom_line(position = pd) +
  geom_point(shape = 19,
             size = 4,
             position = pd) +
  labs(x = "Time Point",
       y = "Visual Angle (degrees)") + 
  scale_x_discrete(labels = c('Baseline', 'Post Test')) +
  # scale_color_discrete(name = "Groups", labels = c("PP (250 trials)", "PP-None (20 trials)")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

PAE2_plot
