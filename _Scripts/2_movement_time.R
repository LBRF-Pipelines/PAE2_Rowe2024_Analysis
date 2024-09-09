# Plot movement time across 10 blocks of 25 trials for all groups
library(ggplot2)
pd = position_dodge(0.2)
movement_time <- ggplot(df_movement_time,
                        aes(x = subblock, y = response_time_avg, colour = group, group = group))+
  geom_smooth(method='lm', formula= y~x) +
  geom_point(aes(shape = group, color = group), #Adding in shapes 
             size = 4,
             position = pd) +
  labs(x = "10 Blocks of Exposure Trials",
       y = "Response time (ms)") + 
  scale_x_discrete(limits = factor(1:10))+ 
  #scale_shape_discrete(name = "Groups", labels = c('Control', 'Motor Imagery', "Physical Practice")) +
  #scale_color_discrete(name = "Groups", labels = c('Control', 'Motor Imagery', "Physical Practice")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = rel(1.5)), #adjusting font size
        axis.text.y = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.25)),
        axis.title = element_text(size = rel(1.25)))

movement_time


