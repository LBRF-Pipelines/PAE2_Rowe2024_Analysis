# Mean and standard deviation of age
mean_age <- mean(demo_dat$age) # 21.36
sd_age <- sd(demo_dat$age) # 5.21

# Range of age
min(demo_dat$age) # 18
max(demo_dat$age) # 65

# Num of F and M in experiment
sum(demo_dat$sex == 'M') # 40
sum(demo_dat$sex == 'F') # 50

# Num of R and L in experiment
sum(demo_dat$handedness == "L") # 9
sum(demo_dat$handedness == "R") # 81

# Determining number of times prisms were removed per group
prism_removed <- demo_dat[, c("id",
                              "group",
                              "Prism_removal_y_n")]

prism_removed$Prism_removal_y_n <- tools::toTitleCase(prism_removed$Prism_removal_y_n)

long <- reshape2::melt(prism_removed, id.vars = c("group", "id"))
xtabs(~group+value,long)

# PP-CTRL: 8 removed prism lenses
# PP-MI: 6 removed prism lenses

# Summarizing age and KVIQ data for each group
df_summary <- demo_dat%>%
  group_by(group) %>%
  summarise(age_avg = mean(age),
            age_sd = sd(age),
            KVIQ_V_avg = mean(KVIQ_V),
            KVIQ_V_sd = sd(KVIQ_V),
            KVIQ_K_avg = mean(KVIQ_K),
            KVIQ_K_sd = sd(KVIQ_K))

# Checking the Normality of KVIQ-V and KVIQ-K
by(demo_dat$KVIQ_V, demo_dat$group, shapiro.test) 
by(demo_dat$KVIQ_K, demo_dat$group, shapiro.test)
# Data is not normal. Right skew. ANOVA is robust. 

# Transforming data
demo_dat$KVIQ_V_New <- log10(26 - demo_dat$KVIQ_V)
hist(demo_dat$KVIQ_V_New)
demo_dat$KVIQ_K_New <- log10(26 - demo_dat$KVIQ_K)
hist(demo_dat$KVIQ_K_New)
by(demo_dat$KVIQ_V_New, demo_dat$group, shapiro.test) # Data is non Normal, but better and ANOVA is robust
by(demo_dat$KVIQ_K_New, demo_dat$group, shapiro.test) # Data is non normal, but better and ANOVA is robust


# Checking homogeneity of variance
library(car)
leveneTest(demo_dat$KVIQ_V, demo_dat$group, center = median)
leveneTest(demo_dat$KVIQ_K, demo_dat$group, center = median)
# Assumption met, p > 0.05

# ANOVA
library(afex)

mod_oneway <- aov_ez("id", "KVIQ_V_New",
                     data = demo_dat,
                     between = c("group")
)
mod_oneway
# F (3,86) = 1.38, p > 0.05, n2 = 0.046; no sig diff between groups

mod_oneway_k <- aov_ez("id", "KVIQ_K_New",
                       data = demo_dat,
                       between = c("group")
)
mod_oneway_k
# F(3,86) = 0.78, p > 0.05, n2 = 0.027; no sig diff between groups


