library(ggplot2)
library(dplyr)
library(magrittr)
# Combine data frames
df_combined <- rbind(transform(causalData_lessEdu, dataset = "causalData_lessEdu"), transform(causalData_moreEdu, dataset = "causalData_moreEdu"))
# Create ggplot object and add layers
ggplot(df_combined, aes(x = standardized_w, y = nutritionalStatus, color = dataset)) +
  geom_point(data = subset(df_combined, dataset == "causalData_lessEdu"), size = 3) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  geom_point(data = subset(df_combined, dataset == "causalData_lessEdu"), size = 3) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "standardized_w", y = "nutritionalStatus", title = "Two data frames on a single graph") +
  theme_minimal()

#library(ggplot2)

# Combine data frames
df_combined <- rbind(transform(causalData_lessEdu, dataset = "causalData_lessEdu"), transform(causalData_moreEdu, dataset = "causalData_moreEdu"))

# Add filter to remove data points where standardized_w is less than or equal to -0.25 for causalData_moreEdu
df_combined <- df_combined %>%
  mutate(filtered = ifelse(dataset == "causalData_moreEdu" & standardized_w <= -0.4, "no", "yes")) %>%
  filter(filtered == "yes")
df_combined <- df_combined %>%
  mutate(filtered = ifelse(dataset == "causalData_lessEdu" & standardized_w > -0.4, "no", "yes")) %>%
  filter(filtered == "yes")

# Create ggplot object and add layers
ggplot(df_combined, aes(x = standardized_w, y = nutritionalStatus, color = dataset)) +
  geom_point(size = 3) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "standardized_w", y = "nutritionalStatus", title = "Two data frames on a single graph") +
  theme_minimal()

