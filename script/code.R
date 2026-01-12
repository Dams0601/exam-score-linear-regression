library("broom")
library("car")
library("caret")
library("collapse")
library("correlation")
library("datawizard")
library("effectsize")
library("GGally")
library("ggfortify")
library("ggpubr")
library("ggrepel")
library("glue")
library("gtsummary")
library("insight")
library("janitor")
library("kableExtra")
library("lmtest")
library("marginaleffects")
library("matrixTests")
library("modelbased")
library("multcomp")
library("openxlsx")
library("parameters")
library("patchwork")
library("performance")
library("pROC")
library("qqplotr")
library("rstatix")
library("scales")
library("see")
library("tidyverse")

source("helper_functions.R")

###################################
### Section 1 - Data management ###
###################################
set.seed(42)



### Part 1 - Import data ###
data = read_csv("../data/project.csv", show_col_types = F)
head(data, n=10)

### Part 2 -  Convert coded categorical variables into properly labelled factors ###
# 1. Convert into factors
data_clean <- data |>
  mutate(
    sexe = factor(sexe, levels = c(1, 2, 3), labels = c("Female", "Male", "Other")),
    school_type = factor(school_type, levels = c(1, 2), labels = c("Public", "Private")),
    parent_educ = factor(parent_educ, levels = 1:6, 
                         labels = c("No formal", "High school", "Graduate", "Postgraduate 1", "Postgraduate 2", "PhD"), 
                         ordered = TRUE),
    sleep_qual = factor(sleep_qual, levels = c(1, 2, 3), labels = c("Poor", "Average", "Good"), ordered = TRUE),
    web_access = factor(web_access, levels = c(1, 2), labels = c("No", "Yes")),
    trav_time = factor(trav_time, levels = 1:4, 
                       labels = c("<15 min", "15–30 min", "30–60 min", ">60 min"), ordered = TRUE),
    extra_act = factor(extra_act, levels = c(1, 2), labels = c("No", "Yes")),
    study_method = factor(study_method, levels = 1:6, 
                          labels = c("Online videos", "Coaching", "Notes", "Textbook", "Group study", "Mixed"))
  ) |>
  # 2. Fix references (relevel)
  mutate(
    sexe = relevel(sexe, ref = "Female"),
    school_type = relevel(school_type, ref = "Public"),
    web_access = relevel(web_access, ref = "No"),
    extra_act = relevel(extra_act, ref = "No"),
    study_method = relevel(study_method, ref = "Online videos")
  ) |>
  # 3. Relabel
  relabel(
    sexe = "Gender",
    school_type = "School type",
    parent_educ = "Parental education",
    sleep_qual = "Sleep quality",
    web_access = "Internet access",
    trav_time = "Commute time",
    extra_act = "Extra curricular activities",
    study_method = "Study method",
    y = "Exam score",
    age = "Age (years)",
    attend_pct = "School attendance (%)"
  )



### Part 3 - Verification data integrity and plausibility ###
# Check duplicate ID
data_clean |>
  count(`id`) |>
  filter(n > 1)

# Confirm plausible range
summary(select(data_clean,
               `y`,
               `age`,
               `study_hrs`,
               `sleep_hrs`,
               `attend_pct`))


# Identify extreme values
quant_vars <- data_clean |>
  select(y, age, study_hrs, sleep_hrs, attend_pct)
outlier_summary <- quant_vars |>
  summarise(across(everything(), ~ {
    q1 <- quantile(.x, 0.25, na.rm = TRUE)
    q3 <- quantile(.x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    sum(.x < (q1 - 1.5 * iqr) | .x > (q3 + 1.5 * iqr), na.rm = TRUE)
  }))

outlier_summary



# Visualization of data
numeric_vars <- c("y", "age", "study_hrs", "sleep_hrs", "attend_pct")
vlabels <- c("Exam score", "Age (years)", "Weekly study (hours)", 
             "Sleep duration (hours)", "School attendance (%)")

data_clean |>
  select(id, all_of(numeric_vars)) |>
  pivot_longer(cols = -id, names_to = "var", values_to = "value") |>
  mutate(var = factor(var, levels = numeric_vars, labels = vlabels)) |>
  ggplot(aes(x = value)) +
  geom_histogram(fill = "dodgerblue", color = "black", bins = 20, linewidth = 0.5) +
  facet_wrap(~var, scales = "free") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(x = "Value", y = "Count") +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(size = 11, face = "bold"))

### Part 4 - Redundant representations ###
ggplot(data_clean, aes(x = agecat, y = age)) +
  geom_boxplot(fill = "grey80") +
  labs(x = "Age category", y = "Age (years)")

ggplot(data_clean, aes(x = attend_pct_cat, y = attend_pct)) +
  geom_boxplot(fill = "grey80") +
  labs(x = "Attendance category", y = "Attendance (%)")

#######################
### Section 2 - EDA ###
#######################

### Part 1 - Descriptive statistics for quant. variable ###
select(data, y, age, study_hrs, sleep_hrs, attend_pct) |>
  describe_distribution(centrality = c("mean", "median"), quartiles = TRUE) |>
  as_tibble()

### Part 2 - Frequency tables for categorical variables ###
tab_freq1(data_clean, c("sexe", "school_type", "parent_educ", "sleep_qual", "web_access",
                 "trav_time", "extra_act", "study_method"), digits= 1) |>
  kable(align = "l", padding= 2) |>
  row_spec(c(1, 5, 8, 15, 19, 22, 27, 30), bold= TRUE)

### Part 3 - Visualisation y vs Predictors ###
# y vs variables quantitatives (Linéarité)
data_clean |>
  select(y, age, study_hrs, sleep_hrs, attend_pct) |>
  pivot_longer(-y) |>
  ggplot(aes(x = value, y = y)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red") + # Pour vérifier la linéarité [cite: 61]
  facet_wrap(~name, scales = "free_x") +
  theme_minimal()

# y vs variables qualitatives (Boxplots)
data_clean |>
  select(y, sexe, school_type, sleep_qual, study_method, extra_act) |>
  # Conversion en caractères pour permettre la combinaison dans pivot_longer
  mutate(across(-y, as.character)) |> 
  pivot_longer(
    cols = -y,
    names_to = "variable",
    values_to = "value"
  ) |>
  ggplot(aes(x = value, y = y, fill = variable)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution de l'examen score (y) par variable qualitative",
       x = "Catégorie",
       y = "Exam score")

# Exemple de comparaison [cite: 73, 75]
mod_simple <- lm(y ~ age + sexe, data = data_clean)
mod_complet <- lm(y ~ . -id -agecat -attend_pct_cat, data = data_clean)

# Comparaison
compare_performance(mod_simple, mod_complet, metrics = "common")