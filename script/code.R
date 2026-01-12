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
library("here")
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

source(here("script", "helper_functions.R"))

###################################
### Section 1 - Data management ###
###################################
set.seed(42)



### Part 1 - Import data ###
data = read.csv(here("data", "project.csv"))
head(data, n=10)
datawizard::describe_distribution(data) |> kable()

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
duplicates <- data_clean |> filter(duplicated(id))
cat("Nombre de doublons :", nrow(duplicates))

# Confirm plausible range
summary(select(data_clean,
               `y`,
               `age`,
               `study_hrs`,
               `sleep_hrs`,
               `attend_pct`))

# Verification of NA
na_count <- colSums(is.na(data_clean))
cat("Les NA sont :", na_count[na_count > 0]) # Display only if some NA exist

# Verification of values impossibles (ex: score < 0 or > 100)
invalid_data <- data_clean |> 
  filter(y < 0 | y > 100 | sleep_hrs > 24 | study_hrs > 168)
nrow(invalid_data)


# Identify extreme values + visualization of outliers
numeric_vars <- c("y", "age", "study_hrs", "sleep_hrs", "attend_pct")
vlabels <- c("Exam score", "Age (years)", "Weekly study (hours)", 
             "Sleep duration (hours)", "School attendance (%)")

data_clean |>
  select(all_of(numeric_vars)) |>
  set_names(vlabels) |> 
  pivot_longer(everything(), names_to = "Variable", values_to = "Valeur") |>
  ggplot(aes(x = Variable, y = Valeur, fill = Variable)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Analyse des valeurs extrêmes",
       subtitle = "Les points rouges indiquent des outliers potentiels (méthode IQR)",
       x = "", 
       y = "Valeurs") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 10))

# Filter to inspect extreme exam score values (y)
# We use the IQR method to flag potential outliers for manual review
outliers_y <- data_clean |> 
  filter(y < (quantile(y, 0.25) - 1.5 * IQR(y)) | 
           y > (quantile(y, 0.75) + 1.5 * IQR(y)))
print(outliers_y)


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

# Mathematical proof of redundancy using Spearman correlation (rank-based)
age_redundancy <- cor(data_clean$age, as.numeric(data_clean$agecat), method = "spearman")
attend_redundancy <- cor(data_clean$attend_pct, as.numeric(data_clean$attend_pct_cat), method = "spearman")

cat("Correlation Age vs AgeCat:", age_redundancy, "\n")
cat("Correlation Attend vs AttendCat:", attend_redundancy, "\n")

# Rationale: Since correlations are near 1, including both would cause perfect multicollinearity.
# We keep the continuous versions to preserve granular information.

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
# a. Distribution of y
p_hist <- ggplot(data_clean, aes(x = y)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "dodgerblue", color = "white") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Histogram and Density of Exam Score", x = "Score (y)", y = "Density") +
  theme_minimal()

p_box <- ggplot(data_clean, aes(y = y)) +
  geom_boxplot(fill = "dodgerblue", outlier.color = "red") +
  labs(title = "Boxplot of Exam Score", y = "Score (y)") +
  theme_minimal()

p_hist + p_box

skewness(data_clean$y) # Skewness close to 0 => symmetry:  OK

# b. y vs variables quantitatives (Linearity)
data_clean |>
  select(y, age, study_hrs, sleep_hrs, attend_pct) |>
  pivot_longer(-y) |>
  ggplot(aes(x = value, y = y)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red") + # Pour vérifier la linéarité [cite: 61]
  facet_wrap(~name, scales = "free_x") +
  theme_minimal()

# b. y vs qualitative variables (Boxplots)
data_clean |>
  select(y, sexe, school_type, sleep_qual, study_method, extra_act) |>
  # On convertit tout sauf 'y' en character pour éviter le conflit de types
  mutate(across(-y, as.character)) |> 
  pivot_longer(
    cols = -y,
    names_to = "variable",
    values_to = "value"
  ) |>
  ggplot(aes(x = value, y = y, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  facet_wrap(~ variable, scales = "free_x") + # free_x est crucial ici
  theme_bw() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    title = "Distribution of Exam Score (y) by Categorical Predictors",
    x = "Category Level",
    y = "Exam Score"
  )

# c. Numeric-Numeric: Correlation Matrix
data_clean |>
  select(all_of(numeric_vars)) |>
  correlation(method = "pearson") |>
  summary(redundant = TRUE) |>
  plot() +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  labs(title = "Correlation Heatmap: Quantitative Predictors")

# c. Categorical-Categorical: Contingency & Association


# c. Mixed


### Part 4 - Assessment of monotone trends for ordinal predictors ###
# Example for Sleep Quality
p1 <- plot_monotone(data_clean, "sleep_qual", "Sleep Quality")

# Example for Parental Education
p2 <- plot_monotone(data_clean, "parent_educ", "Parental Education")
library(patchwork)
p1 / p2

# Part 5 - Conclude EDA #
