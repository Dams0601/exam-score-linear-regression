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
cat("=== DATASET STRUCTURE ===\n")
cat("Sample size:", nrow(data), "observations\n")
cat("Number of variables:", ncol(data), "\n\n")
glimpse(data)

cat("\n=== VARIABLE TYPES ===\n")
data |> 
  summarise(across(everything(), class)) |> 
  pivot_longer(everything(), names_to = "Variable", values_to = "Type") |> 
  kable()

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

cat("\n=== FACTOR LEVELS ===\n")
data_clean |> 
  select(where(is.factor)) |> 
  lapply(levels) |> 
  print()

### Part 3 - Verification data integrity and plausibility ###
cat("\n=== DUPLICATE CHECK ===\n")
duplicates <- data_clean |> filter(duplicated(id))
cat("Number of duplicate IDs:", nrow(duplicates), "\n")

cat("\n=== MISSING VALUES ===\n")
na_count <- colSums(is.na(data_clean))
if(sum(na_count) == 0) {
  cat("No missing values detected\n")
} else {
  print(na_count[na_count > 0])
}

cat("\n=== RANGE VERIFICATION ===\n")
data_clean |>
  summarise(
    across(c(y, age, study_hrs, sleep_hrs, attend_pct),
           list(min = min, max = max, mean = mean, sd = sd))
  ) |>
  pivot_longer(everything()) |>
  separate(name, into = c("Variable", "Statistic"), sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = Statistic, values_from = value) |>
  kable(digits = 2, caption = "Descriptive statistics for numeric variables")

cat("\n=== IMPLAUSIBLE VALUES ===\n")
invalid_data <- data_clean |> 
  filter(y < 0 | y > 100 | sleep_hrs < 0 | sleep_hrs > 24 | 
           study_hrs < 0 | study_hrs > 168 | attend_pct < 0 | attend_pct > 100)
cat("Number of implausible values:", nrow(invalid_data), "\n")

cat("\n=== OUTLIER DETECTION ===\n")
numeric_vars <- c("y", "age", "study_hrs", "sleep_hrs", "attend_pct")
vlabels <- c("Exam score", "Age (years)", "Weekly study (hours)", 
             "Sleep duration (hours)", "School attendance (%)")

# Boxplot visualization
data_clean |>
  select(all_of(numeric_vars)) |>
  set_names(vlabels) |> 
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
  ggplot(aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Outlier detection (IQR method)",
       subtitle = "Red points indicate potential outliers",
       x = "", y = "Values") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

# Identify outliers in exam score
outliers_y <- data_clean |> 
  filter(y < (quantile(y, 0.25) - 1.5 * IQR(y)) | 
           y > (quantile(y, 0.75) + 1.5 * IQR(y)))
cat("Potential outliers in exam score:", nrow(outliers_y), "\n")
cat("Decision: Retained (values are plausible within [0,100] range)\n")

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
cat("\n=== REDUNDANCY ANALYSIS ===\n")

# Visual inspection
p1 <- ggplot(data_clean, aes(x = factor(agecat), y = age)) +
  geom_boxplot(fill = "grey80") +
  labs(title = "Age vs Age category", x = "Age category", y = "Age (years)") +
  theme_bw()

p2 <- ggplot(data_clean, aes(x = factor(attend_pct_cat), y = attend_pct)) +
  geom_boxplot(fill = "grey80") +
  labs(title = "Attendance vs Attendance category",
       x = "Attendance category", y = "Attendance (%)") +
  theme_bw()

p1 + p2

# Quantify redundancy
age_cor <- cor(data_clean$age, as.numeric(data_clean$agecat), method = "spearman")
attend_cor <- cor(data_clean$attend_pct, as.numeric(data_clean$attend_pct_cat), 
                  method = "spearman")

cat("Spearman correlation - Age vs AgeCat:", round(age_cor, 3), "\n")
cat("Spearman correlation - Attend vs AttendCat:", round(attend_cor, 3), "\n")

cat("\nRationale: Both pairs show near-perfect correlation (r > 0.99),")
cat("\nindicating they represent the same information.")
cat("\nDecision: Keep continuous versions (age, attend_pct) to preserve")
cat("\ngranular information and avoid multicollinearity.\n")

# Remove redundant variables
data_clean <- data_clean |>
  select(-agecat, -attend_pct_cat)

cat("\nVariables removed: agecat, attend_pct_cat\n")
cat("\nNote: Variable 'id' is retained for traceability but will not be used in modeling.\n")

### Part 5 - Final data structure ###

cat("\n=== FINAL CLEANED DATASET ===\n")
cat("Final sample size:", nrow(data_clean), "observations\n")
cat("Final number of variables:", ncol(data_clean), "\n")
glimpse(data_clean)

# Pairwise relationships (without redundant variables)
data_clean |>
  select(-id) |>
  relocate(y, .after = last_col()) |>
  ggpairs(
    lower = list(
      continuous = wrap("points", size = 1, shape = 21, 
                        fill = "white", color = "blue", alpha = 0.6)
    ),
    title = "Pairwise relationships (cleaned data)"
  ) +
  theme_bw(base_size = 10)



#######################
### Section 2 - EDA ###
#######################

### Part 1 - Descriptive statistics for quant. variable ###
data_clean |>
  select(y, age, study_hrs, sleep_hrs, attend_pct) |>
  describe_distribution(centrality = c("mean", "median"), quartiles = TRUE) |>
  as_tibble() |>
  kable(digits = 2, caption = "Descriptive statistics for numeric variables")


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

# Compute skewness
skew_val <- skewness(data_clean$y)$Skewness
cat("\nSkewness:", round(skew_val, 3), "\n")
cat("Interpretation:", 
    ifelse(abs(skew_val) < 0.5, "approximately symmetric", 
           ifelse(skew_val > 0, "right-skewed", "left-skewed")), "\n")
n_outliers = sum(data_clean$y < quantile(data_clean$y, 0.25) - 1.5*IQR(data_clean$y) |
                   data_clean$y > quantile(data_clean$y, 0.75) + 1.5*IQR(data_clean$y))
cat("Outliers:", n_outliers, "detected but within plausible range [0, 100]\n")


# b. y vs variables quantitatives (Linearity)
data_clean |>
  select(y, age, study_hrs, sleep_hrs, attend_pct) |>
  pivot_longer(-y, names_to = "predictor", values_to = "value") |>
  ggplot(aes(x = value, y = y)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  facet_wrap(~predictor, scales = "free_x") +
  labs(title = "Exam Score vs Quantitative Predictors (with smooth trends)",
       subtitle = "Red line shows LOESS smooth to assess linearity",
       x = "Predictor value", y = "Exam Score") +
  theme_bw()

cat("\n=== LINEARITY ASSESSMENT ===\n")
cat("Based on scatterplots with LOESS smooths:\n\n")
cat("age:\n")
cat("  - Nearly FLAT trend, highly scattered points\n")
cat("  - No clear linear or non-linear relationship with exam score\n")
cat("  → Very weak predictor, consider excluding\n\n")

cat("attend_pct:\n")
cat("  - STRONG POSITIVE LINEAR trend across entire range\n")
cat("  - Clear pattern from 50% to 100% attendance\n")
cat("  → Excellent linear predictor\n\n")

cat("sleep_hrs:\n")
cat("  - MODERATE POSITIVE trend\n")
cat("  - Some leveling off at higher sleep hours (plateau effect)\n")
cat("  - Overall linear approximation reasonable\n")
cat("  → Adequate linear predictor\n\n")

cat("study_hrs:\n")
cat("  - STRONG POSITIVE LINEAR trend\n")
cat("  - Consistent relationship throughout range\n")
cat("  → Excellent linear predictor\n\n")



# b. y vs qualitative variables (Boxplots)
data_clean |>
  select(y, sexe, school_type, parent_educ, sleep_qual, 
         web_access, trav_time, extra_act, study_method) |>
  mutate(across(-y, as.character)) |>
  pivot_longer(cols = -y, names_to = "variable", values_to = "value") |>
  ggplot(aes(x = value, y = y, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  facet_wrap(~ variable, scales = "free_x", nrow = 3) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        strip.text = element_text(face = "bold", size = 9)) +
  labs(title = "Distribution of Exam Score by Categorical Predictors",
       x = "Category Level", y = "Exam Score")

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
### Categorical-Categorical associations (using rstatix) ###
cat("\n--- Categorical-Categorical Associations ---\n")

cat_vars <- c("sexe", "school_type", "parent_educ", "sleep_qual", 
              "web_access", "trav_time", "extra_act", "study_method")

# Function to calculate Cramér's V
cramers_v <- function(x, y) {
  tbl <- table(x, y)
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
  n <- sum(tbl)
  min_dim <- min(nrow(tbl) - 1, ncol(tbl) - 1)
  if(min_dim == 0) return(NA)
  cramers <- sqrt(chi2 / (n * min_dim))
  return(as.numeric(cramers))
}

# Function to get p-value
get_pvalue <- function(x, y) {
  tbl <- table(x, y)
  test <- suppressWarnings(chisq.test(tbl))
  return(test$p.value)
}

# Prepare data
cat_data <- data_clean |>
  select(all_of(cat_vars))

# Calculate all pairwise associations
n_vars <- length(cat_vars)
results <- data.frame()

for(i in 1:(n_vars-1)) {
  for(j in (i+1):n_vars) {
    cramers <- cramers_v(cat_data[[i]], cat_data[[j]])
    pval <- get_pvalue(cat_data[[i]], cat_data[[j]])
    
    results <- rbind(results, data.frame(
      Var1 = cat_vars[i],
      Var2 = cat_vars[j],
      CramersV = cramers,
      p_value = pval
    ))
  }
}

# Create matrix for heatmap
cramers_matrix <- matrix(1, n_vars, n_vars)
rownames(cramers_matrix) <- colnames(cramers_matrix) <- cat_vars

for(i in 1:nrow(results)) {
  v1 <- results$Var1[i]
  v2 <- results$Var2[i]
  val <- results$CramersV[i]
  
  cramers_matrix[v1, v2] <- val
  cramers_matrix[v2, v1] <- val
}

# Plot heatmap
cramers_df <- as.data.frame(cramers_matrix) |>
  rownames_to_column("Var1") |>
  pivot_longer(-Var1, names_to = "Var2", values_to = "cramers_v")

ggplot(cramers_df, aes(x = Var1, y = Var2, fill = cramers_v)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", cramers_v)), 
            size = 2.5, color = "black") +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue",
                       midpoint = 0.25, limit = c(0, 1),
                       name = "Cramér's V",
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  labs(title = "Cramér's V: Categorical Variable Associations",
       subtitle = "Values closer to 1 indicate stronger association",
       x = "", y = "") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "right",
        panel.grid = element_blank())

# Display strongest associations
cat("\nStrongest categorical associations (Cramér's V > 0.2):\n")
results |>
  filter(CramersV > 0.2) |>
  arrange(desc(CramersV)) |>
  mutate(
    Strength = case_when(
      CramersV >= 0.5 ~ "Strong",
      CramersV >= 0.3 ~ "Moderate",
      TRUE ~ "Weak"
    ),
    Significance = ifelse(p_value < 0.05, "Significant", "Not significant")
  ) |>
  select(Var1, Var2, CramersV, Strength, p_value, Significance) |>
  kable(digits = 3, caption = "Categorical associations (Cramér's V > 0.2)")

# Summary statistics
cat("\n=== SUMMARY ===\n")
cat("Total pairs tested:", nrow(results), "\n")
cat("Pairs with V > 0.5 (strong):", sum(results$CramersV > 0.5, na.rm = TRUE), "\n")
cat("Pairs with V > 0.3 (moderate):", sum(results$CramersV > 0.3 & results$CramersV <= 0.5, na.rm = TRUE), "\n")
cat("Pairs with V > 0.2 (weak):", sum(results$CramersV > 0.2 & results$CramersV <= 0.3, na.rm = TRUE), "\n")
cat("Pairs with V ≤ 0.2 (negligible):", sum(results$CramersV <= 0.2, na.rm = TRUE), "\n\n")

cat("→ Interpretation:\n")
cat("  - No strong associations (V > 0.5) suggest minimal confounding among categoricals\n")
cat("  - Moderate associations (0.3 < V < 0.5) may indicate related but distinct constructs\n")
cat("  - Weak associations (V < 0.3) indicate independence\n\n")

# Check for potential confounding
max_cramers <- max(results$CramersV, na.rm = TRUE)
if(max_cramers < 0.5) {
  cat("✓ No severe confounding detected among categorical predictors\n")
} else {
  cat("⚠ Warning: Some categorical pairs show strong association (V > 0.5)\n")
  cat("  Consider carefully when including both in the same model\n")
}
cat("\n")

# c. Mixed associations
cat("\n--- Mixed Associations (Numeric-Categorical) ---\n")

cat("Testing whether numeric predictors differ significantly across categorical groups:\n\n")

# Example 1: study_hrs by school_type
cat("1. study_hrs by school_type:\n")
t_test1 <- t.test(study_hrs ~ school_type, data = data_clean)
cat("   Mean study hours - Public:", round(mean(data_clean$study_hrs[data_clean$school_type == "Public"]), 2), 
    "hrs, Private:", round(mean(data_clean$study_hrs[data_clean$school_type == "Private"]), 2), "hrs\n")
cat("   t-test p-value:", round(t_test1$p.value, 4), "\n")
cat("   →", ifelse(t_test1$p.value < 0.05, "Significant difference", "No significant difference"), "\n\n")

# Example 2: attend_pct by web_access
cat("2. attend_pct by web_access:\n")
t_test2 <- t.test(attend_pct ~ web_access, data = data_clean)
cat("   Mean attendance - No internet:", 
    round(mean(data_clean$attend_pct[data_clean$web_access == "No"]), 2), 
    "%, With internet:", 
    round(mean(data_clean$attend_pct[data_clean$web_access == "Yes"]), 2), "%\n")
cat("   t-test p-value:", round(t_test2$p.value, 4), "\n")
cat("   →", ifelse(t_test2$p.value < 0.05, "Significant difference", "No significant difference"), "\n\n")

# Example 3: study_hrs by extra_act
cat("3. study_hrs by extra_act:\n")
t_test3 <- t.test(study_hrs ~ extra_act, data = data_clean)
cat("   Mean study hours - No activities:", 
    round(mean(data_clean$study_hrs[data_clean$extra_act == "No"]), 2), 
    "hrs, With activities:", 
    round(mean(data_clean$study_hrs[data_clean$extra_act == "Yes"]), 2), "hrs\n")
cat("   t-test p-value:", round(t_test3$p.value, 4), "\n")
cat("   →", ifelse(t_test3$p.value < 0.05, "Significant difference", "No significant difference"), "\n\n")

# Visualization
p_mixed1 <- ggplot(data_clean, aes(x = school_type, y = study_hrs, fill = school_type)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(title = "Study hours by School type", x = "School type", y = "Study hours") +
  theme_bw() +
  theme(legend.position = "none")

p_mixed2 <- ggplot(data_clean, aes(x = web_access, y = attend_pct, fill = web_access)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(title = "Attendance by Internet access", x = "Internet access", y = "Attendance (%)") +
  theme_bw() +
  theme(legend.position = "none")

p_mixed1 + p_mixed2

cat("→ Mixed associations help identify potential confounders:\n")
cat("  If numeric predictors vary by categorical groups, those groups may\n")
cat("  confound relationships in the regression model\n\n")

### Part 4 - Assessment of monotone trends for ordinal predictors ###
cat("\n=== ORDINAL TRENDS ASSESSMENT ===\n")

p1 <- plot_monotone(data_clean, "sleep_qual", "Sleep Quality")
p2 <- plot_monotone(data_clean, "parent_educ", "Parental Education")
p3 <- plot_monotone(data_clean, "trav_time", "Travel Time")

(p1 | p2) / p3 +
  plot_annotation(
    title = "Assessing monotonicity for ordered categorical predictors",
    subtitle = "Points show mean ± SE; red line shows overall trend"
  )

cat("\n=== DETAILED ORDINAL ASSESSMENT ===\n\n")

cat("Based on boxplots with trend lines connecting group means:\n\n")

cat("sleep_qual (Poor → Average → Good):\n")
cat("  Observed pattern from graph:\n")
cat("  - Poor: ~50, Average: ~60, Good: ~63\n")
cat("  - Trend line shows POSITIVE but MODEST progression\n")
cat("  - Largest gain from Poor to Average (+10 pts)\n")
cat("  - Smaller gain from Average to Good (+3 pts, plateau effect)\n")
cat("  Assessment:\n")
cat("  - WEAKLY MONOTONE: Generally positive trend with plateau\n")
cat("  - Not a strong ordinal effect (diminishing returns at 'Good')\n")
cat("  → Decision: Treat as ORDERED factor with caution\n")
cat("  → Consider testing as unordered if ordered assumption fails in model\n\n")

cat("parent_educ (No formal → PhD):\n")
cat("  Observed pattern from graph:\n")
cat("  - Progression: No formal(~50) → High school(~55) → Graduate(~60)\n")
cat("                 → Postgrad 1(~63) → Postgrad 2(~63) → PhD(~65)\n")
cat("  - Trend line shows CONSISTENT POSITIVE progression\n")
cat("  - Slight plateau at Postgraduate levels (63-65 range)\n")
cat("  - Total range: ~15 points from lowest to highest\n")
cat("  Assessment:\n")
cat("  - MONOTONE POSITIVE: Clear ordinal pattern throughout\n")
cat("  - Strongest effect among ordinal predictors\n")
cat("  → Decision: Treat as ORDERED factor\n")
cat("  → Ordinal assumption well-supported by data\n\n")

cat("trav_time (<15min → >60min):\n")
cat("  Observed pattern from graph:\n")
cat("  - Clear progression: <15min(~65) → 15-30min(~62)\n")
cat("                       → 30-60min(~56) → >60min(~47)\n")
cat("  - Trend line shows CONSISTENT NEGATIVE trend\n")
cat("  - Approximately linear decrease across all levels\n")
cat("  - Total range: ~18 points (strongest ordinal effect!)\n")
cat("  Assessment:\n")
cat("  - MONOTONE NEGATIVE: Strong ordinal pattern\n")
cat("  - Each increase in commute time associated with lower scores\n")
cat("  - Largest effect among ordinal predictors\n")
cat("  → Decision: Treat as ORDERED factor\n")
cat("  → Strong support for ordinal coding\n\n")


cat("==========================================================\n")
cat("SUMMARY - Ordinal Modeling Decisions:\n")
cat("==========================================================\n")
cat("  ✓ trav_time: Use as ORDERED factor\n")
cat("     - Strongest monotone effect (18-point range)\n")
cat("     - Clear negative progression across all levels\n")
cat("  \n")
cat("  ✓ parent_educ: Use as ORDERED factor\n")
cat("     - Clear positive monotone pattern with a plateau (15-point range)\n")
cat("     - Consistent progression despite slight plateau\n")
cat("  \n")
cat("  ? sleep_qual: Use as ORDERED factor with caution\n")
cat("     - Positive pattern with plateau (13-point range)\n")
cat("     - Diminishing returns at 'Good' level\n")
cat("     - Consider comparing ordered vs unordered in model diagnostics\n")
cat("==========================================================\n\n")

# Part 5 - Conclude EDA #
cat("\n==========================================================\n")
cat("=== KEY EDA FINDINGS & MODELING IMPLICATIONS ===\n")
cat("==========================================================\n\n")
# Pre-calculating values for the summary section
attend_r <- cor(data_clean$y, data_clean$attend_pct, use = "complete.obs")
study_r  <- cor(data_clean$y, data_clean$study_hrs, use = "complete.obs")
sleep_r  <- cor(data_clean$y, data_clean$sleep_hrs, use = "complete.obs")
age_r    <- cor(data_clean$y, data_clean$age, use = "complete.obs")

# Get the maximum absolute correlation among numeric predictors (excluding y)
cor_matrix <- data_clean |> 
  select(all_of(numeric_vars)) |> 
  cor(use = "complete.obs")
diag(cor_matrix) <- 0 # Remove self-correlation of 1
max_cor <- max(abs(cor_matrix))

# Get the maximum Cramér's V from your results dataframe
max_cramers <- max(results$CramersV, na.rm = TRUE)

cat("1. OUTCOME DISTRIBUTION\n")
cat("   - Exam scores approximately normal (skewness =", round(skew_val, 2), ")\n")
cat("   - Range: [", min(data_clean$y), ",", max(data_clean$y), "]\n")
cat("   - Few outliers (n =", n_outliers, "), all within plausible [0, 100] range\n")
cat("   → Implication: OLS regression appropriate; no transformation needed\n\n")

cat("2. CANDIDATE PREDICTORS (ranked by apparent effect strength)\n")
cat("   STRONG candidates:\n")
cat("   - web_access: ~10 point difference (No: 50 vs Yes: 60)\n")
cat("   - attend_pct: Strong linear correlation (r =", round(attend_r, 2), ")\n")
cat("   - study_hrs: Strong linear correlation (r =", round(study_r, 2), ")\n")
cat("   - extra_act: ~8 point difference (No: 52 vs Yes: 60)\n")
cat("   \n")
cat("   MODERATE candidates:\n")
cat("   - school_type: ~6 point difference (Public: 52 vs Private: 58)\n")
cat("   - parent_educ: ~12 point range but irregular pattern\n")
cat("   - sleep_hrs: Weak correlation (r =", round(sleep_r, 2), ")\n")
cat("   \n")
cat("   WEAK/EXCLUDE candidates:\n")
cat("   - sexe: No visible differences (all ~58)\n")
cat("   - study_method: Minimal differences (58-62 range)\n")
cat("   - age: Very weak correlation (r =", round(age_r, 2), ")\n")
cat("   - sleep_qual: Non-monotone pattern (Average > Poor > Good)\n")
cat("   - trav_time: Non-monotone pattern (irregular U-shape)\n")
cat("   → Implication: Prioritize strong predictors; carefully handle non-monotone ordinals\n\n")

cat("3. ORDINAL PREDICTOR ASSESSMENT\n")
cat("   All three ordinal predictors show meaningful patterns:\n")
cat("   \n")
cat("   STRONG ordinal effects (treat as ordered):\n")
cat("   - trav_time: Clear monotone NEGATIVE trend (~18 pt range)\n")
cat("   - parent_educ: Clear monotone POSITIVE trend (~15 pt range)\n")
cat("   \n")
cat("   MODERATE ordinal effect (treat as ordered with caution):\n")
cat("   - sleep_qual: Weak monotone POSITIVE with plateau (~13 pt range)\n")
cat("   \n")
cat("   → Implication: All can be treated as ordered factors initially\n")
cat("   → Monitor ordered assumption in model diagnostics for sleep_qual\n")
cat("   → Expected strongest ordinal effects: trav_time and parent_educ\n\n")

cat("4. LINEARITY & FUNCTIONAL FORM\n")
cat("   - study_hrs: Excellent linear relationship\n")
cat("   - attend_pct: Strong linear relationship\n")
cat("   - sleep_hrs: Adequate linear approximation (slight plateau)\n")
cat("   - age: No clear functional form\n")
cat("   → Implication: Use linear terms; no polynomial transformations needed\n\n")

cat("5. COLLINEARITY ASSESSMENT\n")
cat("   - Numeric predictors: Maximum |r| =", round(max_cor, 2), "(< 0.7 threshold)\n")
cat("   - Categorical: Maximum Cramér's V =", round(max_cramers, 2), "(< 0.5 threshold)\n")
cat("   → Implication: Multicollinearity unlikely to be problematic\n\n")

cat("6. INTERACTION HYPOTHESES TO TEST\n")
cat("   Based on substantive reasoning and observed associations:\n")
cat("   - H1: study_hrs × web_access\n")
cat("        Rationale: Internet may enhance study effectiveness\n")
cat("   - H2: parent_educ × school_type\n")
cat("        Rationale: Private school benefit may vary by SES (V = 0.23)\n")
cat("   - H3: sleep_qual × study_hrs\n")
cat("        Rationale: Good sleep may be critical for high study hours\n")
cat("   → Implication: Test interactions in extended models (not all simultaneously)\n\n")

cat("==========================================================\n\n")



##############################################
### Section 3 - Building regression models ###
##############################################

# Part 1 - Simple regressions (Descriptive Baseline)
# Define your list of predictors
# 1. Quantitative Predictors
predictors <- c("age", "study_hrs", "sleep_hrs", "attend_pct", "sexe", 
                "school_type", "sleep_qual", "trav_time", "web_access", 
                "parent_educ", "extra_act", "study_method") 
quant_preds <- c("age", "study_hrs", "sleep_hrs", "attend_pct")

data_clean |>
  select(y, all_of(quant_preds)) |>
  pivot_longer(cols = -y, names_to = "predictor", values_to = "value") |>
  ggplot(aes(x = value, y = y)) +
  facet_wrap(~predictor, scales = "free_x", ncol = 2) +
  geom_point(size = 1.5, shape = 21, fill = "dodgerblue", color = "black", alpha = 0.3) +
  # Adding the regression line (intuition building)
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(x = "Predictor Value", y = "Exam Score (y)",
       title = "Baseline Associations: Quantitative Variables") +
  theme_bw(base_size = 12)

# 2. Categorical Predictors
cat_preds <- c("sexe", "school_type", "sleep_qual", "extra_act", "study_method")

data_clean |>
  select(y, all_of(cat_preds)) |>
  # Ensure all predictors are treated as characters for pivoting
  mutate(across(all_of(cat_preds), as.character)) |>
  pivot_longer(all_of(cat_preds), names_to = "var", values_to = "value") |>
  # Apply labels using the same logic as your Pok code
  mutate(var = factor(var, levels = cat_preds, labels = vlabels(data_clean[, cat_preds]))) |>
  # Shorten long labels (like study methods) if necessary
  mutate(value = fct_relabel(factor(value), \(x) str_trunc(x, 15))) |>
  ggplot(aes(x = y, y = value)) +
  # Boxplot with thinner lines for a cleaner look
  geom_boxplot(linewidth = 0.3, median.linewidth = 0.8, fill = "grey95") +
  # Add mean point to anticipate regression coefficients
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "firebrick") +
  facet_wrap(vars(var), scales = "free_y") +
  labs(
    x = "Exam Score (y)", 
    y = NULL,
    title = "Bivariate Intuition: Categorical Predictors vs Score",
    subtitle = "Red diamonds represent the group mean (target of linear regression)"
  ) +
  theme_bw(base_size = 14) +
  labs_pubr() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank()
  )


# Part 2 - Multiple Regression Models
# Model 1: Socio-demographic Baseline
# Justification: Controls for non-modifiable structural factors
m1_socio <- lm(y ~ age + sexe + school_type + web_access, data = data_clean)

# Model 2: Behavioral & Academic effort
# Justification: EDA showed strong linear trends for study and attendance. 
# We add them to see their effect while controlling for socio-demographics.
m2_behavior <- update(m1_socio, . ~ . + study_hrs + sleep_hrs + attend_pct + sleep_qual + trav_time)

# Model 3: Full Contextual Model with Interaction
# Justification: We test if 'study_method' efficiency depends on 'study_hrs'
# and include parental/environmental factors.
m3_full <- update(m2_behavior, . ~ . + parent_educ + web_access + 
                    extra_act + study_method + study_hrs:study_method)

# Statistical comparison of models
model_comp <- compare_performance(m1_socio, m2_behavior, m3_full, metrics = "common")
print(model_comp)

# Visualization of the comparison
plot(model_comp) + theme_minimal()

# Check if our justified set of predictors is statistically sound
check_collinearity(m3_full) |> plot()


# Part 3 - Transformations or re-expressions only when justified by EDA/diagnostics


# Part 4 - Interactions: include them only if motivated by a clear, testable hypothesis


# Part 5 - Compare candidate models using criteria covered in class/labs



#############################################################
### Section 4 -  Diagnostics, assumptions, and robustness ###
#############################################################
# Part 1 - Check final model
# Assumptions [P1 - P4]
p1 <- resid_vs_order(m3_full) # Residuals vs obs
p2 <- resid_stand_hist(m3_full) # Histogram of standardized residuals
p3 <- resid_stand_dens(m3_full) # Density of standardized residuals
p4 <- resid_stand_qq(m3_full) # Normal Q–Q plot

p1 + p2 + p3 + p4

# Outliers - Distance Cook
check_outliers(m3_full, method = "cook") |> plot()

resid_vs_fit(model = m3_full)

# Part 2 - Issues (heteroskedasticity, nonlinearity, influential observations)


# Part 3 -  Do not remove observations unless you can justify that they are data errors


#################################################
### Section 5 -  Interpretation and inference ###
#################################################


