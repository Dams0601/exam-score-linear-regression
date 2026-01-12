



resid_vs_fit <- function(
    model, which = c("res", "abs_rstand", "rstud", "rstand"), smooth_method = "lm", 
    jitter = FALSE, size = 2, shape = 21, fill = "red", color = "black"
) {
  
  which <- match.arg(which)
  res <- switch(
    which, res = residuals(model), rstand = rstandard(model), 
    abs_rstand = sqrt(abs(rstandard(model))), rstud = rstudent(model)
  )
  res_name <- switch(
    which, res = "Residuals", abs_rstand = expression(sqrt(abs("Standardized residuals"))), 
    rstud = "Studentized residuals", rstand = "Standardized residuals"
  )
  
  p <- augment(model) |> 
    mutate(res = res) |> 
    ggplot(aes(x = .fitted, y = res)) 
  if (jitter) {
    p <- p + geom_jitter(width = 0.5, size = size, shape = shape, fill = fill, color = color, alpha = 0.5) 
  } else {
    p <- p + geom_point(size = size, shape = shape, fill = fill, color = color) 
  }
  
  p + geom_smooth(method = smooth_method, se = FALSE) +
    labs(x = "Fitted values", y = res_name) +
    theme_bw(base_size = 14) +
    labs_pubr()
}



resid_vs_var_numeric <- function(
    model, numeric_vars, which = c("res", "abs_rstand", "rstud", "rstand"), 
    facet_ncol = 2, smooth_method = "lm", size = 2, shape = 21, fill = "red", color = "black"
) {
  which <- match.arg(which)
  res <- switch(
    which, res = residuals(model), rstand = rstandard(model), 
    abs_rstand = sqrt(abs(rstandard(model))), rstud = rstudent(model)
  )
  res_name <- switch(
    which, res = "Residuals", abs_rstand = expression(sqrt(abs("Standardized residuals"))), 
    rstud = "Studentized residuals", rstand = "Standardized residuals"
  )
  
  row_to_remove <-  as.numeric(model$na.action)
  used_data <- eval(model$call$data) |> 
    select(all_of(numeric_vars))
  if (length(row_to_remove) != 0) used_data <- slice(used_data, -row_to_remove)
  
  labels <- vlabels(used_data[numeric_vars])
  labels <- ifelse(is.na(labels), names(labels), labels)
  
  diag_data <- mutate(used_data, res = res)
  
  p <- diag_data |>
    frename(labels, cols = numeric_vars) |>
    pivot_longer(-res, names_to = "name", values_to = "value") |>
    mutate(name = as_factor(name)) |>
    ggplot(aes(x = value, y = res)) +
    facet_wrap(vars(name), ncol = facet_ncol, scales = "free_x") +
    geom_point(size = size, shape = shape, fill = fill, color = color, na.rm = TRUE) +
    geom_smooth(method = smooth_method, se = FALSE, na.rm = TRUE) +
    labs(x =NULL, y = res_name) +
    theme_bw(base_size = 14) +
    labs_pubr()
  p
}



resid_vs_var_factor <- function(
    model, factor_vars, which = c("res", "abs_rstand", "rstud", "rstand"),
    facet_ncol = 2, size = 2, shape = 21, fill = "blue", color = "black"
) {
  
  which <- match.arg(which)
  res <- switch(
    which, res = residuals(model), rstand = rstandard(model), 
    abs_rstand = sqrt(abs(rstandard(model))), rstud = rstudent(model)
  )
  res_name <- switch(
    which, res = "Residuals", abs_rstand = expression(sqrt(abs(r[Standardized]))), 
    rstud = expression(r[Studentized]), rstand = expression(r[Standardized])
  )
  
  row_to_remove <-  as.numeric(model$na.action)
  used_data <- eval(model$call$data) |> 
    select(all_of(factor_vars))
  if (length(row_to_remove) != 0) used_data <- slice(used_data, -row_to_remove)
  
  labels <- vlabels(used_data[factor_vars])
  labels <- ifelse(is.na(labels), names(labels), labels)
  
  diag_data <- mutate(used_data, res = res)
  
  
  p <- diag_data |>
    frename(labels, cols = factor_vars) |>
    pivot_longer(-res, names_to = "name", values_to = "value") |>
    mutate(name = as_factor(name)) |>
    ggplot(aes(x = value, y = res)) +
    facet_wrap(vars(name), ncol = facet_ncol, scales = "free_x") +
    geom_boxplot(outlier.shape = NA, fill = "grey90", alpha = 0.5, linewidth = 0.25) +
    geom_jitter(width = 0.1, size = size, shape = shape, fill = fill, color = color, alpha = 0.3) +
    # geom_violin(fill = NA, linewidth = 0.75, show.legend = FALSE, adjust = 1, color = "dodgerblue") +
    labs(x = NULL, y = res_name) +
    theme_bw(base_size = 14) +
    labs_pubr()
  p
}



resid_vs_predictors <- function(model, predictors, facet_ncol = 2, smooth_method = "loess") {
  aug_df <- augment(model)
  aug_df |>
    select("Fitted values" = .fitted, all_of(predictors), res = .resid) |>
    frename(vlabels(aug_df[, predictors]), cols = predictors) |>
    pivot_longer(- res, names_to = "name", values_to = "value") |>
    mutate(name = as_factor(name)) |>
    ggplot(aes(x = value, y = res)) +
    facet_wrap(vars(name), ncol = facet_ncol, scales = "free_x") +
    geom_point(size = 2, shape = 21, fill = "red", color = "black") +
    geom_smooth(method = smooth_method, se = FALSE) +
    labs(x = "Value", y = "Residuals") +
    theme_bw(base_size = 14) +
    labs_pubr()
}


resid_stand_vs_predictors  <- function(model, predictors, facet_ncol = 2, smooth_method = "loess") {
  aug_df <- augment(model)
  mutate(aug_df, res = sqrt(abs(.std.resid))) |> 
    select("Fitted values" = .fitted, all_of(predictors), res) |>
    frename(vlabels(aug_df[, predictors]), cols = predictors) |>
    pivot_longer(- res, names_to = "name", values_to = "value") |>
    mutate(name = as_factor(name)) |> 
    ggplot(aes(x = value, y = res)) +
    facet_wrap(vars(name), ncol = 2, scales = "free_x") +
    geom_point(size = 2, shape = 21,  fill = "red", color = "black", alpha = 1) +
    geom_smooth(method = smooth_method, se = FALSE)  +
    labs(x = "Value", y = expression(sqrt(abs("Standardized residuals")))) +
    theme_bw(base_size = 14) +
    labs_pubr()
}



resid_stud_vs_predictors  <- function(model, predictors, facet_ncol = 2, smooth_method = "loess") {
  aug_df <- augment(model)
  mutate(aug_df, res = rstudent(model)) |> 
    select("Fitted values" = .fitted, all_of(predictors), res) |>
    frename(vlabels(aug_df[, predictors]), cols = predictors) |>
    pivot_longer(- res, names_to = "name", values_to = "value") |>
    mutate(name = as_factor(name)) |> 
    ggplot(aes(x = value, y = res)) +
    facet_wrap(vars(name), ncol = 2, scales = "free_x") +
    geom_point(size = 2, shape = 21,  fill = "red", color = "black", alpha = 1) +
    geom_smooth(method = smooth_method, se = FALSE)  +
    labs(x = "Value", y = "Studentized Residuals") +
    theme_bw(base_size = 14) +
    labs_pubr()
}

resid_vs_order <- function(model, smooth_method = "loess") {
  tibble(res = resid(model), obs = seq_along(res)) |> 
    ggplot(aes(x = obs, y = res)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point(shape = 21, size = 2) +
    geom_smooth(method = smooth_method, se = FALSE)  +
    scale_x_continuous(breaks = breaks_pretty(n = 5)) +
    labs(x = "Observation", y = "Residuals") +
    theme_bw(base_size = 14) + 
    labs_pubr()
}

resid_stand_hist <- function(model, bins = 12) {
  tibble(res = rstandard(model)) |> 
    ggplot(aes(x = res)) +
    geom_histogram(bins = bins, color = "black", fill = "dodgerblue") +
    scale_y_continuous(expand = expansion(c(0, 0.05)), breaks = breaks_pretty(n = 5)) +
    scale_x_continuous(breaks = breaks_pretty(n = 5)) +
    labs(x = "Standardized residuals", y = "Count") +
    theme_bw(base_size = 14) +
    labs_pubr()
}

resid_stand_dens <- function(model, bins = 12) {
  tibble(res = rstandard(model)) |> 
    ggdensity(x = "res", fill = "dodgerblue") +
    scale_x_continuous(limits = c(-4, 4)) +
    stat_overlay_normal_density(color = "red", linetype = 1, linewidth = 1) +
    scale_y_continuous(expand = expansion(c(0, 0.05)), breaks = breaks_pretty(n = 5)) +
    scale_x_continuous(breaks = breaks_pretty(n = 5)) +
    labs(x = "Standardized residuals", y = "Density") +
    theme_bw(base_size = 14) +
    labs_pubr()
}

resid_stand_qq <- function(model, bins = 12) {
  tibble(res = rstandard(model)) |> 
    ggplot(aes(sample = res)) +
    stat_qq_band(alpha = 0.2, fill = "blue") + # du package {qqplotr}
    stat_qq_line(color = "red") + # version du package {qqplotr}
    stat_qq_point(size = 0.5) +
    scale_y_continuous(breaks = breaks_pretty(n = 5)) +
    scale_x_continuous(breaks = breaks_pretty(n = 5)) +
    labs(y = "Sample quantile", x = "Theoretical quantile") +
    theme_bw(base_size = 14) +
    labs_pubr()
}

mean_and_sd <- function(x, digits = 1) {
  m <- style_number(fmean(x), digits = digits)  
  s <- style_number(fsd(x), digits = digits)
  glue("{m} ({s})") |> as.character()
}

mean_by_group <- function(data, x, by, digits = 1) {
  vlabx <- vlabels(data[[by]])
  labelx <- if (is.na(vlabx)) by else vlabx
  d <- select(data, all_of(c(x, by))) |>
    rename(y = 1, Variable = 2) |> 
    drop_na()
  tab <- group_by(d, Variable) |>
    summarise(N = n(), "Mean (SD)" = mean_and_sd(y, digits = digits)) |> 
    mutate(Variable = paste0("\U2000", Variable)) 
  bind_rows(c(Variable = labelx), tab)
}


tab_freq1 <- function(data, x, digits = 1) {
  df <- data[x]
  labels <- vlabels(df)
  
  map(df, \(v) fct_count(v, prop = TRUE)) |> 
    map(\(d) mutate(d, p = format_percent(p, digits = digits))) |> 
    map(\(d) mutate(d, f = paste0("\U2000", f))) |> 
    map2_dfr(labels, \(x, y) bind_rows(c(f = y), x)) |> 
    # ICI : On remplace tous les NA par du vide ""
    mutate(across(everything(), \(x) replace_na(as.character(x), ""))) |> 
    set_names(c("Variable", "Count (n)", "Percent (%)"))
}


scatter_plot <- function(data, x, y, size = 1, color = "black", ols_line = "red", nbreaks = 10, ...) {
  select(data, x = all_of(x), y = all_of(y)) |> 
    ggplot(aes(x = x, y = y)) +
    geom_point(size = size, color = color, ...) +
    geom_smooth(method = "lm", se = FALSE, color = ols_line) +
    scale_x_continuous(breaks = breaks_pretty(n = nbreaks)) +
    scale_y_continuous(breaks = breaks_pretty(n = nbreaks)) +
    theme_bw(base_size = 12)
}


# Function to plot monotone trends
plot_monotone <- function(data, var_name, label_name) {
  data %>%
    # Ensure we calculate the mean for each group to draw the trend line
    group_by(!!sym(var_name)) %>%
    mutate(mean_y = mean(y, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x = !!sym(var_name), y = y)) +
    geom_boxplot(fill = "steelblue", alpha = 0.2, outlier.alpha = 0.5) +
    # The trend line connecting the means
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linewidth = 1) +
    stat_summary(fun = mean, geom = "point", color = "red", size = 2) +
    theme_minimal() +
    labs(title = paste("Trend Analysis:", label_name),
         subtitle = "Red line connects group means to assess monotonicity",
         x = label_name, y = "Exam Score (y)")
}


