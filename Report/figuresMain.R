library(dplyr)
library(readr)
library(here)
library(tidyr)
library(stringr)
library(visOmopResults)
library(omopgenerics)
library(ggplot2)
library(forestploter)
library(latex2exp)
library(egg)
library(grid)
library(ggh4x)
library(ggtext)

# load functions
source(here("functions.R"))

# load data
load(here("shinyData-meta.Rdata"))

cdmMain <- c("CPRD GOLD", "SCIFI-PEARL", "SIDIAP", "UiO", "META-ANALYSIS")

estimates <- data$risk |>
  mutate(cdm_name = if_else(cdm_name == "UiO Algorithm", "UiO", toupper(cdm_name))) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  filter(
    cdm_name %in% cdmMain,
    covid_definition == "diagnostic_test",
    regression == "cox-sandwich",
    followup_end == "cohort_end_date_pregnancy",
    variable_name == "study",
    delivery_excluded %in% c("-", "yes"),
    window == "15_Inf",
    outcome != "icu_covid",
    !(cdm_name == "CPRD GOLD" & outcome == "inpatient_covid")
  ) |>
  select(!estimate_type) |>
  left_join(
    data$survival_summary |>
      mutate(cdm_name = if_else(cdm_name == "UiO Algorithm", "UiO", toupper(cdm_name))) |>
      filter(grepl("count", estimate_name)) |>
      select(!estimate_type) |>
      pivot_wider(names_from = c("exposed", "estimate_name"), values_from = "estimate_value") |>
      union_all(
        data$survival_summary |>
          filter(grepl("count", estimate_name)) |>
          select(!estimate_type) |>
          pivot_wider(names_from = c("exposed", "estimate_name"), values_from = "estimate_value") |>
          group_by(comparison, covid_definition, strata_name, strata_level, window, followup_end, exposed_censoring, variable_name, outcome, delivery_excluded) |>
          summarise(unexposed_count = sum(unexposed_count, na.rm = TRUE), exposed_count = sum(exposed_count, na.rm = TRUE),
                    unexposed_count_events = sum(unexposed_count_events, na.rm = TRUE), exposed_count_events = sum(exposed_count_events, na.rm = TRUE), .groups = "drop") |>
          mutate(cdm_name = "META-ANALYSIS")
      )
  ) |>
  mutate(
    comparison = factor(
      comparison, levels = c("none_first", "complete_booster"),
      labels = c("Primary Vaccination Schema vs. Unvaccination", "Booster vs. Primary Vaccination Schema")
    ),
    cdm_name = factor(
      cdm_name, levels = c("CPRD GOLD", "SCIFI-PEARL", "SIDIAP", "UiO", "META-ANALYSIS")
    ),
    outcome = factor(
      outcome, levels = c("covid", "inpatient_covid"), labels = c("COVID-19 infection", "COVID-19-related hospitalisation")
    ),
    across(
      all_of(c("coef", "se_coef", "exp_coef", "z", "p", "lower_ci", "upper_ci", "i2")),
      ~ if_else(is.na(unexposed_count) | is.na(exposed_count) | is.na(unexposed_count_events) | is.na(exposed_count_events), NA, .x)
    ),
    unexposed_count_events = if_else(
      is.na(unexposed_count_events), "<5",
      paste0(
        niceNum(unexposed_count_events), " (", niceNum((unexposed_count_events/unexposed_count)*100, dec = 2),"%)"
      )
    ),
    exposed_count_events = if_else(
      is.na(exposed_count_events), "<5",
      paste0(
        niceNum(exposed_count_events), " (", niceNum((exposed_count_events/exposed_count)*100, dec = 2),"%)"
      )
    ),
    across(
      all_of(c("unexposed_count", "exposed_count")),
      ~ if_else(is.na(.x), "<5", as.character(niceNum(.x)))
    ),
    `HR (95% CI)` = if_else(
      !is.na(exp_coef), sprintf("   %.2f (%.2f - %.2f)", exp_coef, lower_ci, upper_ci), ""
    ),
    "VE" =  if_else(
      !is.na(exp_coef), paste0(niceNum((1-exp_coef)*100, dec = 2), "%"), ""
    )
  ) |>
  select(!c("covid_definition", "exposed_censoring", "followup_end", "window", "variable_name", "delivery_excluded", "regression")) |>
  rename(
    "Database" = "cdm_name"
    # "Control\nPopulation, N" = "unexposed_count",
    # "Control\nEvents, N(%)" = "unexposed_count_events",
    # "Exposed\nPopulation, N" = "exposed_count",
    # "Exposed\nEvents, N(%)" = "exposed_count_events"
  )

# Overall figs ----
comparisonNms <- c("Primary Vaccination Schema vs. Unvaccination", "Booster vs. Primary Vaccination Schema")
for (ii in 1:2) {
  fig1 <- estimates |>
    filter(comparison == comparisonNms[ii], strata_name == "overall") |>
    select(!starts_with("strata")) |>
    arrange(outcome, Database) |>
    mutate(
      Database = paste0("      ", Database)
      , " " = "                         ",
      "  " = " ",
      i2 = as.character(round(i2,4))
    ) |>
    select(outcome, Database, starts_with("exposed"), "  ", starts_with("unexposed"), " ",
           starts_with("HR"), "VE", "i2", "exp_coef", "lower_ci", "upper_ci", "se_coef")

  fig1 <- bind_rows(
    tibble(Database = "COVID-19 infection"),
    fig1[fig1$outcome == "COVID-19 infection", 2:15],
    tibble(Database = ""),
    tibble(Database = "COVID-19-related hospitalisation"),
    fig1[fig1$outcome == "COVID-19-related hospitalisation", 2:15]
  ) |>
    mutate(across(any_of(colnames(fig1[,1:9])), ~if_else(is.na(.x), "", .x)))

  thm <- forest_theme(
    base_size = 11,
    base_family = "",
    ci_pch = 15,
    ci_col = "black",
    ci_alpha = 1,
    ci_fill = NULL,
    ci_lty = 1,
    ci_lwd = 1,
    ci_Theight = NULL,
    legend_name = "Group",
    legend_position = "right",
    legend_value = "",
    # legend_gp = gpar(fontsize = base_size, fontfamily = base_family, cex = 1),
    # xaxis_gp = gpar(fontsize = base_size, fontfamily = base_family, lwd = 0.6, cex = 1),
    # refline_gp = gpar(lwd = 1, lty = "dashed", col = "grey20"),
    vertline_lwd = 1,
    vertline_lty = "dashed",
    vertline_col = "red",
    summary_col = "#4575b4",
    # summary_fill = summary_col,
    # footnote_gp = gpar(fontsize = base_size, fontfamily = base_family, cex = 0.6, fontface
    #                    = "plain", col = "black"),
    footnote_parse = TRUE,
    title_just = c("center"),
    # title_gp = gpar(cex = 1.2, fontface = "bold", col = "black", fontfamily = base_family),
    arrow_type = c("open", "closed"),
    arrow_label_just = c("start", "end"),
    arrow_length = 0.05,
    # arrow_gp = gpar(fontsize = base_size, fontfamily = base_family, lwd = 0.6),
    # xlab_adjust = c("refline", "center"),
    # xlab_gp = gpar(fontsize = base_size, fontfamily = base_family, cex = 1, fontface =
    #                  "plain"),
    padding = unit(100, "mm")
  )

  fig1Forestploter <- fig1[,1:10]
  colnames(fig1Forestploter) <- c(
    "", "Population (N)", "Events (N(%))", " ", "Population (N)", "Events (N(%))", " ", " ", "VE", "i2"
  )
  fig1Forestploter[is.na(fig1Forestploter)] <- ""

  p <- forest(
    fig1Forestploter,
    est = fig1$exp_coef,
    lower = fig1$lower_ci,
    upper = fig1$upper_ci,
    sizes = 0.5,
    x_trans = "log10",
    ref_line = 1,
    vert_line = NULL,
    ci_column = 7,
    is_summary = c(rep(FALSE, 5), TRUE, rep(FALSE, 5), TRUE),
    xlim = NULL,
    ticks_at = NULL,
    ticks_digits = NULL,
    ticks_minor = NULL,
    arrow_lab = NULL,
    # xlab = "Hazard Ratio",
    footnote = NULL,
    title = NULL,
    nudge_y = 0,
    fn_ci = makeci,
    fn_summary = make_summary,
    index_args = NULL,
    theme = thm,
    graphwidth = unit(5, "cm")
  ) |>
    # HR header
    add_text(text = "HR (95% CI)",
             part = "header",
             col = 7:8,
             gp = gpar(fontface = "bold")) |>
    add_text(text = "Exposed",
             part = "header",
             row = 0,
             col = 2:3,
             gp = gpar(fontface = "bold")) |>
    add_text(text = "Unexposed",
             part = "header",
             row = 0,
             col = 5:6,
             gp = gpar(fontface = "bold")) |>
    add_text(text = "Database",
             part = "header",
             row = 1,
             col = 1,
             gp = gpar(fontface = "bold"),
             just = c("left")) |>
    add_border(part = "header",
               row = 0,
               col = 2:3,
               gp = gpar(lwd = 0.3, alpha = 0.5)) |>
    add_border(part = "header",
               row = 0,
               col = 5:6,
               gp = gpar(lwd = 0.3, alpha = 0.5)) |>
    add_border(part = "body",
               row = c(1, 8),
               where = c("bottom"),
               gp = gpar(lwd = 1)) |>
    add_border(part = "body",
               row = c(1, 8),
               where = c("top"),
               gp = gpar(lwd = 1)) |>
    edit_plot(col = 1, row = c(1, 8),
              which = "text",
              gp = gpar(fontface = "bold", fontsize = 12)) |>
    edit_plot(row = c(6, 12),
              which = "text",
              gp = gpar(fontface = "bold")) |>
    edit_plot(which = "background",
              gp = gpar(fill = "#ffffff")) |>
    edit_plot(row = c(2, 4, 6, 9, 11),
              which = "background",
              gp = gpar(fill = "#778da9", alpha = 0.1)) |>
    edit_plot(row = c(1, 8),
              which = "background",
              gp = gpar(fill = "#778da9", alpha = 0.3)) |>
    edit_plot(col = 7, row = c(6, 12), which = "ci",
              gp = gpar(col = "#415a77", fill = "#415a77"))

  p_wh <- get_wh(p)
  png(here::here(paste0("forest_main_", ii, ".png")), width = 3500, height = 1200, res = 300)
  plot(p)
  dev.off()
}


# By trimesters ---
fig2 <- estimates |>
  filter(strata_name == "trimester") |>
  splitStrata() |>
  mutate(
    trimester = factor(trimester, levels = c("T1", "T2", "T3"), labels = paste0("Trimester ", 1:3)),
    Database = factor(Database,
                      levels = rev(cdmMain)),
    meta = grepl("Meta", Database),
    Hetereogeneity = if_else(i2<=0.4 | is.na(i2) , "i2 <= 0.4", "i2>0.4")
  ) |>
  arrange(outcome, trimester, Database)
  # select(comparison, outcome, "Trimester" = "trimester", Database, starts_with("exposed"), starts_with("unexposed"), starts_with("HR"), "i2", "VE", "exp_coef", "lower_ci", "upper_ci", "se_coef")

color.background <- "#ffffff"
color.grid.major <- "#d9d9d9"
color.axis.text <- "#252525"
color.axis.title <- "#252525"
color.border <- "#595959"
fontsizeRef = 13
ggplot(fig2, aes(x = exp_coef, xmin = lower_ci, xmax = upper_ci, y = Database)) +
  # facet_nested(outcome ~ comparison + trimester, scales = "free_y") +
  facet_nested(trimester ~ comparison + outcome , scales = "free_y") +
  geom_point(aes(x = exp_coef, shape = Database, color = Database), size = 3) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci, color = Database, linetype = Hetereogeneity), size = 0.8) +
  # scale_y_continuous(breaks = y_breaks, labels = strata_level,
  #                    limits = c(0.5, max(y_breaks) + 0.5) ) +
  scale_x_continuous(limits = c(0.07, 4), breaks = c(0.1, 0.25, 0.5, 1, 2, 4), trans = "log10",
                     oob=scales::rescale_none) +
  geom_vline(xintercept = 1)  +
  geom_rect(aes(xmin = 0.01, xmax = 10, ymin = 0.5, ymax = 1.5),
            fill = "#001427",
            alpha = 0.02,
            color = "#eaf4f4",
            linetype = 0) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    # facet
    strip.text = ggplot2::element_text(face = "bold", size = fontsizeRef-1),
    strip.background = ggplot2::element_rect(fill = "#e5e6e4", colour = color.border),
    strip.switch.pad.grid = unit(10, "cm"),
    # strip.text.y.left = ggplot2::element_text(angle = 0),
    # strip.text.y.right = ggplot2::element_text(angle = 0),
    # # title
    plot.title = ggplot2::element_text(face = "bold", size = fontsizeRef+2),
    # axis
    axis.text.y = ggtext::element_markdown(size = fontsizeRef-1, color = color.axis.text),
    axis.text.x = ggplot2::element_text(size = fontsizeRef-1, color = color.axis.text),
    axis.title.x = ggplot2::element_text(size = fontsizeRef, vjust = 0, color = color.axis.title),
    axis.title.y = ggplot2::element_text(size = fontsizeRef, vjust = 1.25, color = color.axis.title),
    # legend
    legend.text = ggplot2::element_text(size = fontsizeRef-1),
    legend.title = ggplot2::element_text(size = fontsizeRef, face = "bold"),
    # legend.position = legendPosition,
    # background
    panel.background = ggplot2::element_rect(fill = color.background, colour = color.background),
    plot.background = ggplot2::element_rect(fill = color.background, colour = color.background),
    panel.border = ggplot2::element_rect(colour = color.border),
    # grid
    panel.grid.major = ggplot2::element_line(color = color.grid.major, size = .25),
    panel.spacing = unit(1, "lines"),
    # margin
    plot.margin = grid::unit(c(0.35, 0.2, 0.3, 0.35), "cm")
  ) +
  # scale_color_brewer(palette = "Paired")
  scale_color_manual(values = c("#1b263b", "#778da9","#778da9", "#778da9", "#778da9")) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = unname(TeX(c("$i^2 \\leq 0.4$", "$i^2 > 0.4$")))) +
  scale_y_discrete(labels = rev(c("CPRD GOLD", "SCIFI-PEARL", "SIDIAP", "UiO", "**META-ANALYSIS**"))) +
  scale_shape_manual(values = c(15, 16, 17, 18, 8)) +
  xlab("Hazard Ratio")

