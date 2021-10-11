drsimonj_colors <- c(
  `red`        = "#d11141",
  `green`      = "#00b159",
  `blue`       = "#00aedb",
  `orange`     = "#f37735",
  `yellow`     = "#ffc425",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c")


# kable 
https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
# continuous -> categorical (how to bin efficiently)
https://towardsdatascience.com/stratified-sampling-and-how-to-perform-it-in-r-8b753efde1ef

round(prop.table(table(Pima$type)), 2) %>% kbl(caption = "Diabete Diagnostic Probability Table") %>%
  kable_classic(full_width = F, html_font = "Cambria")

barplot(prop.table(table(Pima$type)), 
        col = Pima$type,
        ylim = c(0,0.7),
        main = "Diabetes proportion barplot")

# 3.0 EDA --- 
diag_positive <- Pima %>%  filter(type == "Yes")
diag_negative <- Pima %>%  filter(type == "No")
