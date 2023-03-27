rm(list=ls())

installed <- rownames(installed.packages())

# Install packages
for(pkg in c("knitr", "magrittr", "insight", "bayestestR", "parameters", "effectsize", "performance", "correlation", "modelbased", "see", "remotes", "ggplot2",
             "asreml",
             'metan',
             'asremlPlus',
             'DT',  
             'ggplot2',
             'dplyr',
             'data.table',
             'plyr',
             'raster',
             'maps',
             'mapdata',
             'tigris',
             'rcartocolor',
             'cowplot',
             'sf',
             'statgenGxE', 
             'nadiv',
             'broom',
             'spData',
             'ggstatsplot',
             'kableExtra',
             'cowplot',
             'magrittr',
             'knitr',
             'flextable',
             'ggcorrplot',
             'naniar',
             'ggforce',
             'ggpmisc'
             )){
  if (!pkg %in% installed){
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
}
# Because report and easystats are not on CRAN
if (!"report" %in% installed) remotes::install_github("easystats/report")
# if (!"easystats" %in% installed) remotes::install_github("easystats/easystats")

# Load packages silently
for(pkg in c("tidyverse", "ggplot2", "easystats")){
  if (pkg %in% installed && !require(pkg, character.only=TRUE)) {
    suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only=TRUE)))
  }
}

# Install Python
# py_install(packages = c('numpy', 'pandas', 'scipy', 'seaborn', 'matplotlib'))


# Options relative to figure size
figheight <- 6
figwidth <- 6 * see::golden_ratio()

# General options
options(knitr.kable.NA = "NA",
        digits = 4,
        tidyverse.quiet = TRUE,
        kableExtra.latex.load_packages = FALSE)

# Chunk options (see https://yihui.org/knitr/options/#chunk_options)
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE, #echo = F, 
  comment = "#>",  # The prefix to be added before each line of the text output.
  dpi = 600,
  fig.path = "figures/",
  fig.height=figheight,
  fig.width=figwidth,
  fig.align = "center",
  ft_max_row = 9, tab.topcaption=FALSE, ft.align="center"
  
)

options(knitr.table.format = function() {
  if (knitr::is_latex_output()) 'latex' else 'pandoc'
})


# Function to make HTML tables
print_table <- function(table, rownames = FALSE, digits = 4, ...){
  df <- datatable(table, rownames = rownames, extensions = 'Buttons',
                  options = list(scrollX = TRUE, 
                                 dom = '<<t>Bp>',
                                 buttons = c('copy', 'excel', 'pdf', 'print')), ...)
  num_cols <- c(as.numeric(which(sapply(table, class) == "numeric")))
  if(length(num_cols) > 0){
    formatSignif(df, columns = num_cols, digits = digits)
  } else{
    df
  }
}

library(flextable)
use_df_printer()
set_flextable_defaults(
  font.size = 9, 
  padding.bottom = 3, 
  padding.top = 3,
  padding.left = 3,
  padding.right = 3,
  
  post_process_html = function(x){
    theme_vanilla(x) %>% 
      set_table_properties(layout = "autofit", width = .5) %>% 
      autofit()
  },
  post_process_pdf = function(x){
    theme_vanilla(x) %>% 
      set_table_properties(layout = "fixed", width = .5) %>% 
      autofit()
  },
  post_process_docx = function(x){
    theme_vanilla(x) %>% 
      set_table_properties(layout = "fixed", width = .5) %>% 
      autofit()
  }
)

theme_design <- function(x) {
  x <- border_remove(x)
  std_border <- fp_border_default(width = 2, color = "white")
  x <- fontsize(x, size = 10, part = "all")
  #x <- font(x, fontname = "Courier", part = "all")
  x <- align(x, align = "center", part = "body")
  x <- align(x, align = "center", part = "header")
  x <- align(x, align = "left", part = "footer")
  x <- bold(x, bold = TRUE, part = "header")
  x <- bg(x, bg = "#dce1e6", part = "body")
  x <- bg(x, bg = "#edf2f7", part = "header")
  x <- bg(x, bg = "#edeff0", part = "footer")
  x <- color(x, color = "black", part = "all")
  x <- padding(x, padding = 6, part = "all")
  x <- border_outer(x, part="all", border = std_border )
  x <- border_inner_h(x, border = std_border, part="all")
  x <- border_inner_v(x, border = std_border, part="all")
 # x <- set_table_properties(x, layout = "fixed")
  x
}


theme_design2 <- function(x) {
  x <- fontsize(x, size = 10, part = "header")
  x <- fontsize(x, size = 9, part = "body")
  x <- fontsize(x, size = 8, part = "footer")
  x <- align(x, align = "justify", part = "body")
  x <- align(x, align = "justify", part = "header")
  x <- align(x, align = "left", part = "footer")
  x <- bold(x, bold = TRUE, part = "header")
  x
}

#ggplot(corr.m, aes(x = reorder(miRNA, -value), y = value, fill = variable))

plot_selected <- function(data_plot, x, y, mean_sel){

  p1 <-
    ggplot(data_plot, aes({{x}}, {{y}}, fill = SELECTED))+
    stat_summary(fun = mean,
                 geom = "bar",
                 na.rm = TRUE,
                 color = "black",
                 size = 0.1,
                 width = 1)+
    stat_summary(fun.data = mean_se,
                 geom = "errorbar",
                 na.rm = TRUE,
                 color = "black",
                 size = 0.1,
                 width = .5)+
    theme_bw() +
    scale_y_continuous(expand = expansion(c(0, 0.05)))+
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 8, colour = "black"),
          axis.text.y = element_text(size = 8, colour = "black"),
          legend.position = "", #right
          axis.text.x = element_text(angle = 90)) +
    geom_hline(aes(yintercept = mean({{y}}, na.rm = TRUE)), linetype = 1)+
    geom_hline(aes(yintercept = mean_sel), linetype = 2) +
    labs(x = "Genotype",
         y = enquo(y)) +
    coord_flip() +
    scale_fill_manual(values=c("#CC6666", "#66CC99"), 
                      labels = c("NO", "YES"))
}


# Set seed for reproducible results
set.seed(777)
