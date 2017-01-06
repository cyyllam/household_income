# This script reports household income statistics and income quartiles
# requires 'income' column
# user must set f.dir (where data resides)

library(stringr)
library(plotly) # version 4.0 and up
options(pandoc.stack.size="2000m")

# read hh income file
f.dir <- "C:/Users/Christy/Desktop/household_income"
files <- list.files(f.dir, pattern = '.csv')

income.table <- NULL

for (f in 1:length(files)){
  hh.file <- NULL
  year <- NULL
  inc_summary <- NULL
  hh.file <- read.table(file.path(f.dir, paste0(files[f])), header = TRUE, sep = ',')
  file.name <- unlist(strsplit(files[f], "[.]"))[[1]]
  
  # print quartile summary
  inc_summary <- summary(hh.file$income)
  capture.output(inc_summary, file = file.path(f.dir, paste0(file.name, "_incsum.txt")))
  
  # build income table
  year <- str_match(files[f], ".(\\d+).")[,2]
  hh.file$year <- year
  ifelse (is.null(income.table), income.table <- hh.file, income.table <- rbind(income.table, hh.file))
}

# plot
p <- plot_ly(income.table, 
             y = ~income,
             color = ~year,
             type = "box") %>%
  layout(font = list(family="Segoe UI", size = 13.5))

html.file <- paste0("income_summary_boxplot.html")
htmlwidgets::saveWidget(p, file.path(f.dir, html.file))
