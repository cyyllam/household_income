# This script reports household income statistics and income quartiles
# requires 'income' column
# user must set f.dir (where data resides) and option of creating plot

library(stringr)
library(plotly) # version 4.0 and up
options(pandoc.stack.size="2000m")

# read hh income file
f.dir <- "C:/Users/Christy/Desktop/household_income"
files <- list.files(f.dir, pattern = '.csv')

income.table <- NULL
income.summary <- NULL
plot <- 1 # 1 - yes, 0 - no

for (f in 1:length(files)){
  hh.file <- NULL
  year <- NULL
  inc.summary <- NULL
  hh.file <- read.table(file.path(f.dir, paste0(files[f])), header = TRUE, sep = ',')
  file.name <- unlist(strsplit(files[f], "[.]"))[[1]]
  
  # compile quartile summary
  inc.summary <- summary(hh.file$income)
  inc.summary <- data.frame(unclass(summary(hh.file$income)))
  colnames(inc.summary) <- file.name
  ifelse (is.null(income.summary), income.summary <- inc.summary, income.summary <- cbind(income.summary, inc.summary))
  
  # build income table
  year <- str_match(files[f], ".(\\d+).")[,2]
  hh.file$year <- year
  ifelse (is.null(income.table), income.table <- hh.file, income.table <- rbind(income.table, hh.file))
}

# export quartile summary to csv
write.table(income.summary, file.path(f.dir, "income_summary.csv"), col.names = NA, row.names = TRUE, sep = ",")

# box plot
if (plot == 1){
  p <- plot_ly(income.table, 
               y = ~income,
               color = ~year,
               type = "box") %>%
    layout(font = list(family="Segoe UI", size = 13.5))
  
  # export box plot to html
  html.file <- paste0("income_summary_boxplot.html")
  htmlwidgets::saveWidget(p, file.path(f.dir, html.file))
}

