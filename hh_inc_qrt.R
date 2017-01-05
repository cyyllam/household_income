# This script reports household income statistics and income quartiles
# requires 'income' column
# user must set f.dir (where data resides) and file/ext name

library(plotly) # version 4.0 and up

# read hh income file
f.dir <- "C:/Users/Christy/Desktop/household_income"
f <- "hh_2014"
ext <- ".csv"
hh.file <- read.table(file.path(f.dir, paste0(f, ext)), header = TRUE, sep = ',')

# print quartile summary
inc_summary <- summary(hh.file$income)
capture.output(inc_summary, file = file.path(f.dir, paste0(f, "_incsum.txt")))


# plot
p <- plot_ly(hh.file, 
             y = ~income,
             name = '2014',
             type = "box") %>%
  layout(font = list(family="Segoe UI", size = 13.5))

print(p)

html.file <- paste0(f, "_incsum_boxplot.html")
htmlwidgets::saveWidget(p, file.path(f.dir, html.file))
