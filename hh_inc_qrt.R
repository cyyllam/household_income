# This script reports household income statistics and income quartiles
# requires 'HINCP' column
# user must set f.dir (where data resides) and option of creating plot

library(stringr)
library(plotly) # version 4.0 and up
options(pandoc.stack.size="2000m")

# read hh income file
f.dir <- "C:/Users/Christy/Desktop/household_income"
files <- list.files(f.dir, pattern = '.csv')

income.table <- NULL
income.summary <- NULL
income.groups <- NULL

plot <- 0 # 1 - yes, 0 - no

for (f in 1:length(files)){
  hh.file <- NULL
  year <- NULL
  inc.summary <- NULL
  hh.file2 <- read.table(file.path(f.dir, paste0(files[f])), header = TRUE, sep = ',')
  file.name <- unlist(strsplit(files[f], "[.]"))[[1]]
  hh.file <- hh.file2[, c("serialno", "HINCP")]
  hh.file <- within(hh.file, HINCP[HINCP < 0] <- 0)
  rm(hh.file2)
  
  # compile frequency by income groups
  breaks <- c(min(hh.file$HINCP), seq(10000, 50000, 5000), 60000, seq(75000, 150000, 25000), 200000, max(hh.file$HINCP)+1)
  
  hh.by.incgroups <- table(cut(hh.file$HINCP, breaks = breaks, right = FALSE)) %>% as.data.frame()
  hh.by.incgroups[, 1] <- c('Less than $10,000',
                            '$10,000 to $14,999',
                            '$15,000 to $19,999',
                            '$20,000 to $24,999',
                            '$25,000 to $29,999',
                            '$30,000 to $34,999',
                            '$35,000 to $39,999',
                            '$40,000 to $44,999',
                            '$45,000 to $49,999',
                            '$50,000 to $59,999',
                            '$60,000 to $74,999',
                            '$75,000 to $99,999',
                            '$100,000 to $124,999',
                            '$125,000 to $149,999',
                            '$150,000 to $199,999',
                            '$200,000 or more')
  colnames(hh.by.incgroups)[1] <- "incgroup"
  colnames(hh.by.incgroups)[2] <- file.name
  ifelse (is.null(income.groups), income.groups <- hh.by.incgroups, income.groups <- cbind(income.groups, hh.by.incgroups[,2]))
  
  # # compile quartile summary
  # inc.summary <- summary(hh.file$HINCP)
  # inc.summary <- data.frame(unclass(summary(hh.file$HINCP)))
  # colnames(inc.summary) <- file.name
  # ifelse (is.null(income.summary), income.summary <- inc.summary, income.summary <- cbind(income.summary, inc.summary))
  # 
  # # build income table
  # year <- str_match(files[f], ".(\\d+).")[,2]
  # hh.file$year <- year
  # ifelse (is.null(income.table), income.table <- hh.file, income.table <- rbind(income.table, hh.file))
  
}

# # export quartile summary to csv
# write.table(income.summary, file.path(f.dir, "income_summary.csv"), col.names = NA, row.names = TRUE, sep = ",")

# export income group summary
write.table(hh.by.incgroups, file.path(f.dir, "income_groups_summary.csv"), col.names = NA, row.names = TRUE, sep = ",")


# box plot
if (plot == 1){
  p <- plot_ly(income.table, 
               y = ~HINCP,
               color = ~year,
               type = "box") %>%
    layout(font = list(family="Segoe UI", size = 13.5))
  
  # export box plot to html
  html.file <- paste0("income_summary_boxplot.html")
  htmlwidgets::saveWidget(p, file.path(f.dir, html.file))
}

