# This script reports household income statistics and income quartiles
# required: 'HINCP' column, filename ends with data year, the only .csv files in f.dir are raw datafiles
# user must set f.dir (where data resides) and option of creating plot

library(stringr)
library(plotly) # version 4.0 and up
library(scales)
options(pandoc.stack.size="2000m")

# 1 - yes, 0 - no
plot <- 0
export.income.summary <- 0
export.income.groups.table <- 1

# read hh income file
f.dir <- "C:/Users/Christy/Desktop/household_income"
files <- list.files(f.dir, pattern = '.csv')

# initialize tables
income.table <- NULL
income.summary <- NULL
income.groups <- NULL

for (f in 1:length(files)){
  hh.file <- NULL
  year <- NULL
  inc.summary <- NULL
  hh.file2 <- read.table(file.path(f.dir, paste0(files[f])), header = TRUE, sep = ',')
  file.name <- unlist(strsplit(files[f], "[.]"))[[1]]
  hh.file <- hh.file2[, c("serialno", "HINCP")]
  hh.file <- within(hh.file, HINCP[HINCP < 0] <- 0)
  rm(hh.file2)
  
  if (export.income.groups.table == 1){
    # compile frequency by income groups
    # breaks <- c(min(hh.file$HINCP), seq(10000, 50000, 5000), 60000, seq(75000, 150000, 25000), 200000, max(hh.file$HINCP)+1)
    breaks <- c(min(hh.file$HINCP), 12297, 18444, 24593, 30741, 36890, 43038, 49187, 55335, 61484, 73781, 92226, 122969, 153711, 184454, 245939, max(hh.file$HINCP)+1)
    hh.by.incgroups <- table(cut(hh.file$HINCP, breaks = breaks, right = FALSE)) %>% as.data.frame()
    
    start.lab <- paste('Less than', dollar(breaks[2]))
    end.lab <- paste(dollar(breaks[length(breaks)-1]), 'or more')
    from.lab <- sapply(breaks[2:(length(breaks)-2)], function(x) dollar(x))
    to.lab <- sapply(breaks[3:(length(breaks)-1)]-1, function(x) dollar(x))
    cat.lab <- function(x, y) paste(x, "to", y)
    inbtwn.labs <- mapply(cat.lab, from.lab, to.lab, USE.NAMES = FALSE, SIMPLIFY = TRUE)
    hh.by.incgroups[, 1] <- c(start.lab, inbtwn.labs, end.lab)
    
    colnames(hh.by.incgroups)[1] <- "incgroup"
    colnames(hh.by.incgroups)[2] <- file.name
    ifelse (is.null(income.groups), income.groups <- hh.by.incgroups, income.groups <- cbind(income.groups, hh.by.incgroups[,2]))
  }
  
  if (export.income.summary == 1){
    # compile quartile summary
    inc.summary <- summary(hh.file$HINCP)
    inc.summary <- data.frame(unclass(summary(hh.file$HINCP)))
    colnames(inc.summary) <- file.name
    ifelse (is.null(income.summary), income.summary <- inc.summary, income.summary <- cbind(income.summary, inc.summary))
  }
  
  if (plot == 1){
    # build income table for box plot
    year <- str_match(files[f], ".(\\d+).")[,2]
    hh.file$year <- year
    ifelse (is.null(income.table), income.table <- hh.file, income.table <- rbind(income.table, hh.file))
  }
} # end files loop

if (export.income.summary == 1){
  # export quartile summary to csv
  write.table(income.summary, file.path(f.dir, "income_summary.csv"), col.names = NA, row.names = TRUE, sep = ",")
}

if (export.income.groups.table == 1){
  # export income group summary
  write.table(income.groups, file.path(f.dir, "income_groups_summary.csv"), col.names = NA, row.names = TRUE, sep = ",")
}

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

