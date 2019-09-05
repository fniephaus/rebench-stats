library(ggplot2)
library(dplyr)

svgWidth <- 12
svgHeight <- svgWidth / 2

if (exists('import')) {
  statsObject <- import('stats')
}


load_all_data <- function (folder, files) {
  result <- NULL
  for (file in files) {
    data <- load_data_file(paste(folder, file, sep="/"))
    result <- rbind(result, data)
  }
  result
}

load_data_file <- function (file) {
  # [2019-08-25T10:16:55] 5757.000000 ms  total Permute GraalSqueak-EE  graalsqueak 1000  0 1   250
  row_names <- c("Timestamp", "Value", "Unit", "Criterion",
                 "Benchmark", "VM", "Suite", "Extra", "Warmup", "Cores", "InputSize",
                 "Var")
  
  bench <- read.table(file, sep="\t", header=FALSE, col.names=row_names, fill=TRUE)
  
  # Give Run Ids to the rows, but need to consider different criterions
  num_criteria <- length(levels(bench$Criterion))
  if (num_criteria < 1) {
    num_criteria = 1
  }
  run_nums <- seq_len(nrow(bench) / num_criteria)
  bench$rid <- rep(run_nums, each = num_criteria)
  
  bench$iteration <- as.numeric(row.names(bench)) %% bench$Var + 1
  
  bench$Extra <- factor(bench$Extra)
  bench$Cores <- factor(bench$Cores)
  bench$Suite <- factor(bench$Suite)
  
  fileInfo <- strsplit(basename(file), "-")
  date <- paste(fileInfo[[1]][1:3], collapse="-")
  commit <- fileInfo[[1]][4]
  commitShort <- substr(commit, 0, 8)
  bench <- mutate(bench, Date = date, Commit = commit, CommitShort = commitShort)
  bench
}

geomean <- function (x) {
  exp(mean(log(x)))
}

toSVG <- function (plot) {
  svg(width = svgWidth, height = svgHeight)
  print(plot)
  svg.off()
}

benchmark_plot <- function (file, benchName, startIteration) {
  data <- load_data_file(file)
  data <- subset(data, Benchmark == benchName)
  data <- data %>% filter(iteration > startIteration)
  if (exists('statsObject')) {
    statsObject$run1$max <- max(data$Value)
    statsObject$run1$min <- min(data$Value)
    statsObject$run1$mean <- mean(data$Value)
    statsObject$run1$median <- median(data$Value)
    statsObject$run1$geomean <- geomean(data$Value)
    statsObject$run1$sum <- sum(data$Value)
  }
  
  plot <- ggplot(data, aes(x=iteration, y=Value, group=Benchmark)) + geom_point(size=1)
  #plot <- plot + geom_point(aes(color=Benchmark))
  plot <- plot + xlab('') + ylab(data$Unit)
  plot <- plot + scale_x_continuous(limits = c(startIteration, NA))
  #plot <- plot + geom_smooth()
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$iteration), 100))
  plot <- plot + geom_vline(xintercept = c(50, 100, 150, 200, 250), linetype = "longdash", colour = "#cccccc")
  toSVG(plot)
}

benchmark_diff_plot <- function (file1, file2, benchName, startIteration) {
  data1 <- load_data_file(file1)
  data1 <- subset(data1, Benchmark == benchName)
  data1 <- data1 %>% filter(iteration > startIteration)
  data2 <- load_data_file(file2)
  data2 <- subset(data2, Benchmark == benchName)
  data2 <- data2 %>% filter(iteration > startIteration)
  if (exists('statsObject')) {
    statsObject$run1$max <- max(data1$Value)
    statsObject$run1$min <- min(data1$Value)
    statsObject$run1$mean <- mean(data1$Value)
    statsObject$run1$median <- median(data1$Value)
    statsObject$run1$geomean <- geomean(data1$Value)
    statsObject$run1$sum <- sum(data1$Value)
    statsObject$run2$max <- max(data2$Value)
    statsObject$run2$min <- min(data2$Value)
    statsObject$run2$mean <- mean(data2$Value)
    statsObject$run2$median <- median(data2$Value)
    statsObject$run2$geomean <- geomean(data2$Value)
    statsObject$run2$sum <- sum(data2$Value)
  }
  # print(data1)
  
  plot <- ggplot() + geom_point(data=data1, aes(x=iteration, y=Value), shape=16, size=0.5)
  plot <- plot + geom_point(data=data2, aes(x=iteration, y=Value), shape=17, size=0.5, color='red')
  plot <- plot + geom_point(size=1)
  #plot <- plot + geom_point(aes(color=Benchmark))
  plot <- plot + xlab('') + ylab(data1$Unit)
  plot <- plot + scale_x_continuous(limits = c(startIteration, NA))
  plot <- plot + geom_smooth(method = 'loess')
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$iteration), 100))
  plot <- plot + geom_vline(xintercept = c(50, 100, 150, 200, 250), linetype = "longdash", colour = "#cccccc")
  toSVG(plot)
}

summary_plot <- function (file, startIteration) {
  data <- load_data_file(file)
  data <- data %>% group_by(Benchmark) %>% filter(iteration > startIteration)
  plot <- ggplot(data, aes(x=Benchmark, y=Value))
  plot <- plot + geom_boxplot(lwd = 0.4, show.legend = FALSE, outlier.size=0.5, outlier.colour = "#ff0000")
  plot <- plot + xlab('') + ylab(data$Unit)
  plot <- plot + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$iteration), 100))
  toSVG(plot)
}

summary_diff_plot <- function (file1, file2, startIteration) {
  data1 <- load_data_file(file1)
  data2 <- load_data_file(file2)
  data1 <- data1 %>% group_by(Benchmark) %>% filter(iteration > startIteration)
  data2 <- data2 %>% group_by(Benchmark) %>% filter(iteration > startIteration)
  data <- rbind(data1, data2)
  if (exists('statsObject')) {
    statsObject$run1$min <- sum(aggregate(data1$Value, list(data1$Benchmark), min)$x)
    statsObject$run1$max <- sum(aggregate(data1$Value, list(data1$Benchmark), max)$x)
    statsObject$run1$mean <- sum(aggregate(data1$Value, list(data1$Benchmark), mean)$x)
    statsObject$run1$median <- sum(aggregate(data1$Value, list(data1$Benchmark), median)$x)
    statsObject$run1$geomean <- sum(aggregate(data1$Value, list(data1$Benchmark), geomean)$x)
    statsObject$run1$sum <- sum(aggregate(data1$Value, list(data1$Benchmark), sum)$x)
    statsObject$run2$min <- sum(aggregate(data2$Value, list(data2$Benchmark), min)$x)
    statsObject$run2$max <- sum(aggregate(data2$Value, list(data2$Benchmark), max)$x)
    statsObject$run2$mean <- sum(aggregate(data2$Value, list(data2$Benchmark), mean)$x)
    statsObject$run2$median <- sum(aggregate(data2$Value, list(data2$Benchmark), median)$x)
    statsObject$run2$geomean <- sum(aggregate(data2$Value, list(data2$Benchmark), geomean)$x)
    statsObject$run2$sum <- sum(aggregate(data2$Value, list(data2$Benchmark), sum)$x)
  }
  # print(data)
  plot <- ggplot(data, aes(x=CommitShort, y=Value, group=Date)) + facet_wrap(~Benchmark, scale="free")
  plot <- plot + geom_boxplot(lwd = 0.4, show.legend = FALSE, outlier.size=0.5, outlier.colour = "#ff0000")
  plot <- plot + xlab('') + ylab(data$Unit)
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$iteration), 100))
  toSVG(plot)
}

benchmark_branch_plot <- function(folder, files, benchmark, startIteration) {
  data <- load_all_data(folder, files)
  data <- data %>% filter(Benchmark == benchmark) %>% filter(iteration > startIteration)
  #print(data)
  plot <- ggplot(data, aes(x=iteration, y=Value, group=Date, color=Date))
  plot <- plot + geom_line(size=0.4)
  plot <- plot + xlab('') + ylab(data$Unit)
  plot <- plot + scale_color_grey(start=0.8, end=0)
  toSVG(plot)
}

summary_branch_plot <- function(folder, files, startIteration) {
  data <- load_all_data(folder, files)
  data <- data %>% group_by(Benchmark) %>% filter(iteration > startIteration)
  #print(data)
  plot <- ggplot(data, aes(x=CommitShort, y=Value, group=Date)) + facet_wrap(~Benchmark, scale="free")
  plot <- plot + geom_boxplot(lwd = 0.4, show.legend = FALSE, outlier.size=0.5, outlier.colour = "#ff0000")
  plot <- plot + xlab('') + ylab(data$Unit)
  toSVG(plot)
}
