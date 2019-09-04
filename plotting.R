library(ggplot2)
library(dplyr)

svgWidth <- 12
svgHeight <- svgWidth / 2

if (exists('import')) {
  statsObject <- import('stats')
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
  
  bench
}

toSVG <- function (plot) {
  svg(width = svgWidth, height = svgHeight)
  print(plot)
  return(svg.off())
}

benchmark_plot <- function (file1, benchName, startIteration) {
  data <- load_data_file(file1)
  data <- subset(data, Benchmark == benchName)
  data <- data %>% filter(iteration > startIteration)
  if (exists('statsObject')) {
    statsObject$run1$max <- max(data$Value)
    statsObject$run1$min <- min(data$Value)
    statsObject$run1$mean <- mean(data$Value)
    statsObject$run1$median <- median(data$Value)
    statsObject$run1$geomean <- exp(mean(log(data$Value)))
  }
  
  plot <- ggplot(data, aes(x=iteration, y=Value, group=Benchmark)) + geom_point(size=1)
  #plot <- plot + geom_point(aes(color=Benchmark))
  plot <- plot + xlab('') + ylab(data$Unit)
  plot <- plot + scale_x_continuous(limits = c(startIteration, NA))
  #plot <- plot + geom_smooth()
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$iteration), 100))
  plot <- plot + geom_vline(xintercept = c(50, 100, 150, 200, 250), linetype = "longdash", colour = "#cccccc")
  return(toSVG(plot))
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
    statsObject$run1$geomean <- exp(mean(log(data1$Value)))
    statsObject$run2$max <- max(data2$Value)
    statsObject$run2$min <- min(data2$Value)
    statsObject$run2$mean <- mean(data2$Value)
    statsObject$run2$median <- median(data2$Value)
    statsObject$run2$geomean <- exp(mean(log(data2$Value)))
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
  return(toSVG(plot))
}

summary_plot <- function (file, startIteration) {
  data <- load_data_file(file)
  data <- data %>% group_by(Benchmark) %>% filter(iteration > startIteration)
  plot <- ggplot(data, aes(x=Benchmark, y=Value))
  plot <- plot + geom_boxplot(lwd = 0.4, show.legend = FALSE, outlier.size=0.5, outlier.colour = "#ff0000")
  plot <- plot + xlab('') + ylab(data$Unit)
  plot <- plot + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$iteration), 100))
  return(toSVG(plot))
}

summary_diff_plot <- function (commit1, file1, commit2, file2, startIteration) {
  data1 <- load_data_file(file1)
  data2 <- load_data_file(file2)
  data1 <- data1 %>% group_by(Benchmark) %>% filter(iteration > startIteration)
  data2 <- data2 %>% group_by(Benchmark) %>% filter(iteration > startIteration)
  data1$commit <- commit1
  data2$commit <- commit2
  data <- rbind(data1, data2)
  # print(data)
  plot <- ggplot(data, aes(x=commit, y=Value, group=commit)) + facet_wrap(~Benchmark, scale="free")
  plot <- plot + geom_boxplot(lwd = 0.4, show.legend = FALSE, outlier.size=0.5, outlier.colour = "#ff0000")
  plot <- plot + xlab('') + ylab(data$Unit)
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$iteration), 100))
  return(toSVG(plot))
}
