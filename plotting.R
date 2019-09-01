library(ggplot2)
library(dplyr)

if (exists('import')) {
  statsObject <- import('stats')
}


load_data_file <- function (file) {
  # [2019-08-25T10:16:55] 5757.000000 ms  total Permute GraalSqueak-EE  graalsqueak 1000  0 1   250
  row_names <- c("Timestamp", "Value", "Unit", "Total", "Benchmark",
                 "VM", "Experiment", "ProblemSize", "Extra", "Cores", "NA",
                 "Iterations")
  
  bench <- read.table(file, sep="\t", header=FALSE, col.names=row_names, fill=TRUE)
  bench$rid <- as.numeric(row.names(bench)) %% bench$Iterations + 1
  bench
}

example_plot <- function (contents) {
  svg()
  plot <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()
  print(plot)
  return(svg.off())
}

single_plot <- function (file1, benchName, startIteration) {
  data <- load_data_file(file1)
  data <- subset(data, Benchmark == benchName)
  if (startIteration > 0) {
    data <- tail(data, -startIteration)
  }
  statsObject$run1$max <- max(data$Value)
  statsObject$run1$min <- min(data$Value)
  statsObject$run1$mean <- mean(data$Value)
  
  # print(data)
  
  svg(width = 8, height = 4)
  plot <- ggplot(data, aes(x=rid, y=Value, group=Benchmark)) + geom_point(size=1)
  #plot <- plot + geom_point(aes(color=Benchmark))
  plot <- plot + xlab('') + ylab(data$Unit)
  plot <- plot + scale_x_continuous(limits = c(startIteration, NA))
  #plot <- plot + geom_smooth()
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$rid), 100))
  plot <- plot + geom_vline(xintercept = c(50, 100, 150, 200, 250), linetype = "longdash", colour = "#cccccc")
  print(plot)
  return(svg.off())
}

diff_plot <- function (file1, file2, benchName, startIteration) {
  data1 <- load_data_file(file1)
  data1 <- subset(data1, Benchmark == benchName)
  data2 <- load_data_file(file2)
  data2 <- subset(data2, Benchmark == benchName)
  if (startIteration > 0) {
    data1 <- tail(data1, -startIteration)
    data2 <- tail(data2, -startIteration)
  }
  if (exists('statsObject')) {
    statsObject$run1$max <- max(data1$Value)
    statsObject$run1$min <- min(data1$Value)
    statsObject$run1$mean <- mean(data1$Value)
    statsObject$run2$max <- max(data2$Value)
    statsObject$run2$min <- min(data2$Value)
    statsObject$run2$mean <- mean(data2$Value)
  }
  # print(data1)
  
  svg(width = 8, height = 4)
  plot <- ggplot() + geom_point(data=data1, aes(x=rid, y=Value), shape=16, size=0.5)
  plot <- plot + geom_point(data=data2, aes(x=rid, y=Value), shape=17, size=0.5, color='red')
  plot <- plot + geom_point(size=1)
  #plot <- plot + geom_point(aes(color=Benchmark))
  plot <- plot + xlab('') + ylab(data1$Unit)
  plot <- plot + scale_x_continuous(limits = c(startIteration, NA))
  plot <- plot + geom_smooth(method = 'loess')
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$rid), 100))
  plot <- plot + geom_vline(xintercept = c(50, 100, 150, 200, 250), linetype = "longdash", colour = "#cccccc")
  print(plot)
  return(svg.off())
}

summary_plot <- function (file, startIteration) {
  data <- load_data_file(file)
  data <- data %>% group_by(Benchmark) %>% filter(rid > startIteration)
  #data <- tail(data, -100)
  svg(width = 8, height = 4)
  plot <- ggplot(data, aes(x=Benchmark, y=Value))
  plot <- plot + geom_boxplot(lwd = 0.4, show.legend = FALSE, outlier.size=0.5, outlier.colour = "#ff0000")
  plot <- plot + xlab('') + ylab(data$Unit)
  plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$rid), 100))
  print(plot)
  return(svg.off())
}

diff_summary_plot <- function (commit1, file1, commit2, file2, startIteration) {
  data1 <- load_data_file(file1)
  data2 <- load_data_file(file2)
  data1 <- data1 %>% group_by(Benchmark) %>% filter(rid > startIteration)
  data2 <- data2 %>% group_by(Benchmark) %>% filter(rid > startIteration)
  data1$commit <- commit1
  data2$commit <- commit2
  data <- rbind(data1, data2)
  # print(data)
  #data <- tail(data, -100)
  svg(width = 8, height = 4)
  plot <- ggplot(data, aes(x=commit, y=Value, group=commit)) + facet_wrap(~Benchmark, scale="free")
  plot <- plot + geom_boxplot(lwd = 0.4, show.legend = FALSE, outlier.size=0.5, outlier.colour = "#ff0000")
  plot <- plot + xlab('') + ylab(data$Unit)
  #plot <- plot + scale_x_continuous(breaks = seq(0, max(data$rid), 100))
  print(plot)
  return(svg.off())
}
