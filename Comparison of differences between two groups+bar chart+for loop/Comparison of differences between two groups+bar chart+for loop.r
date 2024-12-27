library(ggplot2)
library(dplyr)
library(ggsignif)
library(multcompView)
library(openxlsx)

set.seed(123) # ensure that randomness can be repeated

# Example Data Frame: Building Multiple Indicators
ata <- data.frame(
  group = rep(c("H", "L"), each = 10),
  ACE = c(rnorm(10, mean = 5, sd = 1), rnorm(10, mean = 7, sd = 1.5)),
  ALT = c(rnorm(10, mean = 20, sd = 5), rnorm(10, mean = 25, sd = 6)),
  AST = c(rnorm(10, mean = 30, sd = 4), rnorm(10, mean = 35, sd = 5)),
  GLU = c(rnorm(10, mean = 90, sd = 10), rnorm(10, mean = 95, sd = 12))
)

# get the columns to be analyzed
metrics <- setdiff(names(ata), "group")

# initiate the data frame to store the test results
results <- data.frame(
  Metric = character(),
  Group = character(),
  Mean = numeric(),
  SD = numeric(),
  SEM = numeric(),
  Normality_Test_P = numeric(),
  Variance_Test_P = numeric(),
  P_Value = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

# 统一样式设置  unified style settings  
base_theme <- theme_classic(base_line_size = 1) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 13, face = "plain", color = "black", family = "serif"),
    axis.text = element_text(size = 12, face = "plain", color = "black", family = "serif")
  )

# 循环处理每个指标 loop through each metric
for (metric in metrics) {
  # 分组均值、标准差、标准误  group mean, standard deviation, standard error
  summary_stats <- ata %>%
    group_by(group) %>%
    summarize(
      Mean = mean(.data[[metric]]),
      SD = sd(.data[[metric]]),
      SEM = SD / sqrt(n()), 
      .groups = "drop"
    )
  
  # 正态性和方差齐性检验 normality and homogeneity of variance tests
  shapiro_res <- shapiro.test(ata[[metric]])
  bartlett_res <- bartlett.test(ata[[metric]] ~ group, data = ata)
  
  # 差异显著性检验 significance test
  # 若两检验均为显著，则使用 t 检验；否则使用非参数检验 if both tests are significant, use t test; otherwise, use non-parametric test
  if (shapiro_res$p.value > 0.05 && bartlett_res$p.value > 0.05) {
    test_res <- t.test(ata[[metric]] ~ group, data = ata) # t test
  } else {
    test_res <- wilcox.test(ata[[metric]] ~ group, data = ata) # non-parametric test
  }
  p_value <- test_res$p.value
  
  # 差异显著性标记 significance marker
  if (p_value < 0.001) {
    sig <- "***"
  } else if (p_value < 0.01) {
    sig <- "**"
  } else if (p_value < 0.05) {
    sig <- "*"
  } else {
    sig <- "ns"
  }
  
  # 添加结果到数据框 add results to data frame
  for (i in 1:nrow(summary_stats)) {
    results <- rbind(results, data.frame(
      Metric = metric,
      Group = summary_stats$group[i],
      Mean = summary_stats$Mean[i],
      SD = summary_stats$SD[i],
      SEM = summary_stats$SEM[i],
      Normality_Test_P = ifelse(i == 1, shapiro_res$p.value, NA), # 正态性检验 p 值
      Variance_Test_P = ifelse(i == 1, bartlett_res$p.value, NA), # 方差齐性检验 p 值
      P_Value = ifelse(i == 1, p_value, NA), # 差异检验 p 值
      Significance = ifelse(i == 1, sig, NA), # 差异显著性标记
      stringsAsFactors = FALSE
    ))
  }
  
  # 分位数计算 calculate quantiles
  q <- quantile(ata[[metric]])
  max <- q[1] * 0.2 + q[5]
  max1 <- q[1] + q[5]
   
  # 绘图 draw plot
  p <- ggplot(ata, aes(x = group, y = .data[[metric]])) +
    stat_summary(fun = 'mean', geom = 'bar', aes(fill = group), position = 'dodge', size = 1, width = .5, color = 'black') +
    stat_summary(fun.min = mean, fun.max = function(x) mean(x) + sd(x), geom = 'errorbar', aes(group = group), size = 1, color = 'black', width = .2) +
    geom_signif(annotations = sig, y_position = max, xmin = 1, xmax = 2, tip_length = c(0.2, 0.25), vjust = 0, size = 1) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max1)) +
    scale_fill_manual(values = c("#ACC270", "#FEC868")) +
    ylab(metric) +
    base_theme
  
  # 输出图表 output plot
  ggsave(filename = paste0(metric, "_plot.pdf"), plot = p, width = 6, height = 4)
  print(p)
}

# 查看所有检验结果 view all test results
print(results)

# 将 NA 值替换为空字符串 replace NA values with empty strings
results_no_na <- results
results_no_na[is.na(results_no_na)] <- ""

# 保存结果为 Excel 文件 save results to Excel file
output_file <- "results.xlsx"
write.xlsx(results_no_na, file = output_file, sheetName = "Results", rowNames = FALSE)

cat("Results have been saved to", output_file, "\n")
