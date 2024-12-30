library(tidyverse)
library(ggpubr)

# 检查数据
head(iris)
unique(iris$Species)

# 动态计算 t 检验结果
# 比较 'setosa' vs 'versicolor'
test1 <- t.test(Sepal.Length ~ Species, data = iris[iris$Species %in% c('setosa', 'versicolor'),])
# 比较 'versicolor' vs 'virginica'
test2 <- t.test(Sepal.Length ~ Species, data = iris[iris$Species %in% c('versicolor', 'virginica'),])
# 比较 'setosa' vs 'virginica'
test3 <- t.test(Sepal.Length ~ Species, data = iris[iris$Species %in% c('setosa', 'virginica'),])

# 创建显著性标注数据框
testresult <- data.frame(
  xx = c(1.5, 2.5, 2),  # 两组之间的 x 坐标
  yy = c(7.5, 8.5, 9.5), # y 坐标（标注线的高度）
  zz = c(
    paste0("t = ", round(test1$statistic, 2), ", p = ", signif(test1$p.value, 2)),
    paste0("t = ", round(test2$statistic, 2), ", p = ", signif(test2$p.value, 2)),
    paste0("t = ", round(test3$statistic, 2), ", p = ", signif(test3$p.value, 2))
  )
)

# 可视化
iris %>% 
  mutate(id = rep(1:50, times = 3)) %>% # 添加编号
  ggplot(aes(x = Species, y = Sepal.Length)) + 
  geom_boxplot(width = 0.1) + # 箱线图
  geom_violin(aes(fill = Species), show.legend = F) + # 小提琴图
  geom_point() + # 散点图
  geom_line(aes(group = id), color = 'gold') +#对应线条
  geom_text(data = testresult, aes(x = xx, y = yy, label = zz), inherit.aes = FALSE) + # 显著性标注
  annotate('segment', x = 1, xend = 2, y = 7.5, yend = 7.5, color = 'blue',
           arrow = arrow(angle = 90, ends = 'both', length = unit(0.05, 'inches'))) +
  annotate('segment', x = 2, xend = 3, y = 8.5, yend = 8.5, color = 'blue',
           arrow = arrow(angle = 90, ends = 'both', length = unit(0.05, 'inches'))) +
  annotate('segment', x = 1, xend = 3, y = 9.5, yend = 9.5, color = 'blue',
           arrow = arrow(angle = 90, ends = 'both', length = unit(0.05, 'inches'))) +
  theme_bw()

