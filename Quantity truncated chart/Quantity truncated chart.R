library(ggplot2)
library(openxlsx)
library(ggpubr)

# 加载数据
midu2 <- read.xlsx('核苷酸read2-散点图.xlsx', sheet = 1)
midu <- read.xlsx('核苷酸read-散点图.xlsx', sheet = 1)

# 基础主题
base_theme <- theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks.y = element_line(color = "black", size = 1),
  axis.ticks.x = element_line(color = "black", size = 1),
  axis.line = element_line(colour = "black", size = 1, lineend = "square"),
  axis.text.y = element_text(
    family = "serif", face = "bold", colour = "black", size = 10
  ),
  axis.text.x = element_text(
    family = "serif", face = "bold", colour = "black", size = 10
  )
)

# 图1
p1 <- ggplot(midu2, aes(x = RefID, y = read, colour = type)) +
  geom_point(size = 1) +
  scale_colour_brewer(palette = 'Set1') +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_cartesian(ylim = c(0, 2000), xlim = c(0, 12000)) +
  base_theme

# 图2
p2 <- ggplot(midu, aes(x = RefID, y = read)) +
  geom_point(colour = "#e41a1c", size = 1) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_cartesian(ylim = c(2000, 5000), xlim = c(0, 12000)) +
  base_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# 图3
p3 <- ggplot(midu, aes(x = RefID, y = read)) +
  geom_point(colour = "#e41a1c", size = 1) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_cartesian(ylim = c(5000, 30000), xlim = c(0, 12000)) +
  base_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



# 合并图表
p <- ggarrange(
  p3, p2, p1,
  heights = c(1 / 3, 1 / 3, 1 / 3),
  ncol = 1, nrow = 3,
  common.legend = TRUE,
  legend = "right",
  align = "v"
)

# 显示图表
print(p)

# 保存图表
ggsave('plot1.svg', plot = p, width = 15, height = 8, units = 'cm')
