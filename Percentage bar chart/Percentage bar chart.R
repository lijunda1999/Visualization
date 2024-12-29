# 加载必要的库
library(ggplot2)
library(dplyr)
library(scales)

# 加载内置的 mpg 数据集（来自 ggplot2 包）
data("mpg")

# 检查数据结构
str(mpg)

# 数据预览
head(mpg)

# 以下是示例代码，直接运行即可

# 双连续变量：散点图 + 二次拟合曲线
mpg %>% 
  ggplot(aes(x = cty, y = hwy)) + # cty: 城市燃油效率, hwy: 高速燃油效率
  geom_point() + # 散点图
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) + # 二次拟合
  theme_bw() # 空白背景

# 双分类变量：计数统计
mpg %>% 
  count(trans, drv) # trans: 变速箱类型, drv: 驱动方式

# 双分类变量：堆叠柱状图
mpg %>% 
  ggplot(aes(x = trans, fill = drv)) + # 按 drv 填充颜色
  geom_bar() + # 堆叠柱状图
  theme_bw()

# 双分类变量：并列柱状图
mpg %>% 
  ggplot(aes(x = trans, fill = drv)) +
  geom_bar(position = position_dodge(width = 1, preserve = "single"),
           width = 0.8) + # 设置宽度
  theme_bw()

# 双分类变量：百分比柱状分布图
mpg %>% 
  ggplot(aes(x = trans, fill = drv)) +
  geom_bar(position = 'fill', width = 0.7) +
  theme_bw()

# 百分比堆叠柱状图（带标签）
mpg %>% 
  group_by(trans, drv) %>% 
  summarise(N = n()) %>% # 按 trans 和 drv 分组计数
  group_by(trans) %>% 
  mutate(Sum = sum(N), # 计算每组总数
         Pct = N / Sum, # 百分比
         Pctp = scales::percent(Pct)) %>% # 转换为百分数
  ungroup() %>% 
  ggplot(aes(x = trans, y = Pct, fill = drv)) +
  geom_col(position = 'fill') + # 百分比堆叠柱状图
  geom_text(aes(label = Pctp),
            position = position_stack(vjust = 0.5)) + # 添加标签
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = scales::percent) + # Y 轴百分比格式
  theme_bw()
