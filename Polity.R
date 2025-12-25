# 安装加载读取包
install.packages("readxl")
library(readxl)
library(dplyr)

# 读取Polity5的Excel文件
polity_data <- read_excel("C:/data/p5v2018.xls")

# 筛选核心变量
polity_panel <- polity_data %>%
  # 筛选关键变量：
  # - scode：ISO3国家代码 
  # - country：国家名称
  # - year：年份
  # - polity2：修正后政体得分（核心指标）
  select(scode, country, year, polity2) %>%
  # 生成“民主/专制”二分类变量（polity2>6=民主）
  mutate(democracy = ifelse(polity2 > 6, 1, 0)) %>%
  filter(!is.na(polity2))

# 导出为面板表格
write_csv(polity_panel, "C:/data/Polity5_政体面板数据.csv")
