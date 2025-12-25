
# 加载包、读取数据
install.packages(c("dplyr", "stringr"))
library(dplyr)
library(stringr)

# 读取GEDEvent数据
ged_data <- readRDS("C:/data/GEDEvent_v25_1.rds")

# 筛选【国内武装冲突】
# type_of_violence == 1：UCDP标准“国内武装冲突”

domestic_armed_conflict <- ged_data %>%
  # 筛选“国内武装冲突”（UCDP暴力类型1）
  filter(type_of_violence == 1) %>%
  # 保留核心列
  select(country, country_id, year, conflict_new_id, conflict_name)


# 按【国家-年份】计算年度国内冲突事件数
# ==============================================
annual_domestic_conflict <- domestic_armed_conflict %>%
  group_by(country, country_id, year) %>%
  summarise(
    # 年度国内武装冲突事件数
    domestic_conflict_count = n_distinct(conflict_new_id),
    .groups = "drop"
  )


# 标记【宗教相关冲突】（从conflict_name中提取）
# ==============================================
conflict_with_religion <- domestic_armed_conflict %>%
  # 从冲突名称中识别宗教相关关键词（如Islam、Christian等）
  mutate(
    is_religious_conflict = ifelse(
      str_detect(conflict_name, regex("Islam|Christian|Relig|Muslim|Catholic", ignore_case = TRUE)),
      1, 0
    )
  ) %>%
  # 按国家-年份聚合宗教冲突数
  group_by(country, country_id, year) %>%
  summarise(
    religious_conflict_count = sum(is_religious_conflict),
    .groups = "drop"
  )


# 合并结果→最终面板数据
final_ged_panel <- annual_domestic_conflict %>%
  left_join(conflict_with_religion, by = c("country", "country_id", "year")) %>%
  mutate(religious_conflict_count = replace_na(religious_conflict_count, 0))


# 导出结果
write_csv(final_ged_panel, "C:/data/UCDP_国内武装冲突_面板数据.csv")
