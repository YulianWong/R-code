#安装并加载所有必需包
install.packages(c("readxl", "readr", "tidyr", "dplyr"))
library(readxl)   
library(readr)    
library(tidyr)    
library(dplyr)    

# 指定文件路径 
# WGI Excel文件路径
wgi_path <- "C:/data/wgidataset.xlsx"
# WDI CSV文件路径
wdi_path <- "C:/data/WDICSV.csv"
# 最终合并面板数据导出路径
output_path <- "C:/data/WGI+WDI_10指标_全国家面板数据.csv"

# 处理WGI Excel数据（8个治理指标）
wgi_data <- read_excel(wgi_path) %>%
  
  rename(
    Country_Code = code,       
    Country_Name = countryname,  
    Year = year,               
    Indicator_Code = indicator 
  ) %>%
# 保留核心列 
  select(Country_Code, Country_Name, Year, Indicator_Code, estimate, pctrank) %>%
  
# 筛选目标治理指标（cc/ge/rl/rq）
  filter(Indicator_Code %in% c("cc", "ge", "rl", "rq")) %>%
# 确保年份为数值型 
  mutate(Year = as.numeric(Year)) %>%
# 把estimate/pctrank拆分为行
  pivot_longer(
    cols = c(estimate, pctrank),  
    names_to = "Value_Type",      
    values_to = "Value"           
  ) %>%
# 映射为最终的变量名
  mutate(
    Variable = case_when(
      Indicator_Code == "cc" & Value_Type == "estimate" ~ "corruption_estimate",
      Indicator_Code == "cc" & Value_Type == "pctrank" ~ "corruption_percentile",
      Indicator_Code == "ge" & Value_Type == "estimate" ~ "gov_effectiveness_estimate",
      Indicator_Code == "ge" & Value_Type == "pctrank" ~ "gov_effectiveness_percentile",
      Indicator_Code == "rl" & Value_Type == "estimate" ~ "rule_of_law_estimate",
      Indicator_Code == "rl" & Value_Type == "pctrank" ~ "rule_of_law_percentile",
      Indicator_Code == "rq" & Value_Type == "estimate" ~ "regulatory_quality_estimate",
      Indicator_Code == "rq" & Value_Type == "pctrank" ~ "regulatory_quality_percentile"
    )
  ) %>%
# 转成WGI面板格式
  pivot_wider(
    id_cols = c(Country_Code, Country_Name, Year),  
    names_from = Variable,                          
    values_from = Value                             
  ) %>%
# 按国家+年份排序
  arrange(Country_Code, Year)


# 处理WDI CSV数据（2个GDP指标）
wdi_data <- read_csv(wdi_path, show_col_types = FALSE) %>%
  # 1. 重命名带空格的列
  rename(
    Country_Code = `Country Code`,
    Country_Name = `Country Name`,
    Indicator_Name = `Indicator Name`
  ) %>%
  # 2. 筛选目标GDP指标
  filter(
    Indicator_Name %in% c(
      "GDP growth (annual %)",                  
      "GDP per capita (constant 2015 US$)"      
    )
  ) %>%
  # 3. 年份列转成行
  pivot_longer(
    cols = starts_with(c("19", "20")),  
    names_to = "Year",                  
    values_to = "Value",                
    values_drop_na = TRUE               
  ) %>%
  # 4. 年份转数值型
  mutate(Year = as.numeric(Year)) %>%
  # 5. 映射为简化变量名
  mutate(
    Variable = case_when(
      Indicator_Name == "GDP growth (annual %)" ~ "gdp_growth_rate",
      Indicator_Name == "GDP per capita (constant 2015 US$)" ~ "gdp_per_capita_constant_2015_usd"
    )
  ) %>%
  # 6. 转成WDI面板格式
  pivot_wider(
    id_cols = c(Country_Code, Country_Name, Year),
    names_from = Variable,
    values_from = Value
  ) %>%
  # 7. 按国家+年份排序
  arrange(Country_Code, Year)


# 合并WGI + WDI面板数据
final_panel <- wgi_data %>%
  # 保留所有国家/年份
  full_join(wdi_data, by = c("Country_Code", "Country_Name", "Year")) %>%
  # 排序：国家代码→年份
  arrange(Country_Code, Year)

# 导出最终面板数据（CSV格式）

write_csv(
  final_panel,
  output_path,
  na = ""  # 缺失值用空字符表示（如需保留NA可改为NA）
)
