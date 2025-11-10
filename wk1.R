library(sf)
library(tidyverse)
library(tmap)
library(readr)
library(RSQLite)

shape <- st_read("C:/Users/Elliot/Desktop/05GIS/week1/3/statsnz-territorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp")

#简化
shape_simple <- st_simplify(shape, dTolerance = 1000)

shape_simple %>%
  st_geometry()%>%
  plot()

#read csv
employed_2018 <- read_csv("C:/Users/Elliot/Desktop/05GIS/week1/homework/homework/edited.csv")

# join on the description 
paid_employee_shape <- shape_simple %>%
  merge(.,
        employed_2018,
        by.x="TA2018_V1_", 
        by.y="Territorial authority description")
#check 
paid_employee_shape%>%
  head(., n=10)

#Join on the ID by making the column numeric

paid_employee_shape_byID <- shape_simple %>%
  mutate(TA2018_V1_=(as.numeric(TA2018_V1_)))%>%
  merge(.,
        employed_2018,
        by.x="TA2018_V1_", 
        by.y="Territorial authority code (2018 areas)")
#plot
tmap_mode("plot")
# change the fill to your column name if different
my_map<-paid_employee_shape %>%
  qtm(.,fill = "Paid employee")
#plot my map
my_map

#my_map 报错纠正
## ===== 方式A：按“描述名称”连接（与你的 shape2 对应）=====
# by.x="TA2018_V1_"  对  by.y="Territorial authority description"
paid_employee_shape <- shape_simple %>%
  dplyr::left_join(
    employed_2018 %>% dplyr::select(`Territorial authority description`, `Paid employee`),
    by = c("TA2018_V1_" = "Territorial authority description")
  )

## ===== 方式B：按“ID代码”连接（与你的 shape3 对应）=====
# 先把两边的键都转成 numeric，但不改列名
paid_employee_shape_byID <- shape_simple %>%
  dplyr::mutate(TA2018_V1_ = suppressWarnings(as.numeric(TA2018_V1_))) %>%
  dplyr::left_join(
    employed_2018 %>%
      dplyr::mutate(`Territorial authority code (2018 areas)` =
                      suppressWarnings(as.numeric(`Territorial authority code (2018 areas)`))) %>%
      dplyr::select(`Territorial authority code (2018 areas)`, `Paid employee`),
    by = c("TA2018_V1_" = "Territorial authority code (2018 areas)")
  )
# 选择要画哪一个（名称连接或ID连接）——推荐用 ID 版本更稳
plot_data <- paid_employee_shape_byID

# 画图
tmap_mode("plot")
my_map <- qtm(plot_data, fill = "Paid employee")
my_map

# write to a .gpkg
paid_employee_shape_byID %>%
  st_write(.,"C:/Users/Elliot/Desktop/05GIS/week1/homework/homework/wk1.gpkg",
           "paid_employee",
           delete_layer=TRUE)
#connect .gpkg
con <- dbConnect(RSQLite::SQLite(),dbname="C:/Users/Elliot/Desktop/05GIS/week1/homework/homework/wk1.gpkg")
# list what is in it
con %>%
  dbListTables()           
#Add your .csv and disconnect from the .gpkg
con %>%
  dbWriteTable(.,
               "original_csv",
               employed_2018,
               overwrite=TRUE)

con %>% 
  dbDisconnect()
