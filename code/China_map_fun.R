
China_map <- function(province_map, textSize = 3, lowColor = "white", highColor = "red", legendTitle = "value") {
  if (!require("sf")) install.packages("sf")
  if (!require("ggplot2")) install.packages("ggplot2")
  if (!require("ggspatial")) install.packages("ggspatial")
  # if (!require("showtext")) install.packages("showtext")
  showtext::showtext_auto()

  # 数据来源 http://datav.aliyun.com/portal/school/atlas/area_selector
  # 100000 为中国地图，500000 为重庆，510000 为四川......
  adcode <- "100000"

  # 读取数据，取所需内容
  site <- paste0("https://geo.datav.aliyun.com/areas_v3/bound/", adcode, "_full.json")

  df_map <- read_sf(site[1])[, c("adcode", "name", "center", "parent", "geometry")]

  n <- which(is.na(df_map[, 4])) # 坐标缺失列

  # 标签经、纬度坐标
  df_map$jd <- NA
  df_map[-n, "jd"] <- matrix(unlist(df_map$center[-n]), ncol = 2, byrow = T)[, 1]

  df_map$wd <- NA
  df_map[-n, "wd"] <- matrix(unlist(df_map$center[-n]), ncol = 2, byrow = T)[, 2]

  # df_map$value <- 1:nrow(df_map) # 给各省赋值，可自行更改。
  df_map <- merge(df_map, province_map, by = "name", all.x = T)

  df_map[is.na(df_map$value), "value"] <- 0

  # 画图
  p <- ggplot(data = df_map) +
    geom_sf(aes(fill = value)) +
    fixed_plot_aspect(ratio = 1.25) +
    # coord_sf(crs = 4326) +
    annotation_scale(location = "bl") + # 添加比例尺
    # 添加指北针
    annotation_north_arrow(location = "tl", which_north = "false", style = north_arrow_fancy_orienteering) +
    # 数据列，低值、高值颜色
    scale_fill_gradient(name = legendTitle, low = lowColor, high = highColor) +
    # 无背景
    geom_text(data = df_map, aes(x = jd, y = wd, label = labels), position = "identity", size = textSize) +
    theme_void()

  return(p)
}
