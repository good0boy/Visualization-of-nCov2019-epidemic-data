# Visualization-of-nCov2019-epidemic-data
项目目的是通过web交互应用程序搭建一个关于nCov2019疫情数据即时可视化面板。数据来源是基于南方医科大学余光创教授在GitHub上写的nCov2019包。通过R中的shiny app编写该交互应用程序，并最终达到比较好的可视化效果及预测效果和相关的政策调整判断。<br>


1、代码运行需要通过source('citys.R')语句加载city.R
所以city.R这个文件在运行时需要保存在工作路径

2、map_d <- read.csv("C:/Users/一叶孤城/Documents/source_data/d.csv")                  #这是高东海的路径
china_map <- rgdal::readOGR("C:/Users/一叶孤城/Documents/source_data/bou2_4p.shp")   #这是高东海的路径


源代码中画地图时需要加载外部文件，这是组员的路径，运行时根据自己保存文件的工作路径修改即可。
