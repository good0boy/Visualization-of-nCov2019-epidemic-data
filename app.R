library(DT)
library(nCov2019)
library(shiny)
library(ggplot2)
library(forcats)
library(chinamap)
library(magick)
library(quantmod)
library(ggrepel)
library(leaflet)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(gganimate)
library(dplyr)
source('citys.R')
x=get_nCov2019()
y=load_nCov2019(lang = 'en')#load 取历史数据
d0=y['global']
nCov2019_set_country('China')
yy<-load_nCov2019()
h<-summary(yy,"湖北")
h$time<-as.Date(h$time,format="%Y-%m-%d")
#-------------------------------------王伟部分
s1=subset(yy[],city!="监狱系统")
s2=subset(s1,city!="外地来京")
s3=subset(s2,city!="外地来沪")
s4=subset(s3,city!="境外输入")
s4[which(s4$city=="恩施"),"name"] <- "恩施州"
yyy<-s4

#--------------------------------------------
d <- x[]      #获取本国数据
data_1 <- data.frame(d$name,d$confirm,d$dead,d$heal,d$deadRate,d$healRate)
d[which(d$name=="香港"),"name"] <- "香港特别行政区"
d[which(d$name=="台湾"),"name"] <- "台湾省"
d[which(d$name=="上海"),"name"] <- "上海市"
d[which(d$name=="福建"),"name"] <- "福建省"
d[which(d$name=="广东"),"name"] <- "广东省"
d[which(d$name=="四川"),"name"] <- "四川省"
d[which(d$name=="内蒙古"),"name"] <- "内蒙古自治区"
d[which(d$name=="陕西"),"name"] <- "陕西省"
d[which(d$name=="云南"),"name"] <- "云南省"
d[which(d$name=="山东"),"name"] <- "山东省"
d[which(d$name=="河南"),"name"] <- "河南省"
d[which(d$name=="浙江"),"name"] <- "浙江省"
d[which(d$name=="江苏"),"name"] <- "江苏省"
d[which(d$name=="天津"),"name"] <- "天津市"
d[which(d$name=="山西"),"name"] <- "山西省"
d[which(d$name=="北京"),"name"] <- "北京市"
d[which(d$name=="广西"),"name"] <- "广西壮族自治区"
d[which(d$name=="湖北"),"name"] <- "湖北省"
d[which(d$name=="重庆"),"name"] <- "重庆市"
d[which(d$name=="宁夏"),"name"] <- "宁夏回族自治区"
d[which(d$name=="辽宁"),"name"] <- "辽宁省"
#d[which(d$name=="澳门"),"name"] <-
d[which(d$name=="江西"),"name"] <- "江西省"
d[which(d$name=="贵州"),"name"] <- "贵州省"
d[which(d$name=="青海"),"name"] <- "青海省"
d[which(d$name=="海南"),"name"] <- "海南省"
d[which(d$name=="吉林"),"name"] <- "吉林省"
d[which(d$name=="西藏"),"name"] <- "西藏自治区"
d[which(d$name=="黑龙江"),"name"] <- "黑龙江省"
d[which(d$name=="安徽"),"name"] <- "安徽省"
d[which(d$name=="河北"),"name"] <- "河北省"
d[which(d$name=="湖南"),"name"] <- "湖南省"
d[which(d$name=="新疆"),"name"] <- "新疆维吾尔自治区"
d[which(d$name=="甘肃"),"name"] <- "甘肃省"

names(d)[names(d) == 'name'] <- 'NAME'


#china_map <- rgdal::readOGR("C:/Users/lenovo/bou2_4p.shp")   #这是侯星铭的路径
china_map <- rgdal::readOGR("C:/Users/一叶孤城/Documents/source_data/bou2_4p.shp")  #这是高东海的路径
#Encoding(china_map@data$NAME) <- "UTF-8"    #纠正中文字符乱码
shape <- merge(china_map,d,by="NAME")

#get 取最近数据
# d1= head(x['global'],11)
# #z=z[-1,]  #删除z的第一行
# d1$confirm=as.numeric(d1$confirm)
# d1$name = fct_reorder(d1$name, d1$confirm)

ui<- fluidPage(
    
    # Give the page a title
    titlePanel("the visualization of COVID-19"),
    tabsetPanel(
        tabPanel( p("World"),  #the world 选项卡
                  h5(strong("这里能够直观看到COVID-19在全球的累计感染情况，可见COVID-19在全球范围内
                        对各个国家都造成了毁灭性的伤害")),
                  plotOutput("ConfirmedWorld", height = 500),
                  br(),
                  h5(strong("这里可以看到累计感染、现有确诊以及治愈情况在全球排名靠前的国家 ")),
                  h5(strong('当然，作为参考我们把中国也加入了进来:')),
                  fluidRow(
                      column(width=3,
                             selectInput("data_type",
                                         label = "You can choose the data type you wanna see here:",
                                         choices = c('confirm','dead','deadRate','heal','healRate','now_confirm'),
                                         selected = "confirm")),
                      column(width=3,
                             numericInput("num",
                                          label=("You can choose the number of the top countries in "),
                                          value = 10,min=1,max=20))),
                  plotOutput("top_countries"),
                  br(),
                  br(),
                  h5(strong('这里可以选择自己感兴趣的国家，以了解该国家疫情爆发以来的疫情数据:
                        (such as China,South Korea,United States,Japan,Iran,Italy,Germany,United Kingdom and so on)')),
                  fluidRow(
                      column(width=3,
                             selectInput("country",
                                         label = "Choose a country:",
                                         choices = c('China','South Korea','United States','Japan','Iran','Italy','Germany','United Kingdom'),
                                         selected = "United States")),
                      column(width=3,
                             sliderInput("date_1", "请选择一个观察期:",
                                         min = as.Date(min(h$time),format="%Y-%m-%d"),
                                         max = as.Date(max(h$time),format="%Y-%m-%d"),
                                         value = c(as.Date(min(h$time),format="%Y-%m-%d"),as.Date(max(h$time),format="%Y-%m-%d"))),
                      ),
                      column(width=6,
                             plotOutput("country_confirmed"))),
                  br(),
                  h5(strong('这里可以选择某个国家中的特定城市，以了解该城市疫情爆发后的疫情情况（城市和之前所选的国家需匹配）:
                (such as Washington State in United States, Calabria in Italy, Tokyo in Japan and so on)')),
                  fluidRow(
                      column(width=3,
                             conditionalPanel(
                                 condition = "input.country == 'China'",
                                 selectInput(
                                     "city1", "choose a city:",
                                     choices=citys('China'),
                                     selected = "Hubei"
                                 )
                             ),
                             conditionalPanel(
                                 condition = "input.country == 'South Korea'",
                                 selectInput(
                                     "city2", "choose a city:",
                                     choices=citys('South Korea'),
                                     selected = "Daegu"
                                 )
                             ),
                             conditionalPanel(
                                 condition = "input.country == 'United States'",
                                 selectInput(
                                     "city3", "choose a city:",
                                     choices=citys('United States'),
                                     selected = "Washington State"
                                 )
                             ),
                             conditionalPanel(
                                 condition = "input.country == 'Japan'",
                                 selectInput(
                                     "city4", "choose a city:",
                                     choices=citys('Japan'),
                                     selected = "Hokkaido"
                                 )
                             ),
                             conditionalPanel(
                                 condition = "input.country == 'Iran'",
                                 selectInput(
                                     "city5", "choose a city:",
                                     choices=citys('Iran'),
                                     selected = "Tehran"
                                 )
                             ),
                             conditionalPanel(
                                 condition = "input.country == 'Italy'",
                                 selectInput(
                                     "city6", "choose a city:",
                                     choices=citys('Italy'),
                                     selected = "Lombardy"
                                 )
                             ),
                             conditionalPanel(
                                 condition = "input.country == 'Germany'",
                                 selectInput(
                                     "city7", "choose a city:",
                                     choices=citys('Germany'),
                                     selected = "Bavaria"
                                 )
                             ),
                             conditionalPanel(
                                 condition = "input.country == 'United Kingdom'",
                                 selectInput(
                                     "city8", "choose a city:",
                                     choices=citys('United Kingdom'),
                                     selected = "England"
                                 )
                             )
                      ),
                      column(width=3,
                             sliderInput("date_2", "请选择一个观察期:",
                                         min = as.Date(min(h$time),format="%Y-%m-%d"),
                                         max = as.Date(max(h$time),format="%Y-%m-%d"),
                                         value = c(as.Date(min(h$time),format="%Y-%m-%d"),as.Date(max(h$time),format="%Y-%m-%d"))),
                      ),
                      column(width=6,
                             plotOutput("city_confirmed"))),
                  p('下面是用户选择的国家的城市级数据：'),
                  DT::dataTableOutput("table_2")
        ),
        tabPanel(
            p("China"),
            titlePanel(strong("中国整体疫情")),
            fluidRow(
                h3(strong("中国疫情地图")),
                br(),
                column(6,radioButtons("radio",label="绘图指标",choices=c("累计确诊人数",
                                                                     "现存确诊人数","累计病死人数",
                                                                     "累计治愈人数"),
                                      selected="现存确诊人数"),
                       h4("【点击地图】可显示省份名字以及确诊、治愈、病死等详细信息。"),
                       h4("【鼠标滚轮/双击】、【点击左上角+/-】均可局部缩放地图。"),
                       br(),
                       leafletOutput("map")),
                column(6,DT::dataTableOutput("table"))
            )
            
        ),
        tabPanel( p("Citys"),
                  # Give the page a title
                  titlePanel("COVID-19"),
                  
                  # Generate a row with a sidebar
                  
                  verticalLayout(
                      selectInput("province",
                                  label="请选择一个城市：",
                                  choices=c("香港",   "台湾"  , "上海",   "福建" ,  "四川",  "广东"  , "陕西" ,
                                            "内蒙古" ,"天津",   "江苏",   "云南" ,  "山东" ,  "河南" ,  "浙江",
                                            "北京" ,"山西", "湖北", "广西" ,"辽宁", "重庆" ,  "河北",
                                            "甘肃" ,  "青海"  , "宁夏" ,  "黑龙江" ,"贵州",   "安徽",   "澳门",
                                            "海南" ,  "新疆",   "江西",   "湖南",   "吉林",   "西藏"),
                                  selected=("湖北")),
                      h5(strong("选定省份之后，这里你可以看到该省内各城市累计确诊、累计治愈和现有确诊的情况（也包含了境外输入的数据）： ")),
                      tabsetPanel(
                          tabPanel("confirmed",plotOutput(outputId = "Confirmed")),
                          tabPanel("heal",plotOutput(outputId = "heal")),
                          tabPanel("nowconfirm",plotOutput(outputId = "nowconfirm"))
                      ),
                      
                      br(),
                      br(),
                      h5(strong("这里对于选定省份的各个城市数据展示的更加从分和具体，同时也提供了搜索功能，用户可根据自己的需求来检索省份内某一具体城市信息。")),
                      DT::dataTableOutput("table_1"),
                      h5(strong('这里可以看到选定省份的各城市自疫情爆发以来的累计确诊的趋势，很明显武汉的确诊人数是疯狂超标的，
                                这也从侧面反映了我们国家当时在疫情爆发初期采取封城策略是明智的：')),
                      
                      sliderInput("date", "date:",
                                  min = as.Date(min(h$time),format="%Y-%m-%d"),
                                  max = as.Date(max(h$time),format="%Y-%m-%d"),
                                  value = c(as.Date(min(h$time),format="%Y-%m-%d"),as.Date(max(h$time),format="%Y-%m-%d"))),
                      h5(strong('这里用户可以选定时间段，单独研究所选省份的特定时间段的疫情传播情况（对具体研究可以起到作用，如分段时滞微分方程模型）：')),
                      uiOutput('city'),
                      tabsetPanel(
                          tabPanel("省份",plotOutput(outputId = "citys_confirmed")),
                          tabPanel("城市",plotOutput(outputId = "cum_Confirm"))
                      )
                  )
        ),
        tabPanel( p('关于'),
                  br(),
                  h4(strong('数据来源：')),
                  br(),
                  p('1、',a('Wuhan-2019-nCoV GitHub repository.',href='https://i.snssdk.com/ugc/hotboard_fe/hot_list/template/hot_list/forum_tab.html')),
                  p("该数据源是今日头条,包含了中国详细的城市级数据和世界范围内的国家级数据,也是默认访问的数据源。"),
                  p('2、',a('National Health Commission of the People’s Republic of China',href='http://www.nhc.gov.cn/xcs/yqtb/list_gzbd.shtml')),
                  p("该数据源包含中国的省级数据。"),
                  p('3、',a('DXY.cn. Pneumonia. 2020.',href='https://ncov.dxy.cn/ncovh5/view/pneumonia')),
                  p("该数据源包含了中国历史城市水平的数据。"),
                  p("这些数据的形式基本相同，但默认数据源具有更全面的全局历史信息，也包含较旧的历史数据。"),
                  p("若换用其他数据源，只需修改数据加载的source选项即可："),
                  p("丁香园数据源：load_nCov2019(source = 'dxy')"),
                  p("中国国家卫生健康委员会数据源：load_nCov2019(source = 'cnnhc')"),
                  h4(strong('数据加载方式：')),
                  p("本文所有数据的获取，以及各种操作处理都基于南方医科大学余光创教授在GitHub上写的",a("nCov2019",herf='https://github.com/GuangchuangYu/nCov2019','包,特此感谢余光创教授做出的贡献。'),),
                  h4(strong('分工：')),
                  br(),
                  helpText('整个shiny app 框架分为三个部分，world、china 和 city，从不同层级对疫情数据做出可视化处理，方便具有各种需求的人们
                       有效地了解过往及现在的疫情信息，同时我们队的每个成员负责一个选项卡的编程与修改'),
                  h4(strong('功能介绍：')),
                  p(strong("1、World 界面:")),
                  p('world 界面包含一张图片，一个柱状图和两个曲线图以及一个datatable,分别从不同维度描述了用户所选国家的疫情情况。'),
                  p('第一张图片是全球所有国家累计感染新冠肺炎的热图，从图片中可以看到虽然疫情最开始在我国爆发，但是我国的累计感染人数在世界各国中不算排名靠前的，
                    反而是美国、俄罗斯以及加拿大等国家的累积感染人数非常多，这从侧面也反映了我国在疫情控制方面的杰出成果。'),
                  p('第二个柱状图是根据用户的选择对全球累计感染、现有确诊、治愈率、死亡率等方面靠前的国家进行排名并展示具体的数据。'),
                  p('第三张图会根据用户选择的国家，展示该国家疫情爆发以来，累计确诊、累计死亡、累计治愈和现有确诊的情况和趋势。'),
                  p("第四张图是在用户选定上面的国家后，进而输入该国家的任意一个城市，就可以得到该城市的累计确诊、累计死亡、累计治愈和现有确诊的情况和趋势.",strong('(前提是国家和城市必须匹配)')),
                  p('在world 界面的datatable 显示的是用户选定国家后，该国家的城市级别数据，相比于曲线图的信息，datatable的数据更加直观，也更加具体，并且为用户提供了搜索功能。'),
                  p(strong("2、China 界面:")),
                  p('China界面包含两个部分，第一个部分是左侧的可以放大或缩小的地图，第二部分是右侧的表格。'),
                  p('左侧的地图可以观看四个指标，你可以看到某个指标在中国各个省的
                    大致分布情况。比如，你选择了指标“现存确诊人数”，地图会直观地呈现
                    现存确诊人数在中国各省的数量情况。并且，用鼠标点击各个省在地图上的位置，地图会相应显示
                    这个指标的数量。'),
                  p('在右侧的表格中，你可以直接看到各个省各个指标的数量，并且可以进行
                    按条件查询等操作。'),
                  p(strong("3、City 界面:")),
                  p('city界面包含四个部分，分别是一个柱状图和两个曲线图以及一个datatable，通过选定特定省份，图像详细描述了特定城市的疫情情况。'),
                  p('第一张图展现了选定省份的各个城市以及境外输入最新的疫情被感染确诊总人数，痊愈人数以及最新感染的数据。'),
                  p('第二张刻画了从疫情开始以来选定省份各个城市被感染总人数的趋势图，可以看到前期被感染人数的急剧增长到后期曲线趋于平缓，说明疫情得以控制，感染人数渐渐停止了急剧增加的势头。'),
                  p('第三张图展示的是被选定省份的各个城市的疫情详细数据，可以通过搜索框搜索想要查询的城市的疫情数据，其中包括确诊人数，死亡人数，痊愈人数以及相应的死亡率，痊愈率。'),
                  p('第四张图展示了所选定省份总的累计确诊人数，痊愈人数，死亡人数随时间的变化情况，通过比较三条曲线，可以看到三个数据渐趋于平缓，且痊愈人数到后期愈发接近确诊人数，
                这不仅说明疫情得以控制，感染人数停止增加，更说明治疗效果渐好，有赖于一线抗疫英雄，国家调控管理，抗疫防疫，形势渐好。'),
                  
                  
        )
    )
)


#服务器函数
server <- function(input, output) {       #}   #这里server部分全部注释为空
    output$ConfirmedWorld<-renderPlot({
        plot(x)
        
    })
    output$top_countries<-renderPlot({
        d1= head(x['global'],input$num)
        #d1=d1[-1,]  #删除z的第一行
        d1$now_confirm=d1$confirm-d1$dead-d1$heal
        d1$answer=as.numeric(d1[[input$data_type]])
        d1$name = fct_reorder(d1$name, d1$answer)
        ggplot(d1, aes(name, answer)) +
            geom_col(fill='steelblue') + coord_flip() +
            geom_text(aes(y = answer+2, label=answer), hjust=0) +
            theme_minimal(base_size=14) +
            scale_y_continuous(expand=c(0.2,10)) +
            xlab(NULL) + ylab(NULL)+labs(caption = paste("accessed date:", time(x)))
    })
    output$country_confirmed<-renderPlot({
        
        time1=d0[which(d0$country==input$country),'time']
        cum_confirm1=d0[which(d0$country==input$country),'cum_confirm']
        cum_heal1=d0[which(d0$country==input$country),'cum_heal']
        cum_dead1=d0[which(d0$country==input$country),'cum_dead']
        now_confirm1=cum_confirm1-cum_heal1-cum_dead1
        dd=data.frame('time'=time1,'cum_confirm'=cum_confirm1,'cum_heal'=cum_heal1,'cum_dead'=cum_dead1,'now_confirm'=now_confirm1)
        dd_1=subset(dd,time>=input$date_1[1])
        dd=subset(dd_1,time<=input$date_1[2])
        ggplot(dd,aes(x=time))+
            geom_line(aes(y=as.numeric(cum_confirm),colour='cum_confirm'),lwd=1.2)+
            geom_line(aes(y=as.numeric(cum_heal),colour='cum_heal'),lwd=1.2)+
            geom_line(aes(y=as.numeric(cum_dead),colour='cum_dead'),lwd=1.2)+
            geom_line(aes(y=as.numeric(now_confirm),colour='now_confirm'),lwd=1.2)+
            xlab(NULL)+ylab(NULL)+
            scale_color_manual(values=c('red','blue','black','green'))+
            labs(title=paste("Number of cum_confirms,cum_deaths,cum_heals and now_confirm in ",input$country))
        
    })
    yr <- reactive({switch(
        input$country,
        'China'=input$city1,
        'South Korea'=input$city2,
        'United States'=input$city3,
        'Japan'=input$city4,
        'Iran'=input$city5,
        'Italy'=input$city6,
        'Germany'=input$city7,
        'United Kingdom'=input$city8
    )})
    output$city_confirmed<-renderPlot({
        nCov2019_set_country(country =input$country)
        ddd=y['province']
        time2=ddd[which(ddd$province==yr()),'time']
        cum_confirm2=ddd[which(ddd$province==yr()),'cum_confirm']
        cum_heal2=ddd[which(ddd$province==yr()),'cum_heal']
        cum_dead2=ddd[which(ddd$province==yr()),'cum_dead']
        now_confirm2=cum_confirm2-cum_heal2-cum_dead2
        dddd<-data.frame('time'=time2,'province'=rep(yr(),length(cum_heal2)),'cum_confirm'=cum_confirm2,'cum_heal'=cum_heal2,'cum_dead'=cum_dead2,'now_confirm'=now_confirm2)
        dddd_1=subset(dddd,time>=input$date_2[1])
        dddd=subset(dddd_1,time<=input$date_2[2])
        ggplot(dddd,aes(x=time))+
            geom_line(aes(y=as.numeric(cum_confirm),colour='cum_confirm'),lwd=1.2)+
            geom_line(aes(y=as.numeric(cum_heal),colour='cum_heal'),lwd=1.2)+
            geom_line(aes(y=as.numeric(cum_dead),colour='cum_dead'),lwd=1.2)+
            geom_line(aes(y=as.numeric(now_confirm),colour='now_confirm'),lwd=1.2)+
            xlab(NULL)+ylab(NULL)+
            scale_color_manual(values=c('red','blue','black','green'))+
            labs(title=paste("Number of cum_confirms,cum_deaths,cum_heals and now_confirm in ",input$city))
    })
    output$table_2 <- DT::renderDataTable({
        nCov2019_set_country(country =input$country)
        ddd=y['province']
        ddd$now_confirm=ddd$cum_confirm-ddd$cum_heal-ddd$cum_dead
        x=tail(ddd,1)
        x=x$time
        ddddd=tail(ddd,length(which(ddd$time==x)))
        DT::datatable(ddddd,
                      colnames=c('时间',"国家","城市","累计确诊","累积治愈","累计死亡","现有确诊"),
                      #caption="中国最新疫情情况",
                      options=list(columnDefs=list(list(className = 'dt-center', targets = 0:6)),
                                   language = list(
                                       info = '',     #不显示左下角的提示
                                       search = '搜索:',
                                       paginate = list(previous = '上页', `next` = '下页'),
                                       lengthMenu = '显示 _MENU_ 项结果')
                                   
                      ))
        
        
    })
    #----------------------------------插入侯星铭部分----------------------
    output$table <- DT::renderDataTable({
        
        DT::datatable(data_1,
                      colnames=c("省份","累计确诊","病死人数","治愈人数","病死率(%)","治愈率(%)"),
                      #caption="中国最新疫情情况",
                      options=list(columnDefs=list(list(className = 'dt-center', targets = 0:6)),
                                   language = list(
                                       info = '',     #不显示左下角的提示
                                       search = '搜索:',
                                       paginate = list(previous = '上页', `next` = '下页'),
                                       lengthMenu = '显示 _MENU_ 项结果'),aLengthMenu=c(10,20,34)
                                   
                                   
                      ))
        
        
    })
    
    output$map <- renderLeaflet({
        mapdata <- switch(input$radio,
                          "累计确诊人数"=shape$confirm,"现存确诊人数"=shape$nowConfirm,
                          "累计病死人数"=shape$dead,"累计治愈人数"=shape$heal)
        
        pal <- colorQuantile("Greens",mapdata)
        i_popup <- paste0("<strong>省份:</strong>",shape$NAME,"<br>",
                          "<strong>数量:</strong>",mapdata)  #设置鼠标悬浮框
        
        leaflet(shape)%>%addTiles()%>%setView(105.387021,35.969369,zoom=3)%>%
            addPolygons(fillColor=~pal(mapdata),fillOpacity=1,weight=1,
                        popup=i_popup)%>%
            addLegend(pal=pal,values=mapdata,position="bottomright")
        
    })
    
    ##----------------------------------插入王伟部分----------------------
    output$selected_city <- renderText({
        paste("You have selected", trans_province(input$province,lang='en'))
    })
    citys<-reactive({
        s5<-subset(yyy,province==input$province)
        unique(s5$city)
    })
    output$city<-renderUI({
        selectInput('selectCity', 'Select the  city: ', choices = citys())
    })
    output$Confirmed<-renderPlot({
        
        d=x[input$province,]
        d$confirm=as.numeric(d$confirm)
        d$name = fct_reorder(d$name, d$confirm)
        ggplot(d, aes(name, confirm)) +
            geom_col(fill='steelblue') + coord_flip() +
            geom_text(aes(y = confirm+2, label=confirm), hjust=0) +
            theme_minimal(base_size=14) +
            scale_y_continuous(expand=c(0,10)) +
            xlab(NULL) + ylab(NULL)+labs(caption = paste("accessed date:", time(x)))
        
        
    })
    output$heal<-renderPlot({
        
        
        d=x[input$province,]
        d$heal=as.numeric(d$heal)
        d$name = fct_reorder(d$name, d$heal)
        ggplot(d, aes(name, heal)) +
            geom_col(fill='steelblue') + coord_flip() +
            geom_text(aes(y = heal+2, label=heal), hjust=0) +
            theme_minimal(base_size=14) +
            scale_y_continuous(expand=c(0,10)) +
            xlab(NULL) + ylab(NULL)+labs(caption = paste("accessed date:", time(x)))
    })
    output$nowconfirm<-renderPlot({
        
        
        d=x[input$province,]
        d$nowConfirm=as.numeric(d$nowConfirm)
        d$name = fct_reorder(d$name, d$nowConfirm)
        ggplot(d, aes(name, nowConfirm)) +
            geom_col(fill='steelblue') + coord_flip() +
            geom_text(aes(y = nowConfirm+2, label=nowConfirm), hjust=0) +
            theme_minimal(base_size=14) +
            scale_y_continuous(expand=c(0,10)) +
            xlab(NULL) + ylab(NULL)+labs(caption = paste("accessed date:", time(x)))
    })
    
    output$cum_Confirm <- renderPlot({
        d<-subset(yyy,province==input$province)
        d1 <- subset(d,city==input$selectCity)
        ggplot(d1,aes(x=time))+
            geom_line(aes(y=as.numeric(cum_confirm),colour="cum_confirm"),linetype=1)+
            geom_point(aes(y=as.numeric(cum_confirm),colour="cum_confirm"))+
            geom_line(aes(y=as.numeric(cum_heal),colour="cum_heal"),linetype=1)+
            geom_point(aes(y=as.numeric(cum_heal),colour="cum_heal"))+
            geom_line(aes(y=as.numeric(cum_dead),colour="cum_dead"),linetype=1)+
            geom_point(aes(y=as.numeric(cum_dead),colour="cum_dead"))+
            xlab(NULL)+ylab(NULL)+
            scale_color_manual(values=c('red','blue','green'))
    })
    
    
    
    output$citys_confirmed<-renderPlot({
        nCov2019_set_country('China')
        y1<-load_nCov2019()
        w<-summary(y1,input$province)
        w1<-subset(w,time<=input$date[2])
        w<-subset(w1,time>=input$date[1])
        ggplot(w,aes(x=time))+
            geom_line(aes(y=as.numeric(cum_confirm),colour="cum_confirm"),linetype=1)+
            geom_point(aes(y=as.numeric(cum_confirm),colour="cum_confirm"))+
            geom_line(aes(y=as.numeric(cum_heal),colour="cum_heal"),linetype=1)+
            geom_point(aes(y=as.numeric(cum_heal),colour="cum_heal"))+
            geom_line(aes(y=as.numeric(cum_dead),colour="cum_dead"),linetype=1)+
            geom_point(aes(y=as.numeric(cum_dead),colour="cum_dead"))+
            xlab(NULL)+ylab(NULL)+
            scale_color_manual(values=c('red','blue','green'))
    })
    output$table_1<-DT::renderDataTable({
        s <- x[input$province,]
        data<-data.frame(s$name,s$nowConfirm,s$confirm ,s$suspect,s$dead,s$deadRate,s$heal,s$healRate )
        DT::datatable(data,
                      colnames=c("城市","新增确诊","累计确诊","疑似","死亡人数","死亡率","痊愈人数","痊愈率"),
                      options=list(columnDefs=list(list(className = 'dt-center', targets = 0:6)),
                                   language = list(
                                       info = '',     #不显示左下角的提示
                                       search = '搜索:',
                                       paginate = list(previous = '上页', `next` = '下页'),
                                       lengthMenu = '显示 _MENU_ 项结果')
                      ))
        
        
    })
}
shinyApp(ui,server)
