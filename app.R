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
y=load_nCov2019(lang = 'en')#load ȡ��ʷ����
d0=y['global']
nCov2019_set_country('China')
yy<-load_nCov2019()
h<-summary(yy,"����")
h$time<-as.Date(h$time,format="%Y-%m-%d")
#-------------------------------------��ΰ����
s1=subset(yy[],city!="����ϵͳ")
s2=subset(s1,city!="�������")
s3=subset(s2,city!="�������")
s4=subset(s3,city!="��������")
s4[which(s4$city=="��ʩ"),"name"] <- "��ʩ��"
yyy<-s4

#--------------------------------------------
d <- x[]      #��ȡ��������
data_1 <- data.frame(d$name,d$confirm,d$dead,d$heal,d$deadRate,d$healRate)
d[which(d$name=="���"),"name"] <- "����ر�������"
d[which(d$name=="̨��"),"name"] <- "̨��ʡ"
d[which(d$name=="�Ϻ�"),"name"] <- "�Ϻ���"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="�㶫"),"name"] <- "�㶫ʡ"
d[which(d$name=="�Ĵ�"),"name"] <- "�Ĵ�ʡ"
d[which(d$name=="���ɹ�"),"name"] <- "���ɹ�������"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="ɽ��"),"name"] <- "ɽ��ʡ"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="�㽭"),"name"] <- "�㽭ʡ"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="���"),"name"] <- "�����"
d[which(d$name=="ɽ��"),"name"] <- "ɽ��ʡ"
d[which(d$name=="����"),"name"] <- "������"
d[which(d$name=="����"),"name"] <- "����׳��������"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="����"),"name"] <- "������"
d[which(d$name=="����"),"name"] <- "���Ļ���������"
d[which(d$name=="����"),"name"] <- "����ʡ"
#d[which(d$name=="����"),"name"] <-
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="�ຣ"),"name"] <- "�ຣʡ"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="����"),"name"] <- "����������"
d[which(d$name=="������"),"name"] <- "������ʡ"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="�ӱ�"),"name"] <- "�ӱ�ʡ"
d[which(d$name=="����"),"name"] <- "����ʡ"
d[which(d$name=="�½�"),"name"] <- "�½�ά���������"
d[which(d$name=="����"),"name"] <- "����ʡ"

names(d)[names(d) == 'name'] <- 'NAME'


#china_map <- rgdal::readOGR("C:/Users/lenovo/bou2_4p.shp")   #���Ǻ�������·��
china_map <- rgdal::readOGR("C:/Users/һҶ�³�/Documents/source_data/bou2_4p.shp")  #���Ǹ߶�����·��
#Encoding(china_map@data$NAME) <- "UTF-8"    #���������ַ�����
shape <- merge(china_map,d,by="NAME")

#get ȡ�������
# d1= head(x['global'],11)
# #z=z[-1,]  #ɾ��z�ĵ�һ��
# d1$confirm=as.numeric(d1$confirm)
# d1$name = fct_reorder(d1$name, d1$confirm)

ui<- fluidPage(
    
    # Give the page a title
    titlePanel("the visualization of COVID-19"),
    tabsetPanel(
        tabPanel( p("World"),  #the world ѡ�
                  h5(strong("�����ܹ�ֱ�ۿ���COVID-19��ȫ����ۼƸ�Ⱦ������ɼ�COVID-19��ȫ��Χ��
                        �Ը������Ҷ�����˻����Ե��˺�")),
                  plotOutput("ConfirmedWorld", height = 500),
                  br(),
                  h5(strong("������Կ����ۼƸ�Ⱦ������ȷ���Լ����������ȫ��������ǰ�Ĺ��� ")),
                  h5(strong('��Ȼ����Ϊ�ο����ǰ��й�Ҳ�����˽���:')),
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
                  h5(strong('�������ѡ���Լ�����Ȥ�Ĺ��ң����˽�ù������鱬����������������:
                        (such as China,South Korea,United States,Japan,Iran,Italy,Germany,United Kingdom and so on)')),
                  fluidRow(
                      column(width=3,
                             selectInput("country",
                                         label = "Choose a country:",
                                         choices = c('China','South Korea','United States','Japan','Iran','Italy','Germany','United Kingdom'),
                                         selected = "United States")),
                      column(width=3,
                             sliderInput("date_1", "��ѡ��һ���۲���:",
                                         min = as.Date(min(h$time),format="%Y-%m-%d"),
                                         max = as.Date(max(h$time),format="%Y-%m-%d"),
                                         value = c(as.Date(min(h$time),format="%Y-%m-%d"),as.Date(max(h$time),format="%Y-%m-%d"))),
                      ),
                      column(width=6,
                             plotOutput("country_confirmed"))),
                  br(),
                  h5(strong('�������ѡ��ĳ�������е��ض����У����˽�ó������鱬�����������������к�֮ǰ��ѡ�Ĺ�����ƥ�䣩:
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
                             sliderInput("date_2", "��ѡ��һ���۲���:",
                                         min = as.Date(min(h$time),format="%Y-%m-%d"),
                                         max = as.Date(max(h$time),format="%Y-%m-%d"),
                                         value = c(as.Date(min(h$time),format="%Y-%m-%d"),as.Date(max(h$time),format="%Y-%m-%d"))),
                      ),
                      column(width=6,
                             plotOutput("city_confirmed"))),
                  p('�������û�ѡ��Ĺ��ҵĳ��м����ݣ�'),
                  DT::dataTableOutput("table_2")
        ),
        tabPanel(
            p("China"),
            titlePanel(strong("�й���������")),
            fluidRow(
                h3(strong("�й������ͼ")),
                br(),
                column(6,radioButtons("radio",label="��ͼָ��",choices=c("�ۼ�ȷ������",
                                                                     "�ִ�ȷ������","�ۼƲ�������",
                                                                     "�ۼ���������"),
                                      selected="�ִ�ȷ������"),
                       h4("�������ͼ������ʾʡ�������Լ�ȷ���������������ϸ��Ϣ��"),
                       h4("��������/˫��������������Ͻ�+/-�����ɾֲ����ŵ�ͼ��"),
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
                                  label="��ѡ��һ�����У�",
                                  choices=c("���",   "̨��"  , "�Ϻ�",   "����" ,  "�Ĵ�",  "�㶫"  , "����" ,
                                            "���ɹ�" ,"���",   "����",   "����" ,  "ɽ��" ,  "����" ,  "�㽭",
                                            "����" ,"ɽ��", "����", "����" ,"����", "����" ,  "�ӱ�",
                                            "����" ,  "�ຣ"  , "����" ,  "������" ,"����",   "����",   "����",
                                            "����" ,  "�½�",   "����",   "����",   "����",   "����"),
                                  selected=("����")),
                      h5(strong("ѡ��ʡ��֮����������Կ�����ʡ�ڸ������ۼ�ȷ��ۼ�����������ȷ��������Ҳ�����˾�����������ݣ��� ")),
                      tabsetPanel(
                          tabPanel("confirmed",plotOutput(outputId = "Confirmed")),
                          tabPanel("heal",plotOutput(outputId = "heal")),
                          tabPanel("nowconfirm",plotOutput(outputId = "nowconfirm"))
                      ),
                      
                      br(),
                      br(),
                      h5(strong("�������ѡ��ʡ�ݵĸ�����������չʾ�ĸ��Ӵӷֺ;��壬ͬʱҲ�ṩ���������ܣ��û��ɸ����Լ�������������ʡ����ĳһ���������Ϣ��")),
                      DT::dataTableOutput("table_1"),
                      h5(strong('������Կ���ѡ��ʡ�ݵĸ����������鱬���������ۼ�ȷ������ƣ��������人��ȷ�������Ƿ�񳬱�ģ�
                                ��Ҳ�Ӳ��淴ӳ�����ǹ��ҵ�ʱ�����鱬�����ڲ�ȡ��ǲ��������ǵģ�')),
                      
                      sliderInput("date", "date:",
                                  min = as.Date(min(h$time),format="%Y-%m-%d"),
                                  max = as.Date(max(h$time),format="%Y-%m-%d"),
                                  value = c(as.Date(min(h$time),format="%Y-%m-%d"),as.Date(max(h$time),format="%Y-%m-%d"))),
                      h5(strong('�����û�����ѡ��ʱ��Σ������о���ѡʡ�ݵ��ض�ʱ��ε����鴫��������Ծ����о����������ã���ֶ�ʱ��΢�ַ���ģ�ͣ���')),
                      uiOutput('city'),
                      tabsetPanel(
                          tabPanel("ʡ��",plotOutput(outputId = "citys_confirmed")),
                          tabPanel("����",plotOutput(outputId = "cum_Confirm"))
                      )
                  )
        ),
        tabPanel( p('����'),
                  br(),
                  h4(strong('������Դ��')),
                  br(),
                  p('1��',a('Wuhan-2019-nCoV GitHub repository.',href='https://i.snssdk.com/ugc/hotboard_fe/hot_list/template/hot_list/forum_tab.html')),
                  p("������Դ�ǽ���ͷ��,�������й���ϸ�ĳ��м����ݺ����緶Χ�ڵĹ��Ҽ�����,Ҳ��Ĭ�Ϸ��ʵ�����Դ��"),
                  p('2��',a('National Health Commission of the People��s Republic of China',href='http://www.nhc.gov.cn/xcs/yqtb/list_gzbd.shtml')),
                  p("������Դ�����й���ʡ�����ݡ�"),
                  p('3��',a('DXY.cn. Pneumonia. 2020.',href='https://ncov.dxy.cn/ncovh5/view/pneumonia')),
                  p("������Դ�������й���ʷ����ˮƽ�����ݡ�"),
                  p("��Щ���ݵ���ʽ������ͬ����Ĭ������Դ���и�ȫ���ȫ����ʷ��Ϣ��Ҳ�����Ͼɵ���ʷ���ݡ�"),
                  p("��������������Դ��ֻ���޸����ݼ��ص�sourceѡ��ɣ�"),
                  p("����԰����Դ��load_nCov2019(source = 'dxy')"),
                  p("�й�������������ίԱ������Դ��load_nCov2019(source = 'cnnhc')"),
                  h4(strong('���ݼ��ط�ʽ��')),
                  p("�����������ݵĻ�ȡ���Լ����ֲ��������������Ϸ�ҽ�ƴ�ѧ��ⴴ������GitHub��д��",a("nCov2019",herf='https://github.com/GuangchuangYu/nCov2019','��,�ش˸�л��ⴴ���������Ĺ��ס�'),),
                  h4(strong('�ֹ���')),
                  br(),
                  helpText('����shiny app ��ܷ�Ϊ�������֣�world��china �� city���Ӳ�ͬ�㼶�����������������ӻ�������������и������������
                       ��Ч���˽���������ڵ�������Ϣ��ͬʱ���Ƕӵ�ÿ����Ա����һ��ѡ��ı�����޸�'),
                  h4(strong('���ܽ��ܣ�')),
                  p(strong("1��World ����:")),
                  p('world �������һ��ͼƬ��һ����״ͼ����������ͼ�Լ�һ��datatable,�ֱ�Ӳ�ͬά���������û���ѡ���ҵ����������'),
                  p('��һ��ͼƬ��ȫ�����й����ۼƸ�Ⱦ�¹ڷ��׵���ͼ����ͼƬ�п��Կ�����Ȼ�����ʼ���ҹ������������ҹ����ۼƸ�Ⱦ��������������в���������ǰ�ģ�
                    ����������������˹�Լ����ô�ȹ��ҵ��ۻ���Ⱦ�����ǳ��࣬��Ӳ���Ҳ��ӳ���ҹ���������Ʒ���Ľܳ��ɹ���'),
                  p('�ڶ�����״ͼ�Ǹ����û���ѡ���ȫ���ۼƸ�Ⱦ������ȷ������ʡ������ʵȷ��濿ǰ�Ĺ��ҽ���������չʾ��������ݡ�'),
                  p('������ͼ������û�ѡ��Ĺ��ң�չʾ�ù������鱬���������ۼ�ȷ��ۼ��������ۼ�����������ȷ�����������ơ�'),
                  p("������ͼ�����û�ѡ������Ĺ��Һ󣬽�������ù��ҵ�����һ�����У��Ϳ��Եõ��ó��е��ۼ�ȷ��ۼ��������ۼ�����������ȷ������������.",strong('(ǰ���ǹ��Һͳ��б���ƥ��)')),
                  p('��world �����datatable ��ʾ�����û�ѡ�����Һ󣬸ù��ҵĳ��м������ݣ����������ͼ����Ϣ��datatable�����ݸ���ֱ�ۣ�Ҳ���Ӿ��壬����Ϊ�û��ṩ���������ܡ�'),
                  p(strong("2��China ����:")),
                  p('China��������������֣���һ�����������Ŀ��ԷŴ����С�ĵ�ͼ���ڶ��������Ҳ�ı���'),
                  p('���ĵ�ͼ���Թۿ��ĸ�ָ�꣬����Կ���ĳ��ָ�����й�����ʡ��
                    ���·ֲ���������磬��ѡ����ָ�ꡰ�ִ�ȷ������������ͼ��ֱ�۵س���
                    �ִ�ȷ���������й���ʡ��������������ң������������ʡ�ڵ�ͼ�ϵ�λ�ã���ͼ����Ӧ��ʾ
                    ���ָ���������'),
                  p('���Ҳ�ı����У������ֱ�ӿ�������ʡ����ָ������������ҿ��Խ���
                    ��������ѯ�Ȳ�����'),
                  p(strong("3��City ����:")),
                  p('city��������ĸ����֣��ֱ���һ����״ͼ����������ͼ�Լ�һ��datatable��ͨ��ѡ���ض�ʡ�ݣ�ͼ����ϸ�������ض����е����������'),
                  p('��һ��ͼչ����ѡ��ʡ�ݵĸ��������Լ������������µ����鱻��Ⱦȷ����������Ȭ�������Լ����¸�Ⱦ�����ݡ�'),
                  p('�ڶ��ſ̻��˴����鿪ʼ����ѡ��ʡ�ݸ������б���Ⱦ������������ͼ�����Կ���ǰ�ڱ���Ⱦ�����ļ���������������������ƽ����˵��������Կ��ƣ���Ⱦ��������ֹͣ�˼������ӵ���ͷ��'),
                  p('������ͼչʾ���Ǳ�ѡ��ʡ�ݵĸ������е�������ϸ���ݣ�����ͨ��������������Ҫ��ѯ�ĳ��е��������ݣ����а���ȷ������������������Ȭ�������Լ���Ӧ�������ʣ�Ȭ���ʡ�'),
                  p('������ͼչʾ����ѡ��ʡ���ܵ��ۼ�ȷ��������Ȭ������������������ʱ��ı仯�����ͨ���Ƚ��������ߣ����Կ����������ݽ�����ƽ������Ȭ�����������������ӽ�ȷ��������
                �ⲻ��˵��������Կ��ƣ���Ⱦ����ֹͣ���ӣ���˵������Ч�����ã�������һ�߿���Ӣ�ۣ����ҵ��ع��������߷��ߣ����ƽ��á�'),
                  
                  
        )
    )
)


#����������
server <- function(input, output) {       #}   #����server����ȫ��ע��Ϊ��
    output$ConfirmedWorld<-renderPlot({
        plot(x)
        
    })
    output$top_countries<-renderPlot({
        d1= head(x['global'],input$num)
        #d1=d1[-1,]  #ɾ��z�ĵ�һ��
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
                      colnames=c('ʱ��',"����","����","�ۼ�ȷ��","�ۻ�����","�ۼ�����","����ȷ��"),
                      #caption="�й������������",
                      options=list(columnDefs=list(list(className = 'dt-center', targets = 0:6)),
                                   language = list(
                                       info = '',     #����ʾ���½ǵ���ʾ
                                       search = '����:',
                                       paginate = list(previous = '��ҳ', `next` = '��ҳ'),
                                       lengthMenu = '��ʾ _MENU_ ����')
                                   
                      ))
        
        
    })
    #----------------------------------�������������----------------------
    output$table <- DT::renderDataTable({
        
        DT::datatable(data_1,
                      colnames=c("ʡ��","�ۼ�ȷ��","��������","��������","������(%)","������(%)"),
                      #caption="�й������������",
                      options=list(columnDefs=list(list(className = 'dt-center', targets = 0:6)),
                                   language = list(
                                       info = '',     #����ʾ���½ǵ���ʾ
                                       search = '����:',
                                       paginate = list(previous = '��ҳ', `next` = '��ҳ'),
                                       lengthMenu = '��ʾ _MENU_ ����'),aLengthMenu=c(10,20,34)
                                   
                                   
                      ))
        
        
    })
    
    output$map <- renderLeaflet({
        mapdata <- switch(input$radio,
                          "�ۼ�ȷ������"=shape$confirm,"�ִ�ȷ������"=shape$nowConfirm,
                          "�ۼƲ�������"=shape$dead,"�ۼ���������"=shape$heal)
        
        pal <- colorQuantile("Greens",mapdata)
        i_popup <- paste0("<strong>ʡ��:</strong>",shape$NAME,"<br>",
                          "<strong>����:</strong>",mapdata)  #�������������
        
        leaflet(shape)%>%addTiles()%>%setView(105.387021,35.969369,zoom=3)%>%
            addPolygons(fillColor=~pal(mapdata),fillOpacity=1,weight=1,
                        popup=i_popup)%>%
            addLegend(pal=pal,values=mapdata,position="bottomright")
        
    })
    
    ##----------------------------------������ΰ����----------------------
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
                      colnames=c("����","����ȷ��","�ۼ�ȷ��","����","��������","������","Ȭ������","Ȭ����"),
                      options=list(columnDefs=list(list(className = 'dt-center', targets = 0:6)),
                                   language = list(
                                       info = '',     #����ʾ���½ǵ���ʾ
                                       search = '����:',
                                       paginate = list(previous = '��ҳ', `next` = '��ҳ'),
                                       lengthMenu = '��ʾ _MENU_ ����')
                      ))
        
        
    })
}
shinyApp(ui,server)