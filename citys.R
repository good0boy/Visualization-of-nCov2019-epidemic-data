library(nCov2019)
y=load_nCov2019(lang = 'en')
citys<-function(chosed_country){
  nCov2019_set_country(country =chosed_country)
  ddd=y['province']
  province=ddd$province
  city=province[!duplicated(province)]
  return(city)
}

