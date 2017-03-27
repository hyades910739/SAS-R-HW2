library(ggmap);
library(mapproj);
library(jsonlite);
library(httr);
library(RCurl);
library(XML);
library(bitops);

#############################################Q1
##  ***REFERENCE***
##1.汙染指標(AQI): http://taqm.epa.gov.tw/taqm/tw/b0201.aspx
##2.data source: data.gov.tw/
##3. geoPoint is based on this : http://chienhung0304.blogspot.tw/2015/10/r-google-map.html


##read.data
data2 = read_json("air2.json",flatten=T);
data3 = data.frame(matrix(unlist(data2), nrow=1000, byrow=T),stringsAsFactors=FALSE);

##select PM2.5 and col needed
data4 = data3[data3[5]=="PM2.5", c(2,5,8)]
anyDuplicated(data4[1]);
n = length(data4[,1]);


##### 
geoPoint = function(address, verbose=FALSE) {
  if(verbose) cat(address,"\n")
  root = "http://maps.google.com/maps/api/geocode/"
  ##address = iconv(address,"big5","utf8")
  #POI API型態(XML or JSON)
  return.call = "xml"
  sensor = "false"
  #產生URL
  url_gen = paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  #擷取網頁原始碼
  html_code = xmlParse(url_gen)
  #若status為OK抓取資料, 若不為OK return status
  if(xpathSApply(html_code,"//GeocodeResponse//status",xmlValue)=="OK") {
    lat = xpathSApply(html_code,"//result//geometry//location//lat",xmlValue)
    lng = xpathSApply(html_code,"//result//geometry//location//lng",xmlValue)
    loc_type = xpathSApply(html_code,"//result//geometry//location_type",xmlValue)
    return(cbind(lat, lng, loc_type, address))
  } else {
    return(paste("Status:", xpathSApply(html_code,"//GeocodeResponse//status",xmlValue), sep = " "))
  }
}
#####

###get location
location = c(0,0,0,0); 
for(i in 1:n){
  curr.obs = data4[i,1]; 
  curr =geoPoint(curr.obs);
  if(length(curr)>4) curr= curr[1,];
  location = rbind(location,curr);
  Sys.sleep(1);
}
copy = location;
location = location[2:72,];
lat = as.numeric(location[,1]);
lng = as.numeric(location[,2]);
data5 = cbind(data4,lat,lng);

##create factor
pm2.5 = as.numeric(data5[,3]);
fac1 = rep(0,71);
fac1[pm2.5<=15.4] = "0.0~15.4:良好";
fac1[pm2.5>=15.5 & pm2.5<= 35.4] = "13.5~35.4 普通"
fac1[pm2.5>=35.5 & pm2.5<= 54.4] = "35.5~54.4 對敏感族群不健康"
data6 = cbind(data5,fac1);

### let's maping,(不是映射)
map = get_map(location = 'Taiwan',zoom= 8, language = "zh-TW", maptype = "roadmap");
m = ggmap(map,darken = 0.1) + geom_point(alpha=0.5,size=5,aes(x = data6[,5], y = data6[,4],color=data6$fac1), data = data6)+ scale_color_manual(values = c("green","yellow","orange"));
m+ labs(title = "2017年1月全國PM2.5觀測值",color="PM2.5指標(單位:微克/立方公尺)");


#################Q2

data02= mtcars;
data02$vs = as.factor(data02$vs);
data02$am = as.factor(data02$am);
reg1 = lm(mpg~.,data=data02);
summary(reg1);

