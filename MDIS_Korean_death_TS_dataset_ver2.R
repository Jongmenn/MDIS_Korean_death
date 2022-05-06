#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#version 2, 통합하기
#세부적으로 나누지 않고, 전체, 연령 (15세 미만, 15-64,75세 이상), 연령 (남/여) 이렇게만 
#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#

daily_count2<-function(data){
  z<-data %>% dplyr:: mutate(tot=Total,m=sex_m,f=sex_f,
                             age0005=ag01,
                             age0014=ag01+ag02+ag03,
                             age1564=ag04+ag05+ag06+ag07+ag08+ag09+ag10+ag11+ag12+ag13,
                             age65  =ag14+ag15+ag16+ag17+ag18+ag19) %>%  select(tot,m,f,age0005,age0014,age1564,age65)
  tibble(z)}

tt01<-daily_count2(ts01);names(tt01)=paste0("D01_",names(tt01))
tt02<-daily_count2(ts02);names(tt02)=paste0("D02_",names(tt02))
tt03<-daily_count2(ts03);names(tt03)=paste0("D03_",names(tt03))
tt04<-daily_count2(ts04);names(tt04)=paste0("D04_",names(tt04))
tt05<-daily_count2(ts05);names(tt05)=paste0("D05_",names(tt05))
tt06<-daily_count2(ts06);names(tt06)=paste0("D06_",names(tt06))
tt07<-daily_count2(ts07);names(tt07)=paste0("D07_",names(tt07))
tt08<-daily_count2(ts08);names(tt08)=paste0("D08_",names(tt08))
tt09<-daily_count2(ts09);names(tt09)=paste0("D09_",names(tt09))
tt10<-daily_count2(ts10);names(tt10)=paste0("D10_",names(tt10))
tt11<-daily_count2(ts11);names(tt11)=paste0("D11_",names(tt11))
tt12<-daily_count2(ts12);names(tt12)=paste0("D12_",names(tt12))
tt13<-daily_count2(ts13);names(tt13)=paste0("D13_",names(tt13))
tt14<-daily_count2(ts14);names(tt14)=paste0("D14_",names(tt14))
tt15<-daily_count2(ts15);names(tt15)=paste0("D15_",names(tt15))
tt16<-daily_count2(ts16);names(tt16)=paste0("D16_",names(tt16))
tt17<-daily_count2(ts17);names(tt17)=paste0("D17_",names(tt17))
tt18<-daily_count2(ts18);names(tt18)=paste0("D18_",names(tt18))
tt19<-daily_count2(ts19);names(tt19)=paste0("D19_",names(tt19))
tt20<-daily_count2(ts20);names(tt20)=paste0("D20_",names(tt20))
tt21<-daily_count2(ts21);names(tt21)=paste0("D21_",names(tt21))
tt22<-daily_count2(ts22);names(tt22)=paste0("D22_",names(tt22))
tt23<-daily_count2(ts23);names(tt23)=paste0("D23_",names(tt23))
tt24<-daily_count2(ts24);names(tt24)=paste0("D24_",names(tt24))
tt25<-daily_count2(ts25);names(tt25)=paste0("D25_",names(tt25))
tt26<-daily_count2(ts26);names(tt26)=paste0("D26_",names(tt26))
tt27<-daily_count2(ts27);names(tt27)=paste0("D27_",names(tt27))
tt28<-daily_count2(ts28);names(tt28)=paste0("D28_",names(tt28))
tt29<-daily_count2(ts29);names(tt29)=paste0("D29_",names(tt29))
tt30<-daily_count2(ts30);names(tt30)=paste0("D30_",names(tt30))
tt31<-daily_count2(ts31);names(tt31)=paste0("D31_",names(tt31))

names(tt01)=gsub("D01","intestinal_infec",names(tt01))
names(tt02)=gsub("D02","circ"            ,names(tt02))
names(tt03)=gsub("D03","angina"          ,names(tt03))
names(tt04)=gsub("D04","MI"              ,names(tt04))
names(tt05)=gsub("D05","ischHD"          ,names(tt05))
names(tt06)=gsub("D06","cerebvas"        ,names(tt06))
names(tt07)=gsub("D07","resp"            ,names(tt07))
names(tt08)=gsub("D08","acuteup"         ,names(tt08))
names(tt09)=gsub("D09","pneum"           ,names(tt09))
names(tt10)=gsub("D10","asthma"          ,names(tt10))
names(tt11)=gsub("D11","voldep"          ,names(tt11))
names(tt12)=gsub("D12","heat"            ,names(tt12))
names(tt13)=gsub("D13","heatrelated"     ,names(tt13))
names(tt14)=gsub("D14","frost"           ,names(tt14))
names(tt15)=gsub("D15","hypothermia"     ,names(tt15))
names(tt16)=gsub("D16","otherredutemp"   ,names(tt16))
names(tt17)=gsub("D17","frostrelated"    ,names(tt17))
names(tt18)=gsub("D18","aki"             ,names(tt18))
names(tt19)=gsub("D19","suicide"         ,names(tt19))
names(tt20)=gsub("D20","mental"          ,names(tt20))
names(tt21)=gsub("D21","all"             ,names(tt21))
names(tt22)=gsub("D22","nonacc"          ,names(tt22))
names(tt23)=gsub("D23","hemoStroke"      ,names(tt23))
names(tt24)=gsub("D24","ischStroke"      ,names(tt24))
names(tt25)=gsub("D25","otherStroke"     ,names(tt25))
names(tt26)=gsub("D26","TIA"             ,names(tt26))
names(tt27)=gsub("D27","acutelow"        ,names(tt27))
names(tt28)=gsub("D28","copd"            ,names(tt28))
names(tt29)=gsub("D29","dm"              ,names(tt29))
names(tt30)=gsub("D30","kidney"          ,names(tt30))
names(tt31)=gsub("D31","accd"            ,names(tt31))

names(version2)

version2<-cbind(ts01 %>% select(key,year:dow),tt01,tt02,tt03,tt04,tt05,tt06,tt07,tt08,tt09,tt10,
                tt11,tt12,tt13,tt14,tt15,tt16,tt17,tt18,tt19,tt20,
                tt21,tt22,tt23,tt24,tt25,tt26,tt27,tt28,tt29,tt30,tt31,ts31 %>% select(mintemp:pm25_model))

sido_df<-data.frame(sido=c(11,21:25,26,29,31:39),
                    area=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북",
                               "전남","경북","경남","제주"),
                    EN_SIDO=c("seoul","busan","daegu","incheon","gwangju","daejeon","ulsan","sejong","ggyeonggi","gangwon","chungbuk","chungnam",
                              "jeonbuk","jeonnam","gyeongbuk","gyeongnam","jeju"))

version2$area=substr(version2$key,1,2)
version2<-version2 %>% left_join(sido_df,by="area")
head(version2)
version2$ddate=ymd(substr(version2$key,4,13))

#serial number
version2$ddd=as.numeric(as.factor(version2$ddate))

version2<-version2 %>% select(ddate,year,month,day,ddd,sido,dow,area,EN_SIDO,
                              all_tot             ,all_age0005             ,all_age0014             ,all_age1564             ,all_age65,
                              nonacc_tot          ,nonacc_age0005          ,nonacc_age0014          ,nonacc_age1564          ,nonacc_age65,
                              circ_tot            ,circ_age0005            ,circ_age0014            ,circ_age1564            ,circ_age65,
                              resp_tot            ,resp_age0005            ,resp_age0014            ,resp_age1564            ,resp_age65,
                              ischHD_tot          ,ischHD_age0005          ,ischHD_age0014          ,ischHD_age1564          ,ischHD_age65,
                              cerebvas_tot        ,cerebvas_age0005        ,cerebvas_age0014        ,cerebvas_age1564        ,cerebvas_age65,
                              angina_tot          ,angina_age0005          ,angina_age0014          ,angina_age1564          ,angina_age65,
                              MI_tot              ,MI_age0005              ,MI_age0014              ,MI_age1564              ,MI_age65,
                              hemoStroke_tot      ,hemoStroke_age0005      ,hemoStroke_age0014      ,hemoStroke_age1564      ,hemoStroke_age65,
                              ischStroke_tot      ,ischStroke_age0005      ,ischStroke_age0014      ,ischStroke_age1564      ,ischStroke_age65,
                              otherStroke_tot     ,otherStroke_age0005     ,otherStroke_age0014     ,otherStroke_age1564     ,otherStroke_age65,
                              TIA_tot             ,TIA_age0005             ,TIA_age0014             ,TIA_age1564             ,TIA_age65,
                              pneum_tot           ,pneum_age0005           ,pneum_age0014           ,pneum_age1564           ,pneum_age65,
                              acutelow_tot        ,acutelow_age0005        ,acutelow_age0014        ,acutelow_age1564        ,acutelow_age65,
                              asthma_tot          ,asthma_age0005          ,asthma_age0014          ,asthma_age1564          ,asthma_age65,
                              copd_tot            ,copd_age0005            ,copd_age0014            ,copd_age1564            ,copd_age65,
                              acuteup_tot         ,acuteup_age0005         ,acuteup_age0014         ,acuteup_age1564         ,acuteup_age65,
                              heat_tot            ,heat_age0005            ,heat_age0014            ,heat_age1564            ,heat_age65,
                              voldep_tot          ,voldep_age0005          ,voldep_age0014          ,voldep_age1564          ,voldep_age65,
                              heatrelated_tot     ,heatrelated_age0005     ,heatrelated_age0014     ,heatrelated_age1564     ,heatrelated_age65,
                              frost_tot           ,frost_age0005           ,frost_age0014           ,frost_age1564           ,frost_age65,
                              hypothermia_tot     ,hypothermia_age0005     ,hypothermia_age0014     ,hypothermia_age1564     ,hypothermia_age65,
                              otherredutemp_tot   ,otherredutemp_age0005   ,otherredutemp_age0014   ,otherredutemp_age1564   ,otherredutemp_age65,
                              frostrelated_tot    ,frostrelated_age0005    ,frostrelated_age0014    ,frostrelated_age1564    ,frostrelated_age65,
                              aki_tot             ,aki_age0005             ,aki_age0014             ,aki_age1564             ,aki_age65,
                              dm_tot              ,dm_age0005              ,dm_age0014              ,dm_age1564              ,dm_age65,
                              kidney_tot          ,kidney_age0005          ,kidney_age0014          ,kidney_age1564          ,kidney_age65,
                              accd_tot            ,accd_age0005            ,accd_age0014            ,accd_age1564            ,accd_age65,
                              suicide_tot         ,suicide_age0005         ,suicide_age0014         ,suicide_age1564         ,suicide_age65,
                              mental_tot          ,mental_age0005          ,mental_age0014          ,mental_age1564          ,mental_age65,
                              intestinal_infec_tot,intestinal_infec_age0005,intestinal_infec_age0014,intestinal_infec_age1564,intestinal_infec_age65,
                    
                    all_m    ,nonacc_m,circ_m,resp_m,ischHD_m,
                    cerebvas_m,angina_m,MI_m,hemoStroke_m,ischStroke_m,otherStroke_m,
                    TIA_m,pneum_m,acutelow_m,asthma_m,copd_m,
                    acuteup_m,heat_m,voldep_m,heatrelated_m,frost_m,hypothermia_m,
                    otherredutemp_m,frostrelated_m,aki_m,dm_m,kidney_m,accd_m,suicide_m,mental_m,intestinal_infec_m,
                    all_f    ,nonacc_f,circ_f,resp_f,ischHD_f,
                    cerebvas_f,angina_f,MI_f,hemoStroke_f,ischStroke_f,otherStroke_f,
                    TIA_f,pneum_f,acutelow_f,asthma_f,copd_f,
                    acuteup_f,heat_f,voldep_f,heatrelated_f,frost_f,hypothermia_f,
                    otherredutemp_f,frostrelated_f,aki_f,dm_f,kidney_f,accd_f,suicide_f,mental_f,intestinal_infec_f,
                    
                    mintemp:pm25_model)

names(version2)[238:242]=c("pm25","pm10","so2","no2","co")

setwd("D:\\SNU\\연구\\질병관리본부\\기후보건영향평가_평가체계구축및시범사업\\2022\\자료\\통계청\\version2")
write.csv(version2,file="version2.csv",row.names=F,na="")
