#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#library
pacman::p_load("dplyr","ggplot2","reshape2","sqldf","RColorBrewer","lubridate","lmtest","readxl","survival",
               "splines","data.table","stringr","tidyr","extrafont","scales","gridExtra","tsModel","mgcv",
               "gamm4")

#Time-series data

dd<-data.frame(date=rep(ymd(seq(as.Date("2001-01-01"),as.Date("2019-12-31"),1)),17),
               sido=rep(c(11,21:25,26,29,31:39),each=6939))
dd$key=paste0(dd$sido,"-",dd$date)

sido_df<-data.frame(sido=c(11,21:25,26,29,31:39),
                    KOR_SIDO=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북",
                               "전남","경북","경남","제주"),
                    EN_SIDO=c("seoul","busan","daegu","incheon","gwangju","daejeon","ulsan","sejong","ggyeonggi","gangwon","chungbuk","chungnam",
                              "jeonbuk","jeonnam","gyeongbuk","gyeongnam","jeju"))

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#기상청 원시자료 
me<-read_excel("D:\\서울대예방의학교실\\연구\\질병관리본부\\기후보건영향평가_평가체계구축및시범사업\\2021\\자료\\기상자료_대기오염_업데이트_일별_2000_2020_OJM.xlsx",sheet=4)
me$key=paste0(me$KOR_SIDO,"-",me$date)

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#version 1, 세부적으로 나눠서 각각 
#일일 건수, 시계열 자료 함수 
daily_count<-function(data){
  
  r<-sqldf("select date,sido, count(date) as Total,sum(sex_m) as sex_m,sum(sex_f) as sex_f,
           sum(ag01) as ag01,sum(ag02) as ag02,sum(ag03) as ag03,sum(ag04) as ag04,
           sum(ag05) as ag05,sum(ag06) as ag06,sum(ag07) as ag07,sum(ag08) as ag08,
           sum(ag09) as ag09,sum(ag10) as ag10,sum(ag11) as ag11,sum(ag12) as ag12,
           sum(ag13) as ag13,sum(ag14) as ag14,sum(ag15) as ag15,sum(ag16) as ag16,
           sum(ag17) as ag17,sum(ag18) as ag18,sum(ag19) as ag19,
           sum(ag01_m) as ag01_m,sum(ag02_m) as ag02_m,sum(ag03_m) as ag03_m,sum(ag04_m) as ag04_m,
           sum(ag05_m) as ag05_m,sum(ag06_m) as ag06_m,sum(ag07_m) as ag07_m,sum(ag08_m) as ag08_m,
           sum(ag09_m) as ag09_m,sum(ag10_m) as ag10_m,sum(ag11_m) as ag11_m,sum(ag12_m) as ag12_m,
           sum(ag13_m) as ag13_m,sum(ag14_m) as ag14_m,sum(ag15_m) as ag15_m,sum(ag16_m) as ag16_m,
           sum(ag17_m) as ag17_m,sum(ag18_m) as ag18_m,sum(ag19_m) as ag19_m,
           sum(ag01_f) as ag01_f,sum(ag02_f) as ag02_f,sum(ag03_f) as ag03_f,sum(ag04_f) as ag04_f,
           sum(ag05_f) as ag05_f,sum(ag06_f) as ag06_f,sum(ag07_f) as ag07_f,sum(ag08_f) as ag08_f,
           sum(ag09_f) as ag09_f,sum(ag10_f) as ag10_f,sum(ag11_f) as ag11_f,sum(ag12_f) as ag12_f,
           sum(ag13_f) as ag13_f,sum(ag14_f) as ag14_f,sum(ag15_f) as ag15_f,sum(ag16_f) as ag16_f,
           sum(ag17_f) as ag17_f,sum(ag18_f) as ag18_f,sum(ag19_f) as ag19_f from data group by date, sido") %>% arrange(date,sido)
  
  r$key=paste0(r$sido,"-",r$date)
  r<-r %>% select (-c(date,sido))
  
  zz<-merge(dd,r,by="key",all.x=T)
  
  zz[is.na(zz)]<-0
  zz$year=year(zz$date)
  zz$month=month(zz$date)
  zz$day=day(zz$date)
  zz$dow=weekdays(zz$date)
  
  zz2<-merge(zz,sido_df,by="sido",all.x=T)
  zz2$key=paste0(zz2$KOR_SIDO,"-",zz2$date)
  zz2<-zz2 %>% select(-date)
  zz3<-zz2 %>% left_join (me)
  zz3<-zz3 %>% dplyr:: select(key,date,sido,KOR_SIDO,EN_SIDO,year:dow,Total:ag19_f,mintemp:meanpress2) %>% arrange(sido);
  zz3}

ts01<-daily_count(z01);ts02<-daily_count(z02);ts03<-daily_count(z03);ts04<-daily_count(z04)
ts05<-daily_count(z05);ts06<-daily_count(z06);ts07<-daily_count(z07);ts08<-daily_count(z08)
ts09<-daily_count(z09);ts10<-daily_count(z10);ts11<-daily_count(z11);ts12<-daily_count(z12)
ts13<-daily_count(z13);ts14<-daily_count(z14);ts15<-daily_count(z15);ts16<-daily_count(z16)
ts17<-daily_count(z17);ts18<-daily_count(z18);ts19<-daily_count(z19);ts20<-daily_count(z20)
ts21<-daily_count(z21);ts22<-daily_count(z22);ts23<-daily_count(z23);ts24<-daily_count(z24)
ts25<-daily_count(z25);ts26<-daily_count(z26);ts27<-daily_count(z27);ts28<-daily_count(z28)
ts29<-daily_count(z29);ts30<-daily_count(z30);ts31<-daily_count(z31);

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#Save the results (01-31)
# write.csv(ts01,file="D:\\서울대예방의학교실\\연구\\질병관리본부\\기후보건영향평가_평가체계구축및시범사업\\2021\\자료rsion1",row.names=F,na="")

