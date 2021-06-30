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

names(ts01)
version2<-cbind(ts01 %>% select(key:dow),tt01,tt02,tt03,tt04,tt05,tt06,tt07,tt08,tt09,tt10,
                tt11,tt12,tt13,tt14,tt15,tt16,tt17,tt18,tt19,tt20,
                tt21,tt22,tt23,tt24,tt25,tt26,tt27,tt28,tt29,tt30,ts31 %>% select(mintemp:meanpress2))

# setwd("D:\\SNU\\기후보건영향평가\version2")
# write.csv(version2,file="version2.csv",row.names=F,na="")
