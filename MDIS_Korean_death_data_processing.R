##################################################
######단국대-기후보건영향평가 - 통계청 DB#########
##################################################

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#library
pacman::p_load("dplyr","ggplot2","reshape2","sqldf","RColorBrewer","lubridate","lmtest","readxl","survival",
               "splines","data.table","stringr","tidyr","extrafont","scales","gridExtra","tsModel","mgcv",
               "gamm4")

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#working directory
setwd("D:\\EUMC\\데이터관리\\통계청_MDIS\\보건복지\\사망원인통계\\사망연간자료\\A형")

dataset_label<-list.files()[grep("사망_연간자료",list.files())]

data.list=NULL
#원자료 변수 이름 동일, 이중 필요한 것만, 주소(시도), 성별, 사망연월일, 사망연령(각세),사망원인 1, 사망원인 2
for(i in 1:length(dataset_label)){
  data.list[[i]]<-read.csv(dataset_label[i]) %>% dplyr:: select("사망자.주소.시도.":"사망연월일","사망연령.각세.",
                                                                "사망원인1","사망원인2")
  names(data.list[[i]])=c("sido","sex","death_date","death_age","death1","death2")
  print(i)}

stat01_20<-as.data.frame(do.call(rbind,data.list));rm(data.list)

stat01_20$date =ymd(stat01_20$death_date)
stat01_20$year =year(stat01_20$date)
stat01_20$month=month(stat01_20$date)
stat01_20$day  =day(stat01_20$date)

nrow(stat01_20) #5,268,698

#시도, 사망연령, 사망원인 결측 검토 
table(is.na(stat01_20$sido))
table(is.na(stat01_20$death_age))
table(is.na(stat01_20$death1))

#사망연령은 "999"로 결측값 표기해서 확인 필요 
#missing value:999
summary(stat01_20$death_age)
addmargins(table(stat01_20$death_age))

subset(stat01_20,death_age==999)$year %>% table

stat01_20<-subset(stat01_20,death_age!=999)

nrow(stat01_20) #5,268,072
summary(stat01_20$death_age)

addmargins(substr(stat01_20$death1,1,1) %>% table)
addmargins(table(stat01_20$sido))
addmargins(table(stat01_20$sex))

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#연령
stat01_20$ag01=with(stat01_20,ifelse(death_age<5,1,0))
stat01_20$ag02=with(stat01_20,ifelse(death_age>=5 & death_age<10,1,0))
stat01_20$ag03=with(stat01_20,ifelse(death_age>=10 & death_age<15,1,0))
stat01_20$ag04=with(stat01_20,ifelse(death_age>=15 & death_age<20,1,0))
stat01_20$ag05=with(stat01_20,ifelse(death_age>=20 & death_age<25,1,0))
stat01_20$ag06=with(stat01_20,ifelse(death_age>=25 & death_age<30,1,0))
stat01_20$ag07=with(stat01_20,ifelse(death_age>=30 & death_age<35,1,0))
stat01_20$ag08=with(stat01_20,ifelse(death_age>=35 & death_age<40,1,0))
stat01_20$ag09=with(stat01_20,ifelse(death_age>=40 & death_age<45,1,0))
stat01_20$ag10=with(stat01_20,ifelse(death_age>=45 & death_age<50,1,0))
stat01_20$ag11=with(stat01_20,ifelse(death_age>=50 & death_age<55,1,0))
stat01_20$ag12=with(stat01_20,ifelse(death_age>=55 & death_age<60,1,0))
stat01_20$ag13=with(stat01_20,ifelse(death_age>=60 & death_age<65,1,0))
stat01_20$ag14=with(stat01_20,ifelse(death_age>=65 & death_age<70,1,0))
stat01_20$ag15=with(stat01_20,ifelse(death_age>=70 & death_age<75,1,0))
stat01_20$ag16=with(stat01_20,ifelse(death_age>=75 & death_age<80,1,0))
stat01_20$ag17=with(stat01_20,ifelse(death_age>=80 & death_age<85,1,0))
stat01_20$ag18=with(stat01_20,ifelse(death_age>=85 & death_age<90,1,0))
stat01_20$ag19=with(stat01_20,ifelse(death_age>=90,1,0))

#성별
stat01_20$sex_m=with(stat01_20,ifelse(sex==1,1,0))
stat01_20$sex_f=with(stat01_20,ifelse(sex==2,1,0))

#시도
stat01_20$sido01=with(stat01_20,ifelse(sido==11,1,0));stat01_20$sido02=with(stat01_20,ifelse(sido==21,1,0))
stat01_20$sido03=with(stat01_20,ifelse(sido==22,1,0));stat01_20$sido04=with(stat01_20,ifelse(sido==23,1,0))
stat01_20$sido05=with(stat01_20,ifelse(sido==24,1,0));stat01_20$sido06=with(stat01_20,ifelse(sido==25,1,0))
stat01_20$sido07=with(stat01_20,ifelse(sido==26,1,0));stat01_20$sido08=with(stat01_20,ifelse(sido==29,1,0))
stat01_20$sido09=with(stat01_20,ifelse(sido==31,1,0));stat01_20$sido10=with(stat01_20,ifelse(sido==32,1,0))
stat01_20$sido11=with(stat01_20,ifelse(sido==33,1,0));stat01_20$sido12=with(stat01_20,ifelse(sido==34,1,0))
stat01_20$sido13=with(stat01_20,ifelse(sido==35,1,0));stat01_20$sido14=with(stat01_20,ifelse(sido==36,1,0))
stat01_20$sido15=with(stat01_20,ifelse(sido==37,1,0));stat01_20$sido16=with(stat01_20,ifelse(sido==38,1,0))
stat01_20$sido17=with(stat01_20,ifelse(sido==39,1,0))

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#연령별 성별:남
stat01_20$ag01_m=with(stat01_20,ifelse(sex==1 & ag01==1,1,0));stat01_20$ag02_m=with(stat01_20,ifelse(sex==1 & ag02==1,1,0))
stat01_20$ag03_m=with(stat01_20,ifelse(sex==1 & ag03==1,1,0));stat01_20$ag04_m=with(stat01_20,ifelse(sex==1 & ag04==1,1,0))
stat01_20$ag05_m=with(stat01_20,ifelse(sex==1 & ag05==1,1,0));stat01_20$ag06_m=with(stat01_20,ifelse(sex==1 & ag06==1,1,0))
stat01_20$ag07_m=with(stat01_20,ifelse(sex==1 & ag07==1,1,0));stat01_20$ag08_m=with(stat01_20,ifelse(sex==1 & ag08==1,1,0))
stat01_20$ag09_m=with(stat01_20,ifelse(sex==1 & ag09==1,1,0));stat01_20$ag10_m=with(stat01_20,ifelse(sex==1 & ag10==1,1,0))
stat01_20$ag11_m=with(stat01_20,ifelse(sex==1 & ag11==1,1,0));stat01_20$ag12_m=with(stat01_20,ifelse(sex==1 & ag12==1,1,0))
stat01_20$ag13_m=with(stat01_20,ifelse(sex==1 & ag13==1,1,0));stat01_20$ag14_m=with(stat01_20,ifelse(sex==1 & ag14==1,1,0))
stat01_20$ag15_m=with(stat01_20,ifelse(sex==1 & ag15==1,1,0));stat01_20$ag16_m=with(stat01_20,ifelse(sex==1 & ag16==1,1,0))
stat01_20$ag17_m=with(stat01_20,ifelse(sex==1 & ag17==1,1,0));stat01_20$ag18_m=with(stat01_20,ifelse(sex==1 & ag18==1,1,0))
stat01_20$ag19_m=with(stat01_20,ifelse(sex==1 & ag19==1,1,0))

#연령별 성별:여
stat01_20$ag01_f=with(stat01_20,ifelse(sex==2 & ag01==1,1,0));stat01_20$ag02_f=with(stat01_20,ifelse(sex==2 & ag02==1,1,0))
stat01_20$ag03_f=with(stat01_20,ifelse(sex==2 & ag03==1,1,0));stat01_20$ag04_f=with(stat01_20,ifelse(sex==2 & ag04==1,1,0))
stat01_20$ag05_f=with(stat01_20,ifelse(sex==2 & ag05==1,1,0));stat01_20$ag06_f=with(stat01_20,ifelse(sex==2 & ag06==1,1,0))
stat01_20$ag07_f=with(stat01_20,ifelse(sex==2 & ag07==1,1,0));stat01_20$ag08_f=with(stat01_20,ifelse(sex==2 & ag08==1,1,0))
stat01_20$ag09_f=with(stat01_20,ifelse(sex==2 & ag09==1,1,0));stat01_20$ag10_f=with(stat01_20,ifelse(sex==2 & ag10==1,1,0))
stat01_20$ag11_f=with(stat01_20,ifelse(sex==2 & ag11==1,1,0));stat01_20$ag12_f=with(stat01_20,ifelse(sex==2 & ag12==1,1,0))
stat01_20$ag13_f=with(stat01_20,ifelse(sex==2 & ag13==1,1,0));stat01_20$ag14_f=with(stat01_20,ifelse(sex==2 & ag14==1,1,0))
stat01_20$ag15_f=with(stat01_20,ifelse(sex==2 & ag15==1,1,0));stat01_20$ag16_f=with(stat01_20,ifelse(sex==2 & ag16==1,1,0))
stat01_20$ag17_f=with(stat01_20,ifelse(sex==2 & ag17==1,1,0));stat01_20$ag18_f=with(stat01_20,ifelse(sex==2 & ag18==1,1,0))
stat01_20$ag19_f=with(stat01_20,ifelse(sex==2 & ag19==1,1,0))

#검토용
apply(stat01_20 %>%  dplyr::select(ag01:ag19),2,sum) %>% sum

#사망원인 검토, 사망원인 2는 V, W, X, Y 등임
table(substr(stat01_20$death2,1,1))

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#연도별-지역별  질환 자료 집계 

yr=data.frame(year=2001:2020)
label=c("전체","연령","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54",
        "55-59","60-64","65-69","70-74","75-79","80-84","85-90","90+",
        "성별","남","여",
        "지역","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북",
        "전남","경북","경남","제주")

#집계 테이블, 함수
agg_tb<-function(data){
  d.list=NULL
  for(i in 1:20){
    
    d<-subset(data,year==yr$year[i])
    d.list[[i]]<-data.frame(count=apply(d %>% dplyr:: select(ag01:sido17),2,sum))}
  agg_df<-as.data.frame(do.call(cbind,d.list))
  names(agg_df)=paste0(2001:2020)
  
  agg_df$Total=apply(agg_df %>% dplyr:: select(`2001`:`2020`),1,sum)
  
  year.tot<-apply(agg_df[1:19,] %>% dplyr:: select(`2001`:`2020`),2,sum)
  tot<-sum(year.tot)
  
  agg_df<-agg_df %>% select(Total,`2001`:`2020`)
  agg_df.r<-rbind(c(tot,year.tot),NA,agg_df[1:19,],NA,agg_df[20:21,],NA,agg_df[22:38,])
  row.names(agg_df.r)=label
  agg_df.r}

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#질환별로 정리 
#전체 감염병 질환 (A00-A09)
z01<-subset(stat01_20,substr(death1,1,3) %in% c("A00","A01","A02","A03","A04","A05","A06","A07","A08","A09"))

#전체 심뇌혈관 질환 (I00-I99)
z02<-subset(stat01_20,substr(death1,1,1) %in% c("I"))

#협심증 (I20)
z03<-subset(stat01_20,substr(death1,1,3) %in% c("I20"))

#심근경색 (I21-I25)
z04<-subset(stat01_20,substr(death1,1,3) %in% c("I21","I22","I23","I24","I25"))

#전체 허혈성 심질환 (I20-I25)
z05<-subset(stat01_20,substr(death1,1,3) %in% c("I20","I21","I22","I23","I24","I25"))

#전체 뇌졸중 (I60-I67, I690-I694,G458-G459)
z06<-rbind(subset(stat01_20,substr(death1,1,3) %in% c("I60","I61","I62","I63","I64","I65","I66","I67")),
           subset(stat01_20,substr(death1,1,4) %in% c("I690","I691","I692","I693","I694","G458","G459")))

#전체 호흡기 (J00-J99)
z07<-subset(stat01_20,substr(death1,1,1) %in% c("J"))

#급성 상기도 감염 (J00-J06)
z08<-subset(stat01_20,substr(death1,1,3) %in% c("J00","J01","J02","J03","J04","J05","J06"))

#인플루엔자 및 폐렴 (J09-J18)
z09<-subset(stat01_20,substr(death1,1,3) %in% c("J09","J10","J11","J12","J13","J14","J15","J16","J17","J18"))

#천식 (J45-46)
z10<-subset(stat01_20,substr(death1,1,3) %in% c("J45","J46"))

#용적 고갈 탈수 (E86)
z11<-subset(stat01_20,substr(death1,1,3) %in% c("E86"))

#고온 관련질환 (열사병, 열피로) (T67)
z12<-subset(stat01_20,substr(death1,1,3) %in% c("T67"))

#온열질환 (E86, T67)
z13<-subset(stat01_20,substr(death1,1,3) %in% c("E86","T67"))

#동상 (T33-T35)
z14<-subset(stat01_20,substr(death1,1,3) %in% c("T33","T34","T35"))

#저체온증 (T68)
z15<-subset(stat01_20,substr(death1,1,3) %in% c("T68"))

#비동결 및 기타 (T69)
z16<-subset(stat01_20,substr(death1,1,3) %in% c("T69"))

#한랭질환 (T33-T35,T68,T69)
z17<-subset(stat01_20,substr(death1,1,3) %in% c("T33","T34","T35","T68","T69"))

#급성 신부전증 (N17)
z18<-subset(stat01_20,substr(death1,1,3) %in% c("N17"))

#자살 (X60-X84)
z19<-subset(stat01_20,substr(death2,1,3) %in% c(paste0("X",60:84)))

#우울증 (F31-F39)
z20<-subset(stat01_20,substr(death1,1,3) %in% c("F31","F32","F33","F34","F35","F36","F37","F38","F39"))

#추가한 부분
#전체 원인 사망 (A00-Z99)
z21<-stat01_20

#비사고사 사망  (A00-R99)
z22<-subset(stat01_20,!substr(death1,1,1) %in% c("S","T",'U'))

#출혈성 뇌졸중  (I60-I62, I690-I692)
z23<-rbind(subset(stat01_20,substr(death1,1,3) %in% c("I60","I61","I62")),
           subset(stat01_20,substr(death1,1,4) %in% c("I690","I691","I692")))

#허혈성 뇌졸중  (I63,I65-I67, I693)
z24<-rbind(subset(stat01_20,substr(death1,1,3) %in% c("I63","I65","I66","I67")),
           subset(stat01_20,substr(death1,1,4) %in% c("I693")))

#기타 뇌졸중  (I64, I694)
z25<-rbind(subset(stat01_20,substr(death1,1,3) %in% c("I64")),
           subset(stat01_20,substr(death1,1,4) %in% c("I694")))

#일과성 뇌허혈질환 (G458-G459)
z26<-subset(stat01_20,substr(death1,1,4) %in% c("G458","G459"))

#하부호흡기질환 (J20-J22)
z27<-subset(stat01_20,substr(death1,1,3) %in% c("J20","J21","J22"))

#만성폐쇄성폐질환 (J40-J44)
z28<-subset(stat01_20,substr(death1,1,3) %in% c(paste0("J",40:44)))

#당뇨 (E10-E14)
z29<-subset(stat01_20,substr(death1,1,3) %in% c(paste0("E",10:14)))

#신장질환 (N00-N29)
k=00:29 ; ifelse(k<10,paste0(0,k),k)
z30<-subset(stat01_20,substr(death1,1,3) %in% c(paste0("N",ifelse(k<10,paste0(0,k),k))))

#사고사 (S00-S99, T00-T99, U00-U99)
z31<-subset(stat01_20,substr(death1,1,1) %in% c("S","T","U"))

#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
out01<-agg_tb(z01);out02<-agg_tb(z02);out03<-agg_tb(z03);out04<-agg_tb(z04)
out05<-agg_tb(z05);out06<-agg_tb(z06);out07<-agg_tb(z07);out08<-agg_tb(z08)
out09<-agg_tb(z09);out10<-agg_tb(z10);out11<-agg_tb(z11);out12<-agg_tb(z12)
out13<-agg_tb(z13);out14<-agg_tb(z14);out15<-agg_tb(z15);out16<-agg_tb(z16)
out17<-agg_tb(z17);out18<-agg_tb(z18);out19<-agg_tb(z19);out20<-agg_tb(z20)
out21<-agg_tb(z21);out22<-agg_tb(z22);out23<-agg_tb(z23);out24<-agg_tb(z24)
out25<-agg_tb(z25);out26<-agg_tb(z26);out27<-agg_tb(z27);out28<-agg_tb(z28)
out29<-agg_tb(z29);out30<-agg_tb(z30);out31<-agg_tb(z31)

#Save the resutls (01-30)
setwd("D:\\SNU\\연구\\질병관리본부\\기후보건영향평가_평가체계구축및시범사업\\2022\\자료\\통계청\\summary")
write.csv(out01,file="out01.csv",row.names=T,na="")
write.csv(out02,file="out02.csv",row.names=T,na="")
write.csv(out03,file="out03.csv",row.names=T,na="")
write.csv(out04,file="out04.csv",row.names=T,na="")
write.csv(out05,file="out05.csv",row.names=T,na="")
write.csv(out06,file="out06.csv",row.names=T,na="")
write.csv(out07,file="out07.csv",row.names=T,na="")
write.csv(out08,file="out08.csv",row.names=T,na="")
write.csv(out09,file="out09.csv",row.names=T,na="")
write.csv(out10,file="out10.csv",row.names=T,na="")
write.csv(out11,file="out11.csv",row.names=T,na="")
write.csv(out12,file="out12.csv",row.names=T,na="")
write.csv(out13,file="out13.csv",row.names=T,na="")
write.csv(out14,file="out14.csv",row.names=T,na="")
write.csv(out15,file="out15.csv",row.names=T,na="")
write.csv(out16,file="out16.csv",row.names=T,na="")
write.csv(out17,file="out17.csv",row.names=T,na="")
write.csv(out18,file="out18.csv",row.names=T,na="")
write.csv(out19,file="out19.csv",row.names=T,na="")
write.csv(out20,file="out20.csv",row.names=T,na="")
write.csv(out21,file="out21.csv",row.names=T,na="")
write.csv(out22,file="out22.csv",row.names=T,na="")
write.csv(out23,file="out23.csv",row.names=T,na="")
write.csv(out24,file="out24.csv",row.names=T,na="")
write.csv(out25,file="out25.csv",row.names=T,na="")
write.csv(out26,file="out26.csv",row.names=T,na="")
write.csv(out27,file="out27.csv",row.names=T,na="")
write.csv(out28,file="out28.csv",row.names=T,na="")
write.csv(out29,file="out29.csv",row.names=T,na="")
write.csv(out30,file="out30.csv",row.names=T,na="")
write.csv(out31,file="out31.csv",row.names=T,na="")
