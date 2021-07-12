libname a 'D:\EUMC\데이터관리\통계청_MDIS\보건복지\사망원인통계\사망연간자료\A형';

/****************************************************/
/*data 저장 위치 이름 지정해서 가져오기, CSV만  */
data a1;
keep fname;
rc=filename("mydir","D:\EUMC\데이터관리\통계청_MDIS\보건복지\사망원인통계\사망연간자료\A형");
did=dopen("mydir");
nf=dnum(did);

do i=1 to nf;
fname=dread(did,i);
if index (upcase(fname),"CSV")>0 then output;
end;
rc=dclose(did);
run;

data _null_; set a1; call symput("n", strip(_n_)); run;

/****************************************************/
/*_null_ : 데이터 셋 만들지 말라*/
/*_n_ 자동변수 */
/*call symput: 직접 매크로 변수를 입력해서 설정*/
/*매크로 이용해서 csv파일 일괄 불러오기 */
%macro importFile;
%do i=1 %to &n;
data _null_; set a1; 
if _n_=&i; 
call symput('file',trim(cats('D:\EUMC\데이터관리\통계청_MDIS\보건복지\사망원인통계\사망연간자료\A형\',fname,"")));run;
proc import out=z&i datafile="&file" DBMS=CSV;  RUN;
%end;
%mend;

%importFile;

/****************************************************/
/*자료 merge */
data a.stat01_19; set z1-z19; run;

/****************************************************/
/* 각 개별 변수
v1: 신고일자 (년)
v2: 신고일자 (월)
v3: 신고일자 (일)
v4: 사망자 주소
v5: 성별
v6: 사망연원일
v7: 사망시간
v8: 사망연령(각 세)
v9: 사망장소
v10: 사망직업
v11: 혼인상태
v12: 교육정도
v13: 사망원인1
v14: 사망원인2
v15: 사망원인 분류 항목
v16: 사망원인 분류 항목 (2) 
*/
/****************************************************/
/*필요한 변수만 추출 */
data a.stat01_19_r ; set a.stat01_19;
keep var4 var5 var6 var8 var13 var14; run;

/****************************************************/
/* 시도 변수 
11	서울특별시
21	부산광역시
22	대구광역시
23	인천광역시
24	광주광역시
25	대전광역시
26	울산광역시
29	세종특별자치시
31	경기도
32	강원도
33	충청북도
34	충청남도
35	전라북도
36	전라남도
37	경삭북도
38	경상남도
39	제주특별자치도
*/
proc freq data= a.stat01_19_r; tables var4; run;

/*결측제외: 연령 999 제외*/
proc means data=a.stat01_19_r; var var8; run;

/****************************************************/
data a.stat01_19_r; set a.stat01_19_r;
dd=compress(var6);
year=substr(dd,1,4);
month=substr(dd,5,2);
day=substr(dd,7,2);
date=compress(year)||"-"||compress(month)||"-"||compress(day);

/*사망원인1 두자리만  (여기서 S,T 코드 확인, 손상, 중독 및 외인에 의한 결과)*/
death1=substr(var13,1,3);
/*사망원인2 V,X,Y,Z (사고, 특수목적, 가해 등, 건강영향 관련없는 것들) */
death2=substr(var14,1,3);

d1=substr(var13,1,1);
d2=substr(var14,1,1);

if var8 not in (999);

/*연령 분류 */
if var8 <15 then ag1=1; else  ag1=0;
if var8 >=15 & var8<65 then ag2=1; else ag2=0;
if var8 >=65 then ag3=1; else ag3=0;

/*성별 & 15세 미만 연령군 분류 */
if var5=1 & ag1=1 then ag1_m=1; else ag1_m=0;
if var5=2 & ag1=1 then ag1_f=1;  else ag1_f=0;

/*성별 & 15-64세 이상 연령군 분류*/
if var5=1 & ag2=1 then ag2_m=1; else ag2_m=0;
if var5=2 & ag2=1 then ag2_f=1;  else ag2_f=0;

/*성별 & 65세 이상 연령군 분류*/
if var5=1 & ag3=1 then ag3_m=1; else ag3_m=0;
if var5=2 & ag3=1 then ag3_f=1;  else ag3_f=0;

KEY=COMPRESS(VAR4)||"-"||COMPRESS(DATE);
drop dd; run;

proc means data=a.stat01_19_r; var var8; run;

/*사망원인1 코드 체크*/
proc freq data=a.stat01_19_r; tables d1/list; run;

/*사망원인2 코드 체크 */
proc freq data=a.stat01_19_r; tables d2/list; run;

/*S,T,U, V, X,Y,Z 코드 있는 경우 제외하기 */

DATA A.STAT01_19_R2; SET A.STAT01_19_R;
IF D1 NOT IN ("S","T","U");
IF D2 NOT IN ("V","X","Y","Z"); RUN;

/*Check*/
proc freq data=a.stat01_19_r2; tables d1/list; run;
proc freq data=a.stat01_19_r2; tables d2/list; run;

/****************************************************/
/*Daily count data, all-cause*/

PROC SQL;
CREATE TABLE A.DAILY_COUNT1 AS SELECT KEY, COUNT(DATE) AS ALL_CAUSE, SUM(AG1) AS AG0014 , SUM(AG2) AS AG1564, SUM(AG3) AS AG65,
SUM(AG1_M) AS AG0014_M, SUM(AG1_F) AS AG0014_F, SUM(AG2_M) AS AG1564_M, SUM(AG2_F) AS AG1564_F, SUM(AG3_M) AS AG65_M, SUM(AG3_F) AS AG65_F FROM A.STAT01_19_R2 
GROUP BY KEY ; QUIT;

DATA A.DAILY_COUNT1; SET A.DAILY_COUNT1;
/*전체 원인 사망  남녀 */
ALL_CAUSE_M=AG0014_M+AG1564_M+AG65_M;
ALL_CAUSE_F=AG0014_F+AG1564_F+AG65_F; 
SIDO   =COMPRESS(SUBSTR(KEY,1,2)," ");
DATE   =SUBSTR(TRIM(KEY),4,10);
YEAR   =SUBSTR(KEY,4,4);
MONTH=SUBSTR(KEY,9,2);
DAY     =SUBSTR(KEY,12,2);
RUN;

/****************************************************/
/*특정원인 사망 Daily count 대분류 */
%macro Daily_count(label,out);

data z; set a.stat01_19_r2;
if d1=&label.;

PROC SQL;
CREATE TABLE A.&out. AS SELECT KEY, COUNT(DATE) AS outcome, SUM(AG1) AS AG0014 , SUM(AG2) AS AG1564, SUM(AG3) AS AG65,
SUM(AG1_M) AS AG0014_M, SUM(AG1_F) AS AG0014_F, SUM(AG2_M) AS AG1564_M, SUM(AG2_F) AS AG1564_F, SUM(AG3_M) AS AG65_M, SUM(AG3_F) AS AG65_F FROM z 
GROUP BY KEY ; QUIT;

DATA A.&out.; SET a.&out.;
/*전체 원인 사망  남녀 */
OUTCOME_M=AG0014_M+AG1564_M+AG65_M;
OUTCOME_F=AG0014_F+AG1564_F+AG65_F; 
SIDO   =COMPRESS(SUBSTR(KEY,1,2)," ");
DATE   =SUBSTR(TRIM(KEY),4,10);
YEAR   =SUBSTR(KEY,4,4);
MONTH=SUBSTR(KEY,9,2);
DAY     =SUBSTR(KEY,12,2);
RUN;
%mend;
/****************************************************/

%Daily_count("I",Daily_count2); /*전체 심혈관*/
%Daily_count("J",Daily_count3); /*전체 호흡기*/
%Daily_count("C",Daily_count4); /*전체 암*/

/****************************************************/
