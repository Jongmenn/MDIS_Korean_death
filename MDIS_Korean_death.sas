libname a 'D:\EUMC\�����Ͱ���\���û_MDIS\���Ǻ���\����������\��������ڷ�\A��';

/****************************************************/
/*data ���� ��ġ �̸� �����ؼ� ��������, CSV��  */
data a1;
keep fname;
rc=filename("mydir","D:\EUMC\�����Ͱ���\���û_MDIS\���Ǻ���\����������\��������ڷ�\A��");
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
/*_null_ : ������ �� ������ ����*/
/*_n_ �ڵ����� */
/*call symput: ���� ��ũ�� ������ �Է��ؼ� ����*/
/*��ũ�� �̿��ؼ� csv���� �ϰ� �ҷ����� */
%macro importFile;
%do i=1 %to &n;
data _null_; set a1; 
if _n_=&i; 
call symput('file',trim(cats('D:\EUMC\�����Ͱ���\���û_MDIS\���Ǻ���\����������\��������ڷ�\A��\',fname,"")));run;
proc import out=z&i datafile="&file" DBMS=CSV;  RUN;
%end;
%mend;

%importFile;

/****************************************************/
/*�ڷ� merge */
data a.stat01_19; set z1-z19; run;

/****************************************************/
/* �� ���� ����
v1: �Ű����� (��)
v2: �Ű����� (��)
v3: �Ű����� (��)
v4: ����� �ּ�
v5: ����
v6: ���������
v7: ����ð�
v8: �������(�� ��)
v9: ������
v10: �������
v11: ȥ�λ���
v12: ��������
v13: �������1
v14: �������2
v15: ������� �з� �׸�
v16: ������� �з� �׸� (2) 
*/
/****************************************************/
/*�ʿ��� ������ ���� */
data a.stat01_19_r ; set a.stat01_19;
keep var4 var5 var6 var8 var13 var14; run;

/****************************************************/
/* �õ� ���� 
11	����Ư����
21	�λ걤����
22	�뱸������
23	��õ������
24	���ֱ�����
25	����������
26	��걤����
29	����Ư����ġ��
31	��⵵
32	������
33	��û�ϵ�
34	��û����
35	����ϵ�
36	���󳲵�
37	���ϵ�
38	��󳲵�
39	����Ư����ġ��
*/
proc freq data= a.stat01_19_r; tables var4; run;

/*��������: ���� 999 ����*/
proc means data=a.stat01_19_r; var var8; run;

/****************************************************/
data a.stat01_19_r; set a.stat01_19_r;
dd=compress(var6);
year=substr(dd,1,4);
month=substr(dd,5,2);
day=substr(dd,7,2);
date=compress(year)||"-"||compress(month)||"-"||compress(day);

/*�������1 ���ڸ���  (���⼭ S,T �ڵ� Ȯ��, �ջ�, �ߵ� �� ���ο� ���� ���)*/
death1=substr(var13,1,3);
/*�������2 V,X,Y,Z (���, Ư������, ���� ��, �ǰ����� ���þ��� �͵�) */
death2=substr(var14,1,3);

d1=substr(var13,1,1);
d2=substr(var14,1,1);

if var8 not in (999);

/*���� �з� */
if var8 <15 then ag1=1; else  ag1=0;
if var8 >=15 & var8<65 then ag2=1; else ag2=0;
if var8 >=65 then ag3=1; else ag3=0;

/*���� & 15�� �̸� ���ɱ� �з� */
if var5=1 & ag1=1 then ag1_m=1; else ag1_m=0;
if var5=2 & ag1=1 then ag1_f=1;  else ag1_f=0;

/*���� & 15-64�� �̻� ���ɱ� �з�*/
if var5=1 & ag2=1 then ag2_m=1; else ag2_m=0;
if var5=2 & ag2=1 then ag2_f=1;  else ag2_f=0;

/*���� & 65�� �̻� ���ɱ� �з�*/
if var5=1 & ag3=1 then ag3_m=1; else ag3_m=0;
if var5=2 & ag3=1 then ag3_f=1;  else ag3_f=0;

KEY=COMPRESS(VAR4)||"-"||COMPRESS(DATE);
drop dd; run;

proc means data=a.stat01_19_r; var var8; run;

/*�������1 �ڵ� üũ*/
proc freq data=a.stat01_19_r; tables d1/list; run;

/*�������2 �ڵ� üũ */
proc freq data=a.stat01_19_r; tables d2/list; run;

/*S,T,U, V, X,Y,Z �ڵ� �ִ� ��� �����ϱ� */

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
/*��ü ���� ���  ���� */
ALL_CAUSE_M=AG0014_M+AG1564_M+AG65_M;
ALL_CAUSE_F=AG0014_F+AG1564_F+AG65_F; 
SIDO   =COMPRESS(SUBSTR(KEY,1,2)," ");
DATE   =SUBSTR(TRIM(KEY),4,10);
YEAR   =SUBSTR(KEY,4,4);
MONTH=SUBSTR(KEY,9,2);
DAY     =SUBSTR(KEY,12,2);
RUN;

/****************************************************/
/*Ư������ ��� Daily count ��з� */
%macro Daily_count(label,out);

data z; set a.stat01_19_r2;
if d1=&label.;

PROC SQL;
CREATE TABLE A.&out. AS SELECT KEY, COUNT(DATE) AS outcome, SUM(AG1) AS AG0014 , SUM(AG2) AS AG1564, SUM(AG3) AS AG65,
SUM(AG1_M) AS AG0014_M, SUM(AG1_F) AS AG0014_F, SUM(AG2_M) AS AG1564_M, SUM(AG2_F) AS AG1564_F, SUM(AG3_M) AS AG65_M, SUM(AG3_F) AS AG65_F FROM z 
GROUP BY KEY ; QUIT;

DATA A.&out.; SET a.&out.;
/*��ü ���� ���  ���� */
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

%Daily_count("I",Daily_count2); /*��ü ������*/
%Daily_count("J",Daily_count3); /*��ü ȣ���*/
%Daily_count("C",Daily_count4); /*��ü ��*/

/****************************************************/
