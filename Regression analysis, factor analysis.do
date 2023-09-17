//回归分析部分dofile  2019.7.29 /20:59 Mon
//打开数据
cd E:\工作\2019社调\数据
use datafac.1.0.dta
//准备工作
//控制变量
q1 q2 q3_1 q3_2 q3_3 q3_4 q5 
//因子变量
factor2scores1 factor2scores2 factor2scores3 
 factor3_6scores1 factor3_8scores1 factor5scores1 factor3_5scores1 factor3_5scores2
  factor5scores2 factor6scores1 factor7scores1 factor8scores1 factor8scores2
//政治效能感
gen mean10=(q98+q100+q101)/3

//民主价值观与网络频率
regress factor5scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
regress factor5scores2q 1 q2 q3_1 q3_2 q3_3 q3_4 q5  q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 

regress  factor6scores1 factor5scores1 factor5scores2,beta 
regress  factor6scores1 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 

//政治参与和网络频率
regress factor7scores1 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 

//政治兴趣和网络频率
regress q96 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta
outreg2 using myfile, replace cttop(full) 
regress q96 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q22 q23 q24 q25 q26,beta
outreg2 using myfile, replace cttop(full) 
//社会资本与网络频率
regress factor2scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile

regress factor2scores2 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile

regress factor2scores3 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile

//政治信任与社会资本
regress factor6scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  factor2scores1 factor2scores2 factor2scores3 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile, replace cttop(full)

//政治效能感与网络频率
regress mean10 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile, replace cttop(full)
//政治信任与政治效能感
regress factor6scores1 qq1 q2 q3_1 q3_2 q3_3 q3_4 q5  mean10
outreg2 using myfile, replace cttop(full)
//政治信任与网络
regress factor6scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile, replace cttop(full)

//政治信任、政治效能感、网络
regress factor6scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  mean10 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile, replace cttop(full)

regress factor6scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q4_1 q4_2 q4_3 q4_4 q5 q6 q7 q8 q9 q10 mean10 q22

//网络使用能力、效用与网络频率
regress factor3_5scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q4_1 q4_2 q4_3 q4_4 q5 q6 q7 q8 q9 q10 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile, replace cttop(full)
regress factor3_5scores2 q1 q2 q3_1 q3_2 q3_3 q3_4 q4_1 q4_2 q4_3 q4_4 q5 q6 q7 q8 q9 q10 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile, replace cttop(full)
//政治信任和政治兴趣
regress factor6scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q96
outreg2 using myfile, replace cttop(full)
//中介
bootstrap  r(ind_eff) r(dir_eff), reps(1000):sgmediation factor6scores1,mv(q96) iv(q22) cv(q1 q2 q3_1 q3_2 q3_3 q3_4 q5 )
//
regress q50 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores1
outreg2 using myfile, replace cttop(full)
regress factor6scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores1
outreg2 using myfile, replace cttop(full)
regress factor6scores1 q50 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores1
outreg2 using myfile, replace cttop(full)
//
regress factor3_5scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q80 q81 q82 q83 q84
outreg2 using myfile, replace cttop(full)

regress  factor7scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores1
outreg2 using myfile, replace cttop(full)

regress factor3_5scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile, replace cttop(full)

regress  factor7scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38,beta 
outreg2 using myfile, replace cttop(full)


//中介 自变量factor3_6scores1 因变量factor6scores1 中介变量zzxq 
gen zzxq=(q96+q97)/2
regress factor6scores1 factor3_6scores1
regress zzxq factor3_6scores1 
regress factor6scores1 factor3_6scores1 zzxq

gen gjryg=(q55+q56+q57)/3
//自变量q23-q29，因变量 gjryg，调节变量zzxq
reg gjryg q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q23 q24 q25 q26 q27 q28 q29 zzxq
estat vif
outreg2 using myfile
outreg2 using myfile, replace cttop(full)

gen q28_zzxq=q28*zzxq

reg gjryg q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q23 q24 q25 q26 q27 q28 q29 zzxq q28_zzxq
estat vif//多重共线性
sum q28 zzxq
gen nq28=q28-3.669118
gen nzzxq=zzxq-2.975735
gen nq28_nzzxq=nq28*nzzxq
reg gjryg q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q23 q24 q25 q26 q27 nq28 q29 nzzxq nq28_nzzxq,beta
estat vif
outreg2 using myfile，replace

reg gjryg q23 q24 q25 q26 q27 q28 q29 q23_zzxq q24_zzxq q25_zzxq q26_zzxq q27_zzxq q28_zzxq q29_zzxq
//社交媒体使用多的人会不会守政治宣传影响？
//这极其重要
reg factor8scores2 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q23 q24 q25 q26 q27 q28 q29 zzxq //!!!!!!!!!!
factor8scores2

//交互
gen y1=q22*factor7scores1
label var y1 "q22*factor7scores1"
reg factor6scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q22 factor7scores1 y1 
outreg2 using myfile，`replace'
reg factor6scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q22 factor7scores1 q23 q24 q25 q26 q27 q28 q29 y1 
outreg2 using myfile，`replace' 
reg factor6scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q22 factor7scores1
outreg2 using myfile

//
reg factor5scores2 factor3_5scores1 //民主-网络参政认同因子 中介
reg factor7scores1 factor3_5scores1
reg factor5scores2 factor3_5scores1 factor7scores1

reg factor6scores1 factor3_5scores1  //政治信任
reg factor7scores1 factor3_5scores1 
reg factor6scores1 factor3_5scores1 factor7scores1

reg factor6scores1  q22 q1 q2
reg factor8scores2  q22 q1 q2
reg factor6scores1  q22 factor8scores2 q1 q2

//
//中介
reg factor6scores1 factor8scores2 
reg q50 factor8scores2 
reg factor6scores1 factor8scores2 q50

//政治兴趣被影响
reg zzxq q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores1 factor3_5scores2 
outreg2 using myfile
reg zzxq q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q30 q31 q32 q33 q34 q35 q36 q37 q38
outreg2 using myfile
reg zzxq q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores1 factor3_5scores2 q30 q31 q32 q33 q34 q35 q36 q37 q38
outreg2 using myfile
//选举参与、权益维护因子、网络媒介使用能力因子、和网络参政认同因子”
reg factor7_ml1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores1 
outreg2 using myfile,replace
reg factor7_ml1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores2
outreg2 using myfile
reg factor7_ml1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores1 factor3_5scores2
outreg2 using myfile
reg factor7_ml2 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores1 
outreg2 using myfile
reg factor7_ml2 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores2
outreg2 using myfile
reg factor7_ml2 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_5scores1 factor3_5scores2
outreg2 using myfile

//国家荣誉感
reg gjryg q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q74 q50

gen factor3_4scores1=(q31+q32)/2 //政治信息关注

factor q33 q34 q35 q36 q37 q38 , pcf //政治意见表达
rotate  
estat kmo
predict factor3_4scores2

reg gjryg q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_4scores1
outreg2 using myfile,replace
reg gjryg q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_4scores2
outreg2 using myfile
reg gjryg q1 q2 q3_1 q3_2 q3_3 q3_4 q5 factor3_4scores1 factor3_4scores2
outreg2 using myfile
//民主价值观
reg factor5scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q50
reg factor5scores2 q1 q2 q3_1 q3_2 q3_3 q3_4 q5 q74 q50
outreg2 using myfile,replace
reg factor5scores1 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q50
outreg2 using myfile,replace
reg factor5scores2 q1 q2 q3_1 q3_2 q3_3 q3_4 q5  q50
outreg2 using myfile
//
reg q28 q34
pwcorr q28 q34,sig
 pwcorr q28 q35,sig








bootstrap r(ind_eff) r(dir_eff), reps(5000): sgmediation factor6scores1, iv(factor3_6scores1) mv(zzxq) cv()












































