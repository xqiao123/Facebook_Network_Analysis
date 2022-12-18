import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

pd.set_option('display.max_columns', 1000) 
pd.set_option('display.max_rows', 1000) 
pd.set_option('display.width', 1000)


test_edges=pd.read_csv('/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/test_edges.csv')
test_edges2=test_edges.rename(columns={'V1':'from','V2':'to'}, inplace=False)
test_edge3=test_edges2[['from','to']]
test_edge3['new']=test_edge3['from']+','+test_edge3.to
test_edge_lst=list(test_edge3['new'])

N=1014
P=57

name_lst= 'SE PH PD LN MB AL DB CWB BS AC BW DZM KW CR AAE HRS LRS SL BCL SLL MWB BD LL BB LSM KM BL CG CSH GGA ADL DL LBF GL CS DSF ALL CW1 DZB PW ARL LZ MBM MD JLT LSF TSL'.split()

#The original strongest component, use this to determine the threshold among three methods
cn_strong_scores=pd.read_csv('/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/cn_strong_scores')
pa_strong_scores=pd.read_csv('/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/pa_strong_scores')
jc_strong_scores=pd.read_csv('/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/jc_strong_scores')

#Training graph
cn_scores=pd.read_csv('/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/cn_scores')
pa_scores=pd.read_csv('/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/pa_scores')
jc_scores=pd.read_csv('/Users/joyce/Desktop/MA710_Data_Mining/Group_Project/jc_scores')


cn_scores2=cn_scores[['from','to','value']]
for i in range(len(name_lst)):
    i2=i+1
    cn_scores2['from'][cn_scores2['from']==i2]=name_lst[i]
    cn_scores2['to'][cn_scores2['to']==i2]=name_lst[i]
    
pa_scores2=pa_scores[['from','to','value']]
for i in range(len(name_lst)):
    i2=i+1
    pa_scores2['from'][pa_scores2['from']==i2]=name_lst[i]
    pa_scores2['to'][pa_scores2['to']==i2]=name_lst[i]
    
jc_scores2=jc_scores[['from','to','value']]
for i in range(len(name_lst)):
    i2=i+1
    jc_scores2['from'][jc_scores2['from']==i2]=name_lst[i]
    jc_scores2['to'][jc_scores2['to']==i2]=name_lst[i]

cm_per_lst=np.percentile(cn_strong_scores.value, range(0,101,1))
pa_per_lst=np.percentile(pa_strong_scores.value, range(0,101,1))
jc_per_lst=np.percentile(jc_strong_scores.value, range(0,101,1))


cm_tpr_lst=[]
cm_fpr_lst=[]

pa_tpr_lst=[]
pa_fpr_lst=[]

jc_tpr_lst=[]
jc_fpr_lst=[]


##CN method
for num in cm_per_lst:
    cn_scores3=cn_scores2[cn_scores2.value>=num]
    cn_scores3['new']=cn_scores3['from']+','+cn_scores3['to']

    counter=0
    for i in range(len(cn_scores3)):
        if cn_scores3.iloc[i,3] in test_edge_lst:
            counter+=1
    
    tp=counter
    fp=len(cn_scores3)-counter
    tn=N-fp
    fn=P-tp
    fpr=fp/N
    fnr=fn/P
    tpr=tp/P
    
    cm_tpr_lst.append(tpr)
    cm_fpr_lst.append(fpr)


##PA method
for num in pa_per_lst:
    pa_scores3=pa_scores2[pa_scores2.value>=num]
    pa_scores3['new']=pa_scores3['from']+','+pa_scores3.to
    
    counter2=0
    for i in range(len(pa_scores3)):
        if pa_scores3.iloc[i,3] in test_edge_lst:
            counter2+=1
    
    tp2=counter2
    fp2=len(pa_scores3)-counter2
    tn2=N-fp2
    fn2=P-tp2
    fpr2=fp2/N
    fnr2=fn2/P
    tpr2=tp2/P
    
    pa_tpr_lst.append(tpr2)
    pa_fpr_lst.append(fpr2)


##JC method
for num in jc_per_lst:
    jc_scores3=jc_scores2[jc_scores2.value>=num]
    jc_scores3['new']=jc_scores3['from']+','+jc_scores3.to
    
    counter3=0
    for i in range(len(jc_scores3)):
        if jc_scores3.iloc[i,3] in test_edge_lst:
            counter3+=1
    
    tp3=counter3
    fp3=len(jc_scores3)-counter3
    tn3=N-fp3
    fn3=P-tp3
    fpr3=fp3/N
    fnr3=fn3/P
    tpr3=tp3/P
    
    jc_tpr_lst.append(tpr3)
    jc_fpr_lst.append(fpr3)


##Visualization

plt.plot(cm_fpr_lst, cm_tpr_lst, label='CN', color='blue')
plt.scatter(cm_fpr_lst, cm_tpr_lst, label='CN', color='blue')

plt.plot(pa_fpr_lst, pa_tpr_lst, label='PA', color='orange')
plt.scatter(pa_fpr_lst, pa_tpr_lst, label='PA', color='orange')

plt.plot(jc_fpr_lst, jc_tpr_lst, label='JC', color='green')
plt.scatter(jc_fpr_lst, jc_tpr_lst, label='JC', color='green')

plt.xlim([0,1])
plt.ylim([0,1])
plt.legend()
plt.title('ROC Curve')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.show()


