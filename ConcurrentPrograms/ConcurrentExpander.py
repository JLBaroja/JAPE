"""
Script that expands AutoShaping.MPC into six different versions

def expand_MPC(output_file,interval_list,probability_left):
	Examples of valid arguments:
	interval_list='LIST I = 20,25,30,35,40,45,50,55,60,65,70'
	probability_left='P = 5000'
	output_file='exit_file.MPC'
	with open('AutoShaping.MPC','r') as base:
		data=base.readlines()
	data[57]=interval_list+'\r\n'
	data[100]='\tset '+probability_left+';\r\n'
	with open(output_file,'w') as modified:
		modified.writelines(data)

expand_MPC('atsh_LnR_Cnstnt.MPC','LIST I = 45','P = 5000')
expand_MPC('atsh_LnR_Vrbl.MPC','LIST I = 20,25,30,35,40,45,50,55,60,65,70','P = 5000')
expand_MPC('atsh_L_Cnstnt.MPC','LIST I = 45','P = 10000')
expand_MPC('atsh_L_Vrbl.MPC','LIST I = 20,25,30,35,40,45,50,55,60,65,70','P = 10000')
expand_MPC('atsh_R_Cnstnt.MPC','LIST I = 45','P = 0')
expand_MPC('atsh_R_Vrbl.MPC','LIST I = 20,25,30,35,40,45,50,55,60,65,70','P = 0')
"""

import numpy as np

with open('Concurrent_VI_VI.MPC','r') as base:
	data=base.readlines()


lower_first=10
upper_first=40
lower_second=50
upper_second=80
lwf=lower_first*60
upf=upper_first*60
lws=lower_second*60
ups=upper_second*60
possible_1st_ch=np.linspace(lwf,upf,upf-lwf+1)
possible_2nd_ch=np.linspace(lws,ups,ups-lws+1)
string_1st=possible_1st_ch.astype('int').astype('str')
string_2nd=possible_2nd_ch.astype('int').astype('str')
list_m='LIST M = '+string_1st[0]
list_n='LIST N = '+string_2nd[0]
for ii in range(len(string_1st)-1):
	list_m=list_m+','+string_1st[ii+1]
	list_n=list_n+','+string_2nd[ii+1]

discrepancies=np.array([-15,20])
dr=discrepancies.astype('str')
list_d='LIST D = '+dr[0]+','+dr[1]

data[data.index('\\\\ Line for LIST M\n')]=list_m+'\n'
data[data.index('\\\\ Line for LIST N\n')]=list_n+'\n'
data[data.index('\\\\ Line for LIST D\n')]=list_d+'\n'


with open('Exit_File.MPC','w') as modified:
	modified.writelines(data)





