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

def expand_concurrent(vi_left,
			vi_right,
			discrepancies=[0,0],
			lower_first=3,
			upper_first=5,
			lower_second=6,
			upper_second=8,
			session_length=10):
	with open('Concurrent_VI_VI.MPC','r') as base:
		data=base.readlines()

	# VI Schedules in each key
#	vi_left=[10,5,25]
#	vi_right=[20,30,15]
	vi_L=np.array(vi_left)
	vi_R=np.array(vi_right)
	vi_L_list=10000/vi_L
	vi_R_list=10000/vi_R
	vi_L_l=vi_L_list.astype('str')
	vi_R_l=vi_R_list.astype('str')
	list_l='LIST L = '+vi_L_l[0]+','+vi_L_l[1]+','+vi_L_l[2]
	list_r='LIST R = '+vi_R_l[0]+','+vi_R_l[1]+','+vi_R_l[2]

	# Lower and Upper limits of possible change points
#	lower_first=10
#	upper_first=40
#	lower_second=50
#	upper_second=80
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
	for ii in range(len(string_2nd)-1):
		list_n=list_n+','+string_2nd[ii+1]

	# Discrepancies
	discrepancies=np.array(discrepancies)
	dr=discrepancies.astype('str')
	list_d='LIST D = '+dr[0]+','+dr[1]

	# Session length
	variable_s=str(session_length*60*100)

	# Lines substitution
	data[data.index('\\\\ Line for LIST M\n')]=list_m+'\n'
	data[data.index('\\\\ Line for LIST N\n')]=list_n+'\n'
	data[data.index('\\\\ Line for LIST D\n')]=list_d+'\n'
	data[data.index('\\\\ Line for LIST L\n')]=list_l+'\n'
	data[data.index('\\\\ Line for LIST R\n')]=list_r+'\n'
	data[data.index('\\\\ Line for variable S\n')]='\tset S = '+variable_s+';\n'

	vi_L=vi_L.astype('str')
	vi_R=vi_R.astype('str')
	file_label='japede_L_'+vi_L[0]+'_'+vi_L[1]+'_'+vi_L[2]+'_'+'R_'+vi_R[0]+'_'+vi_R[1]+'_'+vi_R[2]+'.MPC'
	with open(file_label,'w') as modified:
		modified.writelines(data)

#	return data

#expand_concurrent([30,30,30],[90,90,90])
#expand_concurrent([10,15,5],[10,20,10])
#expand_concurrent([25,10,50],[10,20,10])

