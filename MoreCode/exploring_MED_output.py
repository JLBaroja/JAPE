import pandas as pd
import numpy as np

fd=pd.read_csv('ConcurrentData/concurrent_full.csv')

# Pick bird and session
bb='p510'
ss='s10'

ld=fd.loc[(fd.bird==bb)&(fd.session==ss)]

rsp_left=ld.loc[ld.event=='response_left_key'].session_time_sec
rsp_right=ld.loc[ld.event=='response_right_key'].session_time_sec
rnf_left=ld.loc[ld.event=='feeder_on_left'].session_time_sec
rnf_right=ld.loc[ld.event=='feeder_on_right'].session_time_sec
all_rsp=pd.concat([rsp_left,rsp_right])
lever=pd.concat([pd.Series(np.repeat('left',len(rsp_left))),
	pd.Series(np.repeat('right',len(rsp_right)))])
all_rsp.index=lever.index
resp_dict={'lever':lever,'session_time':all_rsp}

responses=pd.DataFrame(data=resp_dict,index=all_rsp.index)
responses=responses.sort_values('session_time')
responses.index=range(len(responses))

responses['leave']=False
responses['engage']=True
responses['bout']=False

leave=responses.leave.values
engage=responses.engage.values
bout=responses.bout.values
key=responses.lever.values
tm=responses.session_time

ii=0
while ii < (len(key)-1):
	if key[ii+1]!=key[ii]:
		leave[ii]=True
	if key[ii+1]==key[ii]:
		engage[ii+1]=False

	ii=ii+1

bout[(leave==False)&(engage==False)]=True
responses['reinforced']=(tm.isin(rnf_left)|tm.isin(rnf_right))



#all_resp=pd.concat([rsp_left,rsp_right],ignore_index=True)
#all_reinf=pd.concat([rnf_left,rnf_right],ignore_index=True)
#all_resp.sort_values(inplace=True)
#all_reinf.sort_values(inplace=True)

# Next: generate data frames at RESPONSE, VISIT, and REINFORCER levels

# Variables to include in each 
# Common to all: [bird, session, time, lever]
# Specific: 
# RESPONSES: [response_category: join_left | join_right | leave_left | leave_right | working_left | working_right
#		n_resp_in_visit: integer, 1 : length_visit
#		n_resp_remaining: integer, length_visit-1 : 0
#		time_within_visit: continuous, from the first response in visit
#		time_left_visit: continuous, til visit's end
#		reinforced: logical
#		reinf_bfr_visit: integer, 0 : Inf
#		reinf_rem_visit: integer, 0 : Inf
#		resp_in_left: integer, 1 : Inf
#		resp_in_right: integer, 1 : Inf
#		]
# VISITS: [visit_start,
#		visit_end,
#		number_responses,
#		number_reinforcers,
#		time_acum_at_start,
#		time_acum_at_end,
#		resp_acum_at_start,
#		resp_acum_at_end,
#		reinf_acum_at_start,
#		reinf_acum_at_end,
#		visit_in_left,
#		visit_in_right
#		]
# REINFORCERS: [		 


