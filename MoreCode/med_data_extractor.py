import pandas as pd
import numpy as np
import os


def caterpillar(file_name,array):
	"""
	Function that reads file 'file_name' and
	returns data contained in 'array' as a dict.
	(Should work for any MED file.)
	"""
	archive=open(file_name)
	found=False
	block=[]
	array_name=array+':'
	for line in archive:
		if line.startswith('Subject:'):
			subject=line.split()[1]
		if line.startswith('Experiment:'):
			experiment=line.split()[1]
		if line.startswith('Group:'):
			group=line.split()[1]
		if line.startswith('Box:'):
			box=line.split()[1]
		if line.startswith('Start Date:'):
			date=line.split()[2]
		if line.startswith('MSN:'):
			msn=line.split()[1]
		if line.startswith('Start Time:'):
			start_time=line.split()[2]
		if line.startswith('End Time:'):
			end_time=line.split()[2]
		if found:
			if len(line.split())==1:
				break
			block.append(line.split()[1:len(line.split())])
		else:
			if line.startswith(array_name):
				found=True

	before_point=[]
	after_point=[]
	for d1 in range(len(block)):
		for d2 in range(len(block[d1])):
			before_point.append(block[d1][d2].split('.')[0])
			after_point.append(block[d1][d2].split('.')[1])

	output={'raw_block':block,
		'before_point':before_point,
		'after_point':after_point,
		'condition':group,
		'experiment_name':experiment,
		'experiment_program':msn,
		'subject':subject,
		'box':box,
		'date':date,
		'session_start':start_time,
		'session_end':end_time}

	return output


def hatter(file_name,array):
	"""
	Function that extracts certain array from certain
	MED file and returns it in a pandas dataframe
	"""
	extraction=caterpillar(file_name,array)
	base={'before_point':extraction['before_point'],
		'after_point':extraction['after_point'],
		'subject':extraction['subject'],
		'box':extraction['box'],
		'date':extraction['date'],
		'session_start':extraction['session_start'],
		'session_end':extraction['session_end'],
		'condition':extraction['condition'],
		'experiment_name':extraction['experiment_name'],
		'experiment_program':extraction['experiment_program']}
	data_frame=pd.DataFrame(base)
	data_frame=data_frame.loc[(data_frame['after_point']!='000')]
	data_frame['data_file']=file_name
	return data_frame



def concurrent_extractor(file):
	ht=hatter(file,'A')
	rt=ht.loc[:,['box',
			'data_file',
			'date',
			'session_start',
			'session_end']]
	rt['bird']=ht.subject
	rt['session']=file.split('_')[1]
	rt['med_notation_file']=ht.experiment_program
	rt['event_key']=ht.after_point
	rt['time_sec_100']=ht.before_point
	rt['event']=ht.after_point
	rt['session_time_sec']=ht.before_point.astype('float')/100
	rt.event.loc[rt.event_key=='010']='session_start'
	rt.event.loc[rt.event_key=='020']='session_end'
	rt.event.loc[rt.event_key=='110']='response_central_key'
	rt.event.loc[rt.event_key=='120']='response_left_key'
	rt.event.loc[rt.event_key=='130']='response_right_key'
	rt.event.loc[rt.event_key=='220']='left_light_on'
	rt.event.loc[rt.event_key=='230']='right_light_on'
	rt.event.loc[rt.event_key=='320']='left_light_off'
	rt.event.loc[rt.event_key=='330']='right_light_off'
	rt.event.loc[rt.event_key=='240']='feeder_on'
	rt.event.loc[rt.event_key=='340']='feeder_off'
	rt.event.loc[rt.event_key=='250']='chamber_light_on'
	rt.event.loc[rt.event_key=='350']='chamber_light_off'
	rt.event.loc[rt.event_key=='540']='feeder_on_left'
	rt.event.loc[rt.event_key=='640']='feeder_on_right'
	rt.event.loc[rt.event_key=='740']='reinforcer_scheduled_left'
	rt.event.loc[rt.event_key=='840']='reinforcer_scheduled_right'
	return rt



def build_csv_files(sessions):
	read_path='ConcurrentData/Raw MED files/'
	write_path='ConcurrentData/CSV files/'
	for bb in ['p004','p054','p138','p510','p530','p736']:
		for ss in sessions:
			read_file=bb+'_s'+str(ss).zfill(2)+'_japede'
			write_file=bb+'s'+str(ss).zfill(2)+'.csv'
			os.chdir(read_path)
			is_there=read_file in os.listdir('.')
			if is_there:
				df=concurrent_extractor(read_file)
				os.chdir('..')
				os.chdir('..')
				os.chdir(write_path)
				df.to_csv(write_file)
				print 'written'
			os.chdir('..')
			os.chdir('..')
			print read_file
			print write_file
			print is_there

sessions=np.arange(75,80,1)
build_csv_files(sessions)


def concurrent_builder(output_archive):
	"""
	Makes a single DF according to 'function' with information
	contained in all files in 'Raw MED files'.
	"""
	os.chdir('Raw MED files/')
	files=os.listdir('.')
	files.sort()
	global_df=pd.DataFrame()
	for archive in range(len(files)):
		print files[archive]
		#if files[archive]!='p736_s02_atsh':
		frames=[global_df,concurrent_extractor(files[archive])]
		global_df=pd.concat(frames)
	os.chdir('..')
	global_df.to_csv(output_archive)




def concurrent_updater():
	"""
	Assumes there's a full_concurrent.csv, looks for files not already
	included, and adds them to the file.
	"""
	cf=pd.read_csv('ConcurrentData/concurrent_full.csv')
	already_included=cf.data_file.unique()
	not_yet_included=list()
	all_raw_files=os.listdir('ConcurrentData/Raw MED files')
	print 'The following archives be aded to concurrent_full.csv:'
	for arf in all_raw_files:
		if arf not in already_included:
			not_yet_included.append(arf)
			print arf

	keep_going=raw_input('That okay? [y]/n')
	if keep_going.lower()=='y' or keep_going=='':
		os.chdir('ConcurrentData/Raw MED files/')
		for archive in not_yet_included:
			print 'Adding '+archive
			frames=[cf,concurrent_extractor(archive)]
			cf=pd.concat(frames)
		os.chdir('..')
		cf.to_csv('concurrent_full.csv')
		print 'concurrent_full.csv updated, Bru!'
		os.chdir('..')
	else:
		print 'camaras chido!'



def real_time_extractor(file_name,z_pulses=False):
	"""
	Extracts event ocurrence in real time
	"""
	events={'010':'session_start',
		'020':'session_end',
		'110':'resp_central_key',
		'120':'resp_left_key',
		'130':'resp_right_key',
		'210':'central_light_on',
		'220':'left_light_on',
		'230':'right_light_on',
		'240':'feeder_on',
		'250':'chamber_light_on',
		'310':'central_light_off',
		'320':'left_light_off',
		'330':'right_light_off',
		'340':'feeder_off',
		'350':'chamber_light_off'}
	target_array='A'

	if z_pulses:
		events={'001':'z001',
			'002':'z002',
			'003':'z003',
			'004':'z004'}
		target_array='Z'

	df=hatter(file_name,target_array)
	df['event']=0
	df['session_time']=df['before_point'].astype(float)/100

	for ee in events.keys():
		df['event'][df['after_point']==ee]=events[ee]

	return df


def real_time_data(file):
	rt=real_time_extractor(file)
	zp=real_time_extractor(file,z_pulses=True)
	frames=[rt,zp]
	df=pd.concat(frames)
	return df


def data_frame_merger(extracting_function,name_text_file):
	"""
	Makes a single DF according to 'function' with information
	contained in all files in 'Raw MED files'.
	"""
	os.chdir('Raw MED files/')
	files=os.listdir('.')
	global_df=pd.DataFrame()
	for archive in range(len(files)):
		print files[archive]
		#if files[archive]!='p736_s02_atsh':
		frames=[global_df,extracting_function(files[archive])]
		global_df=pd.concat(frames)
	os.chdir('..')
	global_df.to_csv(name_text_file)
