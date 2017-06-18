import pandas as pd
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
	data_frame=data_frame.ix[(data_frame['after_point']!='000')]
	data_frame['data_file']=file_name
	return data_frame


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
		# Trial-level and real-time-level functions work with a single argument now
		frames=[global_df,extracting_function(files[archive])]
		global_df=pd.concat(frames)
	os.chdir('..')
	global_df.to_csv(name_text_file)