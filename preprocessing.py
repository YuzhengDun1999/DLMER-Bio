import pandas as pd
import argparse
from functools import reduce
from tqdm import tqdm, trange
import re


class Preprocessor(object):

	def __init__(self, input_file, output_file):
		content = self.readlines_from_txt(input_file)

		## replace host-associated with host_associated
		self.content = self.fix_host_associated(content)
		df = self.parse_from_lines(self.content)
		df = self.mutate_nlayers_from_labels(df)
		self.df = self.extract_layers(df)
		self.to_text(self.df, output_file=output_file)


	def fix_host_associated(self, content):
		"""
		fix host-associated error in unlayered data, tested
		:param content:
		:return:
		"""
		print('Fixing errors in unlayered data')
		return [i.replace('Host-associated', 'Host_associated') for i in tqdm(content)]

	def readlines_from_txt(self, file_dir):
		"""
		tested
		:param file_dir:
		:return:
		"""
		print('Reading data......', end='')
		with open(file_dir, 'r') as f:
			lines = f.readlines()
		lines = [line.rstrip('\n') for line in lines]
		print('finished !')
		return lines

	def parse_from_lines(self, lines):
		"""
		tested
		:param lines:
		:return:
		"""
		print('Parsing text data to dataframe......', end='')
		parts_tmp = [line.split(' ') for line in lines]
		df_values = [line[0].split('|') + [line[1].split('|')[1]] + [line[2]] for line in parts_tmp]
		data = pd.DataFrame(df_values, columns=['sample_id', 'pred_labels', 'true_label', 'probas'])
		print('finished !')
		return data

	def mutate_nlayers_from_labels(self, df):
		"""
		tested
		:param df:
		:return:
		"""
		print('Calculating the number of layers for each label......', end='')
		pred_labels = df['pred_labels']
		plabels_split = pred_labels.apply(lambda x: x.split(','))
		df['nlayers'] = plabels_split.apply(lambda x: [count_pattern('-', i) + 1 for i in x])
		print('finished !')
		return df

	def extract_layers(self, df):
		"""
		tested
		:param df:
		:return:
		"""
		print('Recalculating the label for each layer')
		pred_labels = df['pred_labels'].apply(lambda x: x.split(','))
		df['pred_labels'] = pred_labels.apply(lambda x: ','.join([','.join(extract_path(i)) for i in x]))
		df['true_label'] = df['true_label'].apply(lambda x: ','.join(extract_path(x)))
		nlayers = df['nlayers']
		probas = df['probas'].apply(lambda x: ['%.3f' % float(i) for i in x.split(',')])
		df['probas'] = [','.join(map(str, rep_elements(probas[i], nlayers[i]))) for i in trange(len(probas))]
		return df

	def to_text(self, df, output_file):
		df['text'] = df.apply(lambda x: '{0}|{1} {0}|{2} {3}'.format(x['sample_id'],
																	 x['pred_labels'],
																	 x['true_label'],
																	 x['probas']),
							  axis=1)

		with open(output_file, 'w') as f:
			f.write('\n'.join(df['text']))
		print('Results are saved in {}'.format(output_file))


def rep_elements(elements, times):
	rep_elements = [[elements[i]] * times[i] for i in range(len(elements))]
	return reduce(lambda x, y: x+y, rep_elements)


def extract_path(string):
	path = string.split('-')
	return ['-'.join(path[0:i]) for i in range(1, len(path) + 1)]


def count_pattern(pattern, string):
	return len(re.findall(pattern, string))


parser = argparse.ArgumentParser()
parser.add_argument("-i", "--input_file", type=str, default='data_unlayered.txt', help="path of your input file")
parser.add_argument("-o", "--output_file", type=str, default='data_layered.txt', help="path to save output")
args = parser.parse_args()
pr = Preprocessor(input_file=args.input_file, output_file=args.output_file)
