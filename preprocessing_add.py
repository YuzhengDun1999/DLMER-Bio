import pandas as pd
import argparse
from functools import reduce
from tqdm import tqdm, trange
import re
from livingTree import SuperTree
from copy import deepcopy


class Preprocessor(object):

	def __init__(self, input_file, output_file, do_filter=True):
		content = self.readlines_from_txt(input_file)

		## replace host-associated with host_associated
		self.content = self.fix_host_associated(content)
		df = self.parse_from_lines(self.content)
		if do_filter:
			print('Option: do filter (yes).')
			df = self.filter_by_label(df)
		self.tree = self.build_tree_from_labels(df['pred_labels'])
		#df = self.mutate_nlayers_from_labels(df)
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

	def build_tree_from_labels(self, labels):
		"""
		"""
		tree = SuperTree()
		tree.create_node(identifier='root')
		unique_labels = pd.Series(reduce(lambda x, y: x + y, labels.apply(lambda x: x.split(',')))).unique()
		basic_paths = map(lambda x: x.split('-'), unique_labels)
		paths = map(lambda x: ['-'.join(x[0:i]) for i in range(2, len(x) + 1)], basic_paths)
		tree.from_paths(paths)
		#tree.show()
		tree.init_nodes_data(value=0)
		return tree

	def extract_probas(self, tree, probas):
		"""
		"""
		#print(probas)
		nodes = list(probas.keys())
		tree.fill_with(probas)
		tree.update_values()
		ancestors = reduce(lambda x, y: x + y, [tree.get_path_to_node(nid) for nid in nodes])
		#print(ancestors)
		new_probas = { ( nid, tree.get_node(nid).data ) for nid in ancestors}
		return new_probas

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

	def filter_by_label(self, df):
		indeces_keep = df.apply(lambda x: x['pred_labels'].split(',').count(x['true_label']), axis=1)
		indeces_keep = indeces_keep.astype(bool)
		ndf = df[indeces_keep].reset_index(drop=True)
		print(ndf)
		return ndf

	def extract_layers(self, df):
		"""
		
		:param df:
		:return:
		"""
		tree = self.tree
		extract_probas = self.extract_probas
		print('Recalculating the label for each layer')
		pred_labels = df['pred_labels'].apply(lambda x: x.split(','))
		probas = df['probas'].apply(lambda x: ['%.3f' % float(i) for i in x.split(',')])
		plabels_probas = [ {label: float(proba) 
							for label, proba in zip(pred_labels[i], probas[i])} 
						   for i in range(len(probas)) ]
		# * + zip means unzip 
		labels_probas_extd = [ list( zip(*extract_probas(deepcopy(tree), i) ) ) 
										  for i in tqdm(plabels_probas) ] 
		#print(labels_probas_extd[0:5]) 
		pred_labels_extd = [','.join(i[0]) for i in labels_probas_extd]
		probas_extd = [','.join(map(lambda x: '%.3f' % x, i[1])) for i in labels_probas_extd]

		df['pred_labels'] = pred_labels_extd
		df['probas'] = probas_extd
		df['true_label'] = df['true_label'].apply(lambda x: ','.join(extract_path(x)))
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

if __name__ == '__main__':
	parser = argparse.ArgumentParser()
	parser.add_argument("-i", "--input-file", type=str, default='data_unlayered.txt', help="path of your input file")
	parser.add_argument("-o", "--output-file", type=str, default='data_layered.txt', help="path to save output")
	parser.add_argument("-f", "--filter", type=bool, default='True', help="adjust whether to filter the input samples")
	args = parser.parse_args()
	pr = Preprocessor(input_file=args.input_file, output_file=args.output_file, do_filter=args.filter)
