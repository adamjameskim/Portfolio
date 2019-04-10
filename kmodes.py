import csv

from random import shuffle

from statistics import mode



################################################################################

def sorenson_dice_similarity(row, centroid):

	#print('in sorenson_dice_similarity')

	assert( len(row) == len(centroid) )

	n = len(centroid)

	i = 0

	same_count = 0

	while i < n:

		if row[i] == centroid[i]:

			same_count += 1

		i+=1

	return float(same_count/n)

################################################################################

def get_centroid(cluster):

	#print('in get_centroid')

	transposed = [list(i) for i in zip(*cluster)]

	centroid = []

	for column in transposed:

		centroid.append( max(column,key=column.count) )
 
	return centroid

################################################################################

def k_modes(clusters , K):

	#print('in k_modes')

	centroids = []

	for cluster in clusters:

		centroids.append( get_centroid(cluster) )

	cluster_itr = 0

	mover_count = 0

	master_counter = 0

	while cluster_itr < len(clusters):

		current_cluster = clusters[cluster_itr]

		member_itr = 0

		while member_itr < len(current_cluster):

			current_member = current_cluster[member_itr]

			distances = []

			for centroid in centroids:

				distances.append( sorenson_dice_similarity(current_member,centroid) )

			best_cluster_itr = distances.index(min(distances))

			'''
			print(distances)
			print(best_cluster_itr,cluster_itr)
			input('CONTINUE?')
			'''

			if best_cluster_itr != cluster_itr:

				clusters[best_cluster_itr].append(current_member)

				del current_cluster[member_itr]

				mover_count += 1

			else:

				member_itr += 1 # move on 

			#master_counter+=1
			#print('observation',master_counter,'done processing')

		cluster_itr += 1

	return clusters, mover_count

################################################################################

def init_clusters( data , K ):

	#print('in init_clusters')

	shuffle(data)

	cluster_centroids = data[:K] # step 1 ~ create clusters
	
	clusters = [ [x] for x in cluster_centroids]

	data = data[K:]

	for observation in data:

		similarities = [ sorenson_dice_similarity(observation,centroid) for centroid in cluster_centroids ]

		value, index = min((value, index) for (index, value) in enumerate(similarities))

		clusters[index].append(observation)

	return clusters

################################################################################

def read_csv():

	#print('in read_csv')

	csvFile = open('Camera.csv','r')

	csvObj = csv.reader(csvFile, delimiter=';')

	dataMatrix = [ x for x in csvObj]

	column_names = dataMatrix[0]

	del dataMatrix[:2] # first two rows are garbage

	csvFile.close()

	return dataMatrix, column_names

################################################################################

def main():

	#print('in main')

	K = 2

	stop_movers = 25

	data , column_names = read_csv()

	clusters = init_clusters( data , K )

	move_count = float('inf')

	j = 0

	print(len(data),' OBSERVATIONS!')
	input('start k_modes?')

	while move_count > stop_movers:

		clusters, move_count = k_modes(clusters , K)

		j += 1

		print('Run',j,':',move_count,'observations moved to another cluster')

		#input('continue?')

	print( '\n\nCOMPLETE!\n\n')

	input('print clusters?')

	k=0
	while k<len(clusters):
		print( 'cluster',k,'has',len(clusters[k]),'members!' )

	print('exit!')


	

################################################################################

if __name__ == '__main__':
	main()
