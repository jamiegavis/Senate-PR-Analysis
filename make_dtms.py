#Edwin Gavis

import collections
import csv
import nltk
import operator
import os
import re
import string
import urllib3
from stemming.porter2 import stem

def main():
	print("Getting lists of press releases in corpus")
	sessions = make_list("Sessions")
	shelby = make_list("Shelby")
	corpus = sessions + shelby
	print("Finding most frequent tri/unigrams")
	search_for_terms(corpus)
	u_counts, t_counts = count_uni_tri_grams(corpus)
	u_freq, t_freq = find_most_frequent(u_counts, t_counts)
	print("Writing DTMs")
	write_dtms(corpus, u_freq, t_freq)

def make_list(senator = "Sessions"):
	rv = []
	folder = "raw/" + senator
	files = os.listdir(folder)
	for pr_title in files:
		rv.append(Press_Release(pr_title, folder))
	return rv

def search_for_terms(corpus):
	columns = [['author', 'date', 'fire_dept', "immigration", "nomination"]]
	fire_ex = re.compile('fire department')
	imm_ex = re.compile('immigration')
	nom_ex = re.compile('nomination')
	with open('outputs/fire_imm_nom_counts.csv', 'w', newline='') as csvfile:
		searchwriter = csv.writer(csvfile)
		searchwriter.writerow(columns)
		for pr in corpus:
			working = [pr.author]
			working.append(pr.day + pr.month + pr.year)
			working.append(len(fire_ex.findall(pr.text.lower())))
			working.append(len(imm_ex.findall(pr.text.lower())))
			working.append(len(nom_ex.findall(pr.text.lower())))
			searchwriter.writerow(working)
	
def count_uni_tri_grams(corpus):
	u_counts = collections.defaultdict(int)
	t_counts = collections.defaultdict(int)
	for pr in corpus:
		for u in pr.unigrams:
			u_counts[u] += 1
		for t in pr.trigrams:
			t_counts[t] += 1
	return u_counts, t_counts

def find_most_frequent(u_counts, t_counts):
	u_freq = sorted(u_counts.items(), reverse=True, key=operator.itemgetter(1))
	t_freq = sorted(t_counts.items(), reverse=True, key=operator.itemgetter(1))
	top_1k_u = [x[0] for x in u_freq[:1000]]
	top_500_t = [x[0] for x in t_freq[:500]]
	return top_1k_u, top_500_t

def write_dtms(corpus, u_freq, t_freq):
	tri_strings = ['.'.join(t) for t in t_freq]
	columns = ["speaker", "date"] + u_freq + tri_strings
	u_columns = ["speaker", "date"] + u_freq 
	t_columns = ["speaker", "date"] + tri_strings
	with open('outputs/top_uni_tri.csv', 'w', newline='') as csvfile:
		combinedwriter = csv.writer(csvfile)
		combinedwriter.writerow(columns)
		for pr in corpus:
			working = [pr.author]
			working.append(pr.day + pr.month + pr.year)
			for u in u_freq:
				working.append(pr.unigrams.count(u))
			for t in t_freq:
				working.append(pr.trigrams.count(t))
			combinedwriter.writerow(working)
	with open('outputs/top_unigrams.csv', 'w', newline='') as csvfile:
		uwriter = csv.writer(csvfile)
		uwriter.writerow(u_columns)
		for pr in corpus:
			working = [pr.author]
			working.append(pr.day + pr.month + pr.year)
			for uni in u_freq:
				working.append(pr.unigrams.count(uni))
			uwriter.writerow(working)
	with open('outputs/top_trigrams.csv', 'w', newline='') as csvfile:
		twriter = csv.writer(csvfile)
		twriter.writerow(t_columns)
		for pr in corpus:
			working = [pr.author]
			working.append(pr.day + pr.month + pr.year)
			for tri in t_freq:
				working.append(pr.trigrams.count(tri))
			twriter.writerow(working)

def get_stemmed_stops():
	http = urllib3.PoolManager()
	r = http.request('GET', 'http://www.ai.mit.edu/projects/jmlr/papers/volume5/lewis04a/a11-smart-stop-list/english.stop')
	cleaned_stops = str(r.data).replace('\\n', ' ').split(' ')
	stemmed_stops = set()
	cleaned_stops += ['shelby', 'sessions', 'richard', 'jeff', 'email', 'press', 'room', 'senate', 'member']
	for word in cleaned_stops:
		stemmed_stops.add(stem(word))
	return stemmed_stops


STOP_WORDS = get_stemmed_stops()


class Press_Release(object):

    def __init__(self, title, folder):
        '''
        Constructor 
        '''
        self.day = re.search('[0-9]+', title).group()
        self.month = re.search('[A-Za-z]+', title).group()
        self.year = re.findall('[0-9]+', title)[1]
        self.author = re.findall('[A-Za-z]+', title)[1]
        self.text = open(folder + '/' + title).read().replace('\n', '')
        self.unigrams = self.get_unigrams()
        self.trigrams = list(nltk.trigrams(self.unigrams))

    def get_unigrams(self):
        '''
        Removes punctuation, makes lowercase and tokenizes the text.
        '''
        depunctuator = str.maketrans('', '', string.punctuation)
        text = self.text.lower().translate(depunctuator)
        text_tokenized = nltk.tokenize.word_tokenize(text)
        unigrams = []
        for word in text_tokenized:
      	    stemmed = stem(word)
      	    if stemmed not in STOP_WORDS: 
      		    unigrams.append(stemmed)
        return unigrams

    def __repr__(self):
    	return (self.day + self.month + self.year + self.author)


if __name__=="__main__":
	main()
