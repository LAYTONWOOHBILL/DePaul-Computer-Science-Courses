
#Author: Wilson Wu
#Date:2019.11.09
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/_mQ7-9-4Kro

import random 

class SimplePlotGenerator:

    def __init__(self):

        '''plot'''

        self.plot='Something happens'
        
        self.value= 0
        

    def registerview(self):

        '''if register'''

        self.value=1
        print('hi new viewer register')

        return self.value

        
    def generate(self):

        return self.plot


class RandomPlotGenerator(SimplePlotGenerator):


     def generate(self):
         
         self.filelst=['plot_names','plot_adjectives','plot_profesions','plot_verbs','plot_adjectives_evil','plot_villains','plot_villian_job']

         self.plotlist=[] #chose word list 

         for self_filename in self.filelst:
             word_list=[] #all words in list
             infile = open(self_filename+'.txt', 'r')
             for i in infile:
                 word = infile.readline().replace('\n','')
                 word_list.append(word)
             one_word = random.choice(word_list) #random choice
             self.plotlist.append(one_word)
             
         self.plot = '{}, a {} {}, must {} the {} {}, {}.'.format(self.plotlist[0],self.plotlist[1],self.plotlist[2],self.plotlist[3],self.plotlist[4],self.plotlist[5],self.plotlist[6])

         return self.plot
        
class InteractivePlotGenerator(SimplePlotGenerator):

    def get(self): # register input

        self.input = int(input('''Please selct one word and enter the number!'''))

        return self.input 

    def generate(self):
         
         self.filelst=['plot_names','plot_adjectives','plot_profesions','plot_verbs','plot_adjectives_evil','plot_villains','plot_villian_job']

         self.plotlist=[]

         for self_filename in self.filelst:
             word_list=[]
             infile = open(self_filename+'.txt', 'r')
             for i in infile:
                 word = infile.readline().replace('\n','')
                 word_list.append(word)
             five_word = random.sample(word_list,5)
             
             if self.value==0: #if not register 
                 ask_word_index = int(input('''Please selct one word and enter the number!\n 1.{} 2.{} 3.{} 4.{} 5.{} '''.format(five_word[0],five_word[1],five_word[2],five_word[3],five_word[4])))
                 self.plotlist.append(five_word[ask_word_index-1])
             else: #if register 
                 print('''1.{} 2.{} 3.{} 4.{} 5.{} '''.format(five_word[0],five_word[1],five_word[2],five_word[3],five_word[4]))
                 ask_word_index=self.get()
                 self.plotlist.append(five_word[ask_word_index-1])

         self.plot = '{}, a {} {}, must {} the {} {}, {}.'.format(self.plotlist[0],self.plotlist[1],self.plotlist[2],self.plotlist[3],self.plotlist[4],self.plotlist[5],self.plotlist[6])

         return self.plot


              
        
             


    

        
            
                 
             

             
