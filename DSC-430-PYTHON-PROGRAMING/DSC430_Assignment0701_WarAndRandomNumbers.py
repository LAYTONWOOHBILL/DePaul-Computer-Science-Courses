#Author: Wilson Wu
#Date:2019.11.03
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/u4rOZ9m8OMM



class WarAndPeacePseudoRandomNumberGenerator:

    

    def __init__(self,startpoint=1000):

        '''set default seed to 1000'''

        self.startpoint=startpoint
        self.binarylist=[]              #create list for 0,1
        
        self.f='war-and-peace.txt'
        self.file = open(self.f, 'r')   #open a file
        self.file.seek(0)               #start from 0

        for i in range(self.startpoint):
            self.start=self.file.read(1)    #the frist str
            
        for i in range(32):                 #start compare 2 str, generate 32 compare group
            
            for i in range(1682):           #1682 a loop
                
                self.b=self.file.read(1)
                
                if not self.b:              #if read end start from 345 
                    
                    self.file.seek(345)
                    
            for i in range(1105):           #1105 a loop
                
                self.c=self.file.read(1)

                if not self.c:

                    self.file.seek(456)     #if read end start from 456 

            if self.b>self.c:               #compare if first one greater than second one,append 1

                self.binarylist.append(1)

            else:
                self.binarylist.append(0)

    def __repr__(self):
        
        #an overloaded operator
        
        return "word is {}".format(self.startpoint)

    def getword(self):

        #get 32 len list
        
        return self.binarylist
        
    def random(self):
        
         #get create random num
        
        self.num=0
        for i in range(len(self.binarylist)):
            self.num+=self.binarylist[i]*(1/2**(i+1))
            
        return self.num
    
    def close(self):
        
        self.file.close()

    
import csv
import statistics
import math

def pseudorandomnumbers(num):
    
    ramdomnum=[]
    
    for i in range(1300,1300+num):
        
        prng=WarAndPeacePseudoRandomNumberGenerator(i)
        
        r1=prng.random()
        

        ramdomnum.append(r1) #append to lst

        prng.close()

    a=min(ramdomnum)

    b=max(ramdomnum)

    c=statistics.mean(ramdomnum)
    
    print('min:{},max:{},mean:{}'.format(a,b,c))

'''

def findmin(r1,r2):
    minnum=r1
    newnum=r2
    
    if newnum<minnum:
        return newnum
    else:      
        return minnum
    
def findmax(r1,r2):
    minnum=r1
    newnum=r2
    
    if newnum>minnum:
        return newnum
    else:      
        return minnum


def main():
    prng=WarAndPeacePseudoRandomNumberGenerator()
    r1=prng.random()
    for i in range(1300,1400):
        prng=WarAndPeacePseudoRandomNumberGenerator(i)
        r2=prng.random()
        r1=findmax(r1,r2)
    return r1


def prn(num):
    prng=WarAndPeacePseudoRandomNumberGenerator(1300)
    r1=prng.random()
    for i in range(1301,1300+num-1):
        prng=WarAndPeacePseudoRandomNumberGenerator(i)
        r2=prng.random()
        r1=findmin(r1,r2)
        r=findmax(r1,r2)
        prng.close()
    a=r1
    b=r2
    #c=statistics.mean(ramdomnum)
    
    print(a,b)
'''
