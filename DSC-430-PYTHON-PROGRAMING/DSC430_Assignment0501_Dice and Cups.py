#Author: Wilson Wu
#Date:2019.10.15
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link:  https://youtu.be/VOJaRBW97a8



import random

class SixSidedDie:
    
    '''Class represent Six Sided Die '''
    
    def __init__(self):
        #random generate dice value
        self.value=random.randint(1,6)

    def __repr__(self):
        #an overloaded operator
        return "SixSidedDie({})".format(self.value)

    def roll(self):
        #random roll dice value
        self.value=int(random.randint(1,6))
        
        return self.value
    
    def getFaceValue(self):
        #return value
        return self.value


class TenSidedDie(SixSidedDie):

#Inheritance-SixSidedDie
    '''Class represent Ten Sided Die '''
    
    def __init__(self):
        #random generate dice value
        self.value=random.randint(1,10)

    def __repr__(self):
        #an overloaded operator
        return "TenSidedDie({})".format(self.value)

    def roll(self):
        #random roll dice value
        self.value=int(random.randint(1,10))
        
        return self.value
    
    
class TwentySidedDie(SixSidedDie):
#Inheritance-SixSidedDie
    '''Class represent Twenty Sided Die '''
    
    def __init__(self):
        #random generate dice value
        self.value=random.randint(1,20)

    def __repr__(self):
        #an overloaded operator
        return "TwentySidedDie({})".format(self.value)

    def roll(self):
        #random roll dice value
        self.value=int(random.randint(1,20))
        
        return self.value


class Cup():

    def __init__(self,NumSixSidedDie=1,NumTenSidedDie=1,NumTwentySidedDie=1):
        #random generate dice value
        
        self.cuplist=[]
        
        #append for overloaded operator
        
        self.value=0

        #add dice value
    
        
        self.NumSixSidedDie=NumSixSidedDie
        self.NumTenSidedDie=NumTenSidedDie
        self.NumTwentySidedDie=NumTwentySidedDie

        #set number of dice


        #for loop generate the dice and value

        for i in range(0,self.NumSixSidedDie):
            s=SixSidedDie()
            self.cuplist.append(s)
            self.value+=s.getFaceValue()
        for i in range(0,self.NumTenSidedDie):
            s=TenSidedDie()
            self.cuplist.append(s)
            self.value+=s.getFaceValue()
        for i in range(0,self.NumTwentySidedDie):
            s=TwentySidedDie()
            self.cuplist.append(s)
            self.value+=s.getFaceValue()
        

    def __repr__(self):
        
        #an overloaded operator
        
        return "Cup{}".format(tuple(self.cuplist))
    
    def roll(self):

        self.cuplist=[]
        self.value=0
        
        #random roll dice value

        #for loop generate the dice and value

        for i in range(0,self.NumSixSidedDie):
            s=SixSidedDie()
            self.cuplist.append(s)
            self.value+=s.getFaceValue()
        for i in range(0,self.NumTenSidedDie):
            s=TenSidedDie()
            self.cuplist.append(s)
            self.value+=s.getFaceValue()
        for i in range(0,self.NumTwentySidedDie):
            s=TwentySidedDie()
            self.cuplist.append(s)
            self.value+=s.getFaceValue()

        return self.value

    def getSum(self):

        #get value
        
        return self.value

        
    
