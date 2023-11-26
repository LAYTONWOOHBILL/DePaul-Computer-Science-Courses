#Author: Wilson Wu
#Date:2019.11.05
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/EKms6uNqhPY

import DSC430_Assignment0701_WarAndRandomNumbers as randomnumber
import math 

class Point:

    'Point class"
    
    def __init__(self,x=0,y=0):
        self.x =x
        self.y =y

    def __repr__(self):

        return "Point({},{})".format(self.x,self.y)
    
    def get(self):

        '''get tuple'''

        return self.x,self.y

    def getx(self):

        '''get x value'''

        return self.x

    def gety(self):

        '''get y value'''

        return self.y
    
    
class Ellipse(Point):

    '''Ellipse Class, heritage Point class'''

    def __init__(self,p1=Point(),p2=Point(),width=4):
        self.p1=p1
        self.p2=p2
        self.width=width
    
    def __repr__(self):

        return "Two Points {},{} and width {}".format(self.p1,self.p2,self.width)
    
    def getp1(self):

        ''' get point 1'''
        
        return self.p1
    
    def getp2(self):

        ''' get point 2'''
        
        return self.p2
    
    def getwidth(self):

        return self.width
        


def computeOverlapOfELLipses(e1,e2):
    
    minpointx,maxpointx, minpointy,maxpointy=creatsquare(e1,e2)
    inside,outside,internal=randomnuminsquare(minpointx,maxpointx, minpointy,maxpointy,e1,e2)
    ol=overlap(inside,outside,internal)


    return ol


def creatsquare(e1,e2):

    e1p1x=e1.getp1().getx()
    e1p1y=e1.getp1().gety()
    e1p2x=e1.getp2().getx()
    e1p2y=e1.getp2().gety()

    e1width=e1.getwidth()

    e2p1x=e2.getp1().getx()
    e2p1y=e2.getp1().gety()
    e2p2x=e2.getp2().getx()
    e2p2y=e2.getp2().gety()

    e2width=e2.getwidth()
    
    minpointx = min(e1p1x,e1p2x,e2p1x,e2p2x)-(max(e1width,e2width)/2)
    maxpointx = max(e1p1x,e1p2x,e2p1x,e2p2x)+(max(e1width,e2width)/2)
    minpointy = min(e1p1y,e1p2y,e2p1y,e2p2y)-(max(e1width,e2width)/2)
    maxpointy = max(e1p1y,e1p2y,e2p1y,e2p2y)+(max(e1width,e2width)/2)

    print(minpointx,maxpointx, minpointy,maxpointy)
    
    return minpointx,maxpointx, minpointy,maxpointy

def randomnuminsquare(minpointx,maxpointx, minpointy,maxpointy,e1,e2):

    lengthx= maxpointx-minpointx
    lengthy= maxpointy-minpointy
    print(lengthx,lengthy)
    
    inside=0
    outside=0
    internal=0
    time=0

    pointlst=[]

    e1p1x=e1.getp1().getx()
    e1p1y=e1.getp1().gety()
    e1p2x=e1.getp2().getx()
    e1p2y=e1.getp2().gety()

    e1width=e1.getwidth()

    e2p1x=e2.getp1().getx()
    e2p1y=e2.getp1().gety()
    e2p2x=e2.getp2().getx()
    e2p2y=e2.getp2().gety()

    e2width=e2.getwidth()

    for i in range(1300,11300):
        time+=1
        if time%500==0:
            print( str(time)+'count done')
            
        prng=randomnumber.WarAndPeacePseudoRandomNumberGenerator(i)
        r1=prng.random()
        prng=randomnumber.WarAndPeacePseudoRandomNumberGenerator(100+i)
        r2=prng.random()
        
        p=Point(lengthx*(r1)+minpointx,lengthy*(r2)+minpointy)

        pointlst.append(p)

        
        pf1 = math.sqrt((p.getx()-e1p1x)**2 + (p.gety()-e1p1y)**2)
        pf2 = math.sqrt((p.getx()-e1p2x)**2 + (p.gety()-e1p2y)**2)
        pf3 = math.sqrt((p.getx()-e2p1x)**2 + (p.gety()-e2p2y)**2)
        pf4 = math.sqrt((p.getx()-e2p2x)**2 + (p.gety()-e2p2y)**2)
        
        if pf1+pf2>lengthx and pf3+pf4>lengthx:
            
            outside+=1

        elif pf1+pf2<=lengthx and pf3+pf4<=lengthx:

            internal+=1
            
        else:
            inside+=1
            
    return inside,outside,internal

def overlap(inside,outside,internal):
    total=inside+outside+internal
    ol=internal/total
    print(ol)
    return ol

    

p1=Point()
p2=Point()
e1=Ellipse(p1,p2,4)
p4=Point()
p5=Point()
e2=Ellipse(p4,p5,4)
computeOverlapOfELLipses(e1,e2)

'''
def testinornot(pointlst,e1,lengthx):

    inside=0
    outside=0

    for i in pointlst:
        i=i
        math.sqrt((i.getx()-e1.getp1().getx())**2 + (i.gety()-e1.getp1().gety())**2)
        
        pf1= math.sqrt((i.getx()-e1.getp1().getx())**2 + (i.gety()-e1.getp1().gety())**2)
        pf2 =math.sqrt((i.getx()-e1.getp2().getx())**2 + (i.gety()-e1.getp2().gety())**2)
        
        if pf1+pf2 >lengthx:
            
            
            outside+=1
            
        else:
            
            inside+=1
            
    return inside,outside

'''

     
    
