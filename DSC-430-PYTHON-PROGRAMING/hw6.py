# hw6.py
# Wilson Wu

#5.38

def collatz(num):
    x= num
    print(int(x))
    while True:
        if x%2 == 0:
            x=x/2
            print(int(x))
        elif x==1:
            break
        else:
            x=3*x+1
            print(int(x))
            
#5.42
            
def primeFac(num):
    #create an empty dict
    pf = []
    for i in range(2,num+1):
       while num %i == 0:
           num = num/i
           pf.append(i)
           #print(num)
    return pf
        
            
         
#5.48

def sublist(lst1,lst2):
    i=0
    j=0
    while i+1<=len(lst1) and j+1 <=len(lst2):
        if lst1[i] == lst2[j]:
            i+=1
            j+=1
        else:
            j+=1
    if i == len(lst1):
        return True 
    else:
        return False

#6.22
def mirror(word):
    ans=''
    code ={'b':'d','d':'b'}
    if word [::-1]!= ans:
        for i in word[::-1]:
            if i in code:
                ans+=code[i]
            else:
                ans+=i
        if ans == word:
            return 'INVALID'
    return ans


#6.30
        
def rps(p1, p2):
    if p1+p2 in ['PR','SP','RS']:
        return -1
    elif p1+p2 in ['SS','PP','RR']:
        return 0
    return 1

import random

def simul(n):
    p1score= 0
    p2score= 0
    for i in range(n):
        p1 = random.choice('PRS')
        p2 = random.choice('PRS')
        #print (i,p1,p2,rps(p1,p2))
        if rps(p1,p2) == 1:
            p1score+= 1
        elif rps(p1,p2) == 0:
            pass
        else:
            p2score+= 1
    if p1score > p2score:
        print('Player 1')
    elif p1score < p2score:
        print('Player 2')
    else:
        print('Tie')
    
            

#6.31
     
def craps():
    d1 = random.randrange(1,7)
    d2 = random.randrange(1,7)
    dstart =d1+d2
    d = d1+d2
    #print(d,dstart, d1, d2)
    while True:
        if dstart in [7,11]:
           return 1
        elif dstart in [2,3,12]:
            return 0
        while dstart not in [2,3,7,11,12]:
            d1 = random.randrange(1,7)
            d2 = random.randrange(1,7)
            d = d1+d2
            #print(d, d1, d2)
            if d == 7:
                return 0
            elif d == dstart:
                return 1
        
def testCraps(n):
    total= 0
    for i in range(n):
        if craps() == 1:
            total +=1     
    return total/n


if __name__=='__main__':
    import doctest
    print( doctest.testfile('hw6TEST.py'))
