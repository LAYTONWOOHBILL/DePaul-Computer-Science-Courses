#Author: Wilson Wu
#Date:2019.09.30
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/k7IeMBxVJos

#main
def main():
    '''main fuction for stemand leaf'''
    greetuser()
    q = True  #if q =ture, then will keep run the loop.
    while q == True:
        n = getuserimput()
        listofnum = readfile(n)
        stemandleaf(listofnum)
        q = querytoexit()

#level 2
def greetuser():
    '''greeting'''
    print('Hi, you are joining stem-and-leaf plot now.')

def getuserimput():
    '''# Asks the user to input a 1, 2 or 3.'''
    n = int(input('Please enter 1,2 or 3 to read the file.')) #change the type to 
    return n
    
def readfile(n):
    '''read the file for user'''
    filename = "/Users/laytonwoohbill/Desktop/Depaul/2019 - fall/DSC 430/02/StemAndLeaf"+str(n)+".txt" #use path = '' 
    listofnum=[]            #list for numberlist
    infile = open(filename, "r")
    lineList = infile.readlines()
    for i in range ( 0, len(lineList) ):    #read one by one
        x = int(lineList[i].strip())        #remove empty space and change string type to int
        listofnum.append(x)                 #append into list
#    print(listofnum)
    return listofnum

#level 3
def stemandleaf(listofnum):
    '''stem and leaf fuction, sort data, find min, max and process the fuction'''
    listofnum.sort()                        #sort num in list 
    minnum = min(listofnum)                 #find min
    maxnum = max(listofnum)                 #find max
    #print(minnum,maxnum)
    bp=findbreakpoint(minnum,maxnum)
    dic=splitlist(listofnum,bp)
    fomart(dic)
'''
def findbreakpoint(minnum,maxnum):
    ''''''breakpoint decide''''''
    if maxnum // minnum < 10:
        bp = 10
        return bp
    else:
        bp = 100
        return bp
'''
def findbreakpoint(minnum,maxnum):
    '''breakpoint decide'''
    maxunit=0
    minunit=0
    while maxnum > 1:            #find digits
        maxnum = maxnum// 10
        maxunit+=1
    while minnum>1:
        minnum = minnum// 10
        minunit+=1
    if maxunit-minunit==0:      #digits difference 
        bp = 10                 #where digits to be split
        return bp
    else:
        bp=10**(maxunit-minunit) 
        return bp
    
def splitlist(listofnum,bp):
    '''split stem and leaves'''
    dic={}
    for n in listofnum:
        stem = n//bp
        leaves = n % bp        #digit split
        if stem not in dic:
            dic[stem]=[leaves]
        else:
            dic[stem].append(leaves)
#    print(dic)
    return dic

def fomart(dic):
    ''' picture stem and leaf'''
    for n in dic.keys():
        print(str(n)+' | '+ str(dic.get(n)).replace('[','').replace(']',''))   #format

#level 2
def querytoexit():
    '''ask to quit or not'''
    q = str(input('Do u still wanna use? Plz answer yes or no. Thank you!'))
    if q.upper() =='NO':
        print('Thank you!')
    else:
        return True



