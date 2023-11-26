#Author: Wilson Wu
#Date:2019.10.08
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/ZUsmskYheeY

def creatprimelstfor2toanynum(num):
    '''collect all prime num that below the the max range num into list'''
    primelst=[]
    for i in range(2,num+1):
        if prime(i) is True:            #prime num will be append in list
            primelst.append(i)
    return primelst





def prime(n):
    '''test num is prime or not'''
    for i in range(2,n):
        if n%i==0:
            return False                 #retrun false if num is not prime 
    return True



    
def findprimefornum(numrange,primelst):
    ''' find the fist Goldbach Conjection and put it in to dictionary'''
    goldbach_conjection={}                          #create dictionary
    for i in range(2,numrange+1,2):                 #how many even number user want to find Goldbach Conjection
        if i == 2:                                  # for 2 i just put in directly,
            goldbach_conjection[i]=[1,1]            #because in future Goldbach Conjection for i don't want 1 show up  
        else:
            for prime1 in primelst:                         # iterate every prime number below max num
                for prime2 in primelst:                     # iterate every prime number below max num
                    if i == prime1 + prime2:                # test for find 2 prime number puls equal to the even number 
                        if i in goldbach_conjection:
                            break                           #only find the first one
                        else:
                            goldbach_conjection[i]=[prime1,prime2]   #put it into dictionary if no exist in yet. 
    return goldbach_conjection






def iteratenum(goldbach_conjection):
    '''print out the Goldbach Conjection'''
    for evennum in goldbach_conjection.keys():
        print('{:2} = {:2}'.format(str(evennum),str(goldbach_conjection.get(evennum)).replace('[','').replace(']','').replace(',',' +'))) #try to print it in fromal way




        
def main(num):
    primelst=creatprimelstfor2toanynum(num)
    goldbach_conjection=findprimefornum(num,primelst)
    iteratenum(goldbach_conjection)


#main(100)
