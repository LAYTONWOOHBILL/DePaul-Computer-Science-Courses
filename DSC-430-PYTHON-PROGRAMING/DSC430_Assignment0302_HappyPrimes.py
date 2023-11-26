#Author: Wilson Wu
#Date:2019.10.08
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/JEKjdVh0H3o
#import math

def greetinguser():
    '''greeting'''
    print("Hey, what's up now you are trying to find an happy number!!")



def inputnum():
    '''asks user number'''
    n = int(input('Please enter an integer positive number for finding happy number!'))
    return n



def testprime(n):
    '''test prime or not'''
    for i in range(2,n):
        if n%i==0:
            return False   #retrun 0 if num is not prime 
    return True


    
def testhappy(n):
    '''test happy number'''
    dic={} #creat a dictionary to help search back when its in not happy number loop" 
#    speratenum(n) #put in speratenum and create new num
    while n !=1: #when the num not be 1 still in th loop
        if n not in dic:  #test is the new genrate number in dic 
            newnumber=speratenum(n) #put in speratenum and create new num and set newnumber is new genrate number in dic
            dic[n]=newnumber #append into dictionary for future test
            n=newnumber #set newnumber into newn umber to continue searching
        else:
            return False #if n in the dictionary then means number try to reapet agian 
#    print(dic) 
    return True #out loop n=1


    
def speratenum(n):
    '''sperate number and squared then plus to new number'''
    newnumber=0
    item = list(str(n))
    for i in item:
        newnumber+=int(i)**2 #squared then plus to new number
    return newnumber

    
    

def printout(valuetestprime,valuetesthappy):
    '''print out the value'''
    if valuetestprime == True:
        if valuetesthappy == True:
            print("It is a happy prime")
        else :
            print("It is a sad prime")
    else:
        if valuetesthappy == True:
            print("It is a happy non-prime")
        else:
            print("It is a sad non-prime")

            

def querytoexit():
    '''ask to quit or not'''
    q = str(input('Do u still wanna use? Plz answer yes or no. Thank you!'))
    if q.upper() =='NO':
        print('Thank you!')
    else:
        return True

    

def main():
    '''main fuction for happy number'''
    greetinguser()
    q =True
    while q == True:
        n =inputnum()
        valuetestprime=testprime(n)
        valuetesthappy=testhappy(n)
        printout(valuetestprime,valuetesthappy)
        q = querytoexit()
        
    
