#Author: Wilson Wu
#Date:2019.10.09
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/xjzg69cWRm0

import random
        
def main():
    "main fuction"
    q =True
    
    while q == True:
        
        i,num=inputlstandnum()

        lst=creatlist(i,num)

        num_conjection=findconjectionfornum(num,lst)

        iteratenum(num_conjection)

        q = querytoexit()


#main()

def inputlstandnum():
    '''asks user for list and number'''
    
    num = int(input('Please enter an integer positive number for finding!'))
    
    i = int(input('Please enter an integer positive number for list!'))
    
    return i,num



def creatlist(i,num):
    '''create number list for find conjection later'''
    lst = [random.randrange(1, num, 1) for item in range(i)]
    
    lst=sorted(lst)

    print('This is the random integer list u create'+ str(lst))

    return lst



def findconjectionfornum(num,lst):

    'find conjection by using dictionary '

    num_conjection={}

    for n1 in lst:                      #set the first num

        n2=binarysearch(num-n1,lst)     #then use binary search to find other half

        if n2==False:

            pass                        #if not go next

        else:

            num_conjection[num]=[n1,n2] #if yes put in dictionary

    return num_conjection 



def binarysearch(x, nums):
    '''binary search the target'''

    low = 0

    high = len(nums) - 1

    while low <= high:                  # if still in range to search

        mid = (low + high)//2           # Position of middle item

        item = nums[mid]

        if x == item:                   

            return nums[mid]            # Found it return the number

        elif x < item:                  # x is in lower half of range

            high = mid - 1              # move top marker down

        else:

            low = mid + 1

    return False



def iteratenum(num_conjection):

    '''print out the Conjection'''

    if num_conjection=={}:

        print('sorry')

    else:   

        for evennum in num_conjection.keys():

            print('{:2} = {:2}'.format(str(evennum),str(num_conjection.get(evennum)).replace('[','').replace(']','').replace(',',' +'))) #try to print it in fromal way




def querytoexit():

    '''ask to quit or not'''

    q = str(input('Do u still wanna use? Plz answer yes or no. Thank you!'))

    if q.upper() =='NO':

        print('Thank you!')

    else:

        return True
