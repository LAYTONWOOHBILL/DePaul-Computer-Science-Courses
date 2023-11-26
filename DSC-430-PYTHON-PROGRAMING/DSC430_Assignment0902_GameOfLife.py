#Author: Wilson Wu
#Date:2019.11.13
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/5KUrZdbrjqQ

#Import NumPy
import numpy as np
import random 

def conway(s,p):
    
    numlist=[] #create a 1,0 list

    boardnum=s**2  #total number we have
    
    probability = round((s**2)*p) #count probability
    
    for zero in range((boardnum-probability)):  #generate 0

        numlist.append(0)

    for one in range(probability):              #generate 1

        numlist.append(1)

    random.shuffle(numlist)                     #shuffle numlist
        
    array = np.asarray(numlist).reshape(s,s)    #shape it

    print(array)

    return array

def advance(b,t):


    for time in range(t):    #run times

        b = np.pad(b, pad_width=1, mode='constant', constant_values=0)  #create surronding


        new_num_list=[]   # new array list
    
        size= b.shape[0]  #size

        for i in range(1,size):     #run one by one
            for j in range(1,size): #run one by one
            
                if b[i][j] == 0:       #different rules tes 

                    try:                    #run neighbor
                        cell1=b[i-1][j]  
                        cell2=b[i+1][j]
                        cell3=b[i][j-1]
                        cell4=b[i][j+1]

                        if cell1+cell2+cell3+cell4==3:    #alive
                        
                            new_num_list.append(1)    

                        else:
                        
                            new_num_list.append(0)
                        
                    except:
                        pass
                        
                elif b[i][j] == 1:
                
                    try:
                        cell1=b[i-1][j]
                        cell2=b[i+1][j]
                        cell3=b[i][j-1]
                        cell4=b[i][j+1]

                        if 2<=cell1+cell2+cell3+cell4<=3:
                        
                            new_num_list.append(1)

                        else:
                            new_num_list.append(0)
                        
                    except:
                        pass
                        

        b = np.asarray(new_num_list).reshape(size-2,size-2)  #form the board
        print(b)

    print(b)

def main():

    b=conway(6,0.4)
    
    advance(b,10)
    



    
