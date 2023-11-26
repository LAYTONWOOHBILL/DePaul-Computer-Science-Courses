#Author: Wilson Wu
#Date:2019.10.16
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/3r_RXBb100s

import random
import DiceCups

def main():
    
    "main function"

    name=greetingUser()

    usermoney=provide100(name)

    ans=asktoplaygame()
    
    while ans==True:
        
        usermoney=game(name,usermoney)
        
        if usermoney <= 0:
            
            print("You don't have money to play! Thank you!")

            ans = False
            
        else:
            ans=asktoplayagain()



def greetingUser():

    '''greeting User and ask name'''
    
    print("Hi! Welcome to the game! Let's gambling!!")

    name = str(input('Please enter your name: ')) #get the name

    return name



def provide100(name):

    '''set user money 100'''
    
    usermoney=100

    print('{}! This is your money: ${}.'.format(name,usermoney))

    return usermoney #get the moeny


def asktoplaygame():

    '''ask for game'''

    ans = str(input('Do you wanna play a dice game?  Plz answer Yes or No! '))

    if ans.upper()=="YES":

        return True
    
    else:

        print('Thank you! Have fun with other game!')

def game(name,usermoney):

    '''game function'''
    
    print('The goal will be a random number between 1 and 100.')
    
    goal = int(random.randint(1,100))   #generate bet number

    bet = askbet()       #ask user bet 
    
    usermoney -= bet    #put the money on the table
    


    a,b,c = askdice()   #ask dice

    cup = DiceCups.Cup(a,b,c)       #call the cup class in DiceCup

    result = cup.roll()             #roll dice
    
    print("The random number is {} and The dice value is {}.".format(goal,result)) #print(goal,dice)
    
    usermoney=report(goal,result,bet,usermoney,name) #show them the result
    
    return usermoney #go back to main loop


def askbet():

    '''ask user bet'''

    bet = int(input('How much would you like to bet?'))

    # no way for -number
    
    while bet < 0 :

        print("Please bet in postive interger number ")

        bet = int(input('How much would you like to bet?'))

    return bet

        
    
def askdice():

    '''ask dice number'''
    
    a=int(input('How many dice would you roll in six sided Dice?'))
    b=int(input('How many dice would you roll in Ten sided Dice?'))
    c=int(input('How many dice would you roll in twenty sided Dice?'))

    return a,b,c


def report(goal,result,bet,usermoney,name):

    '''reveives and result'''

    if result== goal: #value is goal, bet*10
        
        usermoney+=10*bet
        print ("Congrats {}! This is now your total money ${}!".format(name,usermoney))
        return usermoney
    

    elif goal-3<=result<=goal+3: #value between 3, bet*5

        usermoney+=5*bet
        print ("Congrats {}! This is now your total money ${}!".format(name,usermoney))
        return usermoney
    
    elif goal-10<=result<=goal+10:  #value between 10, bet*2

        usermoney+=2*bet
        print ("Congrats {}! This is now your total money ${}!".format(name,usermoney))
        return usermoney

    else: #value not match sad
        print("You can do it better next time {}! This is now your total money ${}!".format(name,usermoney))
        return usermoney


def asktoplayagain():

    '''ask to play again'''

    ans = str(input('Do you wanna play again?  Plz answer Yes or No'))

    if ans.upper()=="YES":
        
        return True

    else:
        print('Thank you! Have fun with other game!')
