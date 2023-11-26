#Author: Wilson Wu
#Date:2019.09.18
#Honor statement: “I have not given or received any unauthorized assistance on this assignment.”
#Video link: https://youtu.be/05gHpOzmghQ
    
def coprime(a,b):
    """returns both numbers prime factor, then compare them"""
    factor_a=[]
    factor_b=[]
    if a == 1:
        return True #if the number is 1 that must be the coprime 
    elif b == 1:
        return True
    else:    
        for num in range(2,a+1): #test all the number that less than enter number
            while a % num == 0:
                factor_a.append(num) #if the number is factor, append into factor list, actually it will be prime factors list.
                a=a/num             #number divide the factor and looping
        for num in range(2,b+1):
            while b % num == 0:
                factor_b.append(num)
                b=b/num
    for prime in factor_a:      #test two number have same common factor
        if prime in factor_b:
            return False
    return True
    
def coprime_test_loop(): 
    """test two nums are coprime or not. User can use this untill they want to stop"""
    a = eval(input("Enter your first number:"))     #input num
    b = eval(input("Enter your second number:"))    #input num
    while True:                                     #while loop, because we don't know how many times user play, two ways statement.
        if coprime(a,b) is True:                    #put num into coprime(a,b),if ture
            print("Two numbers are coprime")        #print(statement)
            keepplay = str(input("Do you still want to continue? Plz answer YES or NO. "))  #input srt for continue!
            if keepplay.upper() == "YES":
                a = eval(input("Enter your first number:"))
                b = eval(input("Enter your second number:"))
                coprime(a,b)                                                                #go back start.
            else:
                print("Thank you and Good Bye!!!")                                           #exit loop
                break
        else:                                       #put num into coprime(a,b),if False
            print("Two numbers are not coprime")    #print(statement)
            keepplay = str(input("Do you still want to continue? Plz answer YES or NO. "))  #input srt for continue!
            if keepplay.upper() == "YES":
                a = eval(input("Enter your first number:"))
                b = eval(input("Enter your second number:"))
                coprime(a,b)
            else:
                print("Thank you and Good Bye!!!")                                              #exit loop
                break
        
    
        
    


