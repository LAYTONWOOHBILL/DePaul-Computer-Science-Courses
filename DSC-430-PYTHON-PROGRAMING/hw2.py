# hw2.py
# Wilson Wu



#Problems:3.23 part (f) - make this the body of a function named forLoops(),
#3.28 - make this the body of a function called squares
#(see sample interaction below),
#3.34, 3.38, 3.39, 3.40, 3.41 

# 3.23 part (f)

def forLoops():
    for num in range(5,22,4):
        print(num)

###3.28

def squares():
    n = eval(input('Enter n: '))
    for num in range(n): 
        print(num**2)

#3.34
    
def pay(wage, time):
    if time > 40:
        return (wage * (40+ (time-40) *1.5))
    else:
        return (wage* time)

#3.38

def abbreviation(x):
    return(x[:2])
    
#3.39

def collision(x1,y1,r1,x2,y2,r2):
    if (x2-x1)**2 + (y1-y2)**2 <= (r1+r2)**2:
        return True
    else:
        return False

#3.40

def partition( namelist ):
    for name in namelist:
        if name[0] in 'ABCDEFGHIJKLM':
            print(name)

#3.41
            
def lastF(FirstName, LastName):
    return (LastName +', '+FirstName [0] + '.')

        
if __name__ == '__main__':
    import doctest
    print( doctest.testfile( 'hw2TEST.py' )







           )
