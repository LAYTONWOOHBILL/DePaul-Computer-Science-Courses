# hw5.py
# Wilson Wu

#5.25

def leap(year):
    if year%4 != 0:
        return False
    while year% 4 == 0:
        if year % 100 != 0 or year % 400 == 0:
            return True
        else:
            return False 
           
#5.28
        
def geometric(numlst):
    for i in range(len(numlst)-2):
        if numlst[i+1]/numlst[i]==numlst[i+2]/numlst[i+1]:
            return True
    return False

    
#5.34

def statement(numlst):
    deposits = 0
    withdraw = 0
    for num in numlst:
        if num >= 0:
            deposits += num
        else:
            withdraw += num
    return [deposits, withdraw]

#5.35

def pixels(dimlst):
    size=0
    for numlst in dimlst:
        for num in numlst:
            if num>0:
                size +=1
    return size
            
#5.36

def prime(num):
    for i in range(2,num):
        if num % i == 0:
            return False
    return True
    

if __name__=='__main__':
    import doctest
    print( doctest.testfile('hw5TEST.py'))
