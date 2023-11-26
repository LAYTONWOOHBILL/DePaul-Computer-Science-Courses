# hw4.py
# Wilson Wu

#5.15
def vowels(word):
    for i in range(len(word)):
        if word[i] in 'aeiouAEIOU':
            print(i) 
#5.16

def indexes(word,character):
    lst=[]
    for i in range(len(word)):
        if word[i] == character:
            lst.append( i )
    return lst
    

#5.18

def four_letter(lst):
    fourletter=[]
    for word in lst:
        if len(word) == 4:
            fourletter.append(word)
    return fourletter 
#5.26

def rps(p1, p2):
    if p1+p2 in ['PR','SP','RS']:
        return -1
    elif p1+p2 in ['SS','PP','RR']:
        return 0
    return 1
    
    
#5.39

def exclamation(word):
    vowelrepeat =''
    for i in word:
        if i in 'aeiouAEIOU':
            vowelrepeat +=4*i
        elif i not in 'aeiouAEIOU':
            vowelrepeat +=i
    return vowelrepeat +'!'
    
if __name__=='__main__':
    import doctest
    print( doctest.testfile('hw4TEST.py'))
