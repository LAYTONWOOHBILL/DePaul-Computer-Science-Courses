# hw3.py
# Wilson Wu


#Required exercises: 4.24, 4.25, 4.26, 4,28, 4.31


###4.24

def cheer(x):
    s1 = "How do you spell winner?\nI know, I know!"
    s2 = '\n'+x
    s3="!\nAnd that's how you spell winner!\nGo {}!".format(x)
    print(s1)
    for word in x.upper():
        print(word, end=' ' )
    print(s3)
    
#4.25
    
def vowelCount(word):
    s = 'a, e, i, o, and u appear, respectively, {}, {}, {}, {}, {} times.'.format(word.count('a'),word.count('e'),word.count('i'),word.count('o'),word.count('u'))
    print(s)

#4.26
    
def crypto(file):
    infile = open( file,'r')
    contents = infile.read().replace('secret','xxxxxx')
    infile.close()
    print(contents)



#4.28
def links(html):
    infile = open(html,'r')
    contents = infile.read()
    num = contents.split().count('link')
    infile.close()
    return(num)

#4.31
    
def duplicate(filename):
    infile = open(filename,'r')
    contents = infile.read().split()
    infile.close()
    for word in contents:
        dup = contents.count(word)
        if dup != 1:
            return (True)
    
    return (False)


if __name__=='__main__':
    import doctest
    print( doctest.testfile('hw3TEST.py'))
