# hw1.py
# Wilson Wu


# 2.11.

(a)

>>> nums = [-7,-6,-5,-4,-3,-2,-1]
>>> sum(nums)
-28

(b)

>>> ((17*9)+(24*10)+(21*11)+(27*12))//(17+24+21+27) #age are integer 
10

(c)

>>> 2**-20
9.5367431640625e-07

(d)

>>> 4356/61
71.40983606557377

(e)

>>> 4356 % 61
25

# 2.12.

(a)

>>> s1='_'
>>> s2='+'
>>> s1+s2
'_+'

(b)

>>> s1+s2+s1
'_+_'

(c)

>>> s2+(2*s1)
'+__'

(d)

>>> 2*(s2+(2*s1))
'+__+__'

(e)

>>> 10*(s2+(2*s1))+s2
'+__+__+__+__+__+__+__+__+__+__+'

(f)

>>> 5*(s2+s1+(3*s2)+2*(s1))
'+_+++__+_+++__+_+++__+_+++__+_+++__'

# 2.14.

(a)

>>> s = 'goodbye'
>>> 'g' in s[0]
True

(b)

>>> 'g' in s[6]
False

(c)

>>> 'g' in s[:2] and 'a' in s[:2]
False

(d)

>>> 'x' in s[-2]
False

(e)

>>> 'd' in s[3]
True

(f)

>>> s[0] == s[-1]
False

(g)

>>> s[3:] == 'tion'
False

# 2.15.

(a)

>>> len('anachronistically') == len('counterintuitive') + 1
True

(b)

>>> "misinterpretation" < "misrepresentation"
True

(c)

>>> not 'e' in "floccinaucinihilipilification."
True

(d)

>>> len('counter') + len('resolution') == len('counterrevolution')
True

# 2.16.

(a)

>>> a=6
>>> b=7
>>> a
6
>>> b
7

(b)

>>> num= [a,b]
>>> num == [6,7]
True
>>> c= sum(num)/len(num)
>>> c
6.5

(c)

>>> inventory =['paper','staples','pencils']
>>> inventory
['paper', 'staples', 'pencils']

(d)

>>> first = 'John'
>>> middle= 'Fitzgerald'
>>> last = 'Kennedy'
>>> first
'John'
>>> middle
'Fitzgerald'
>>> last
'Kennedy'

(e)

>>> fullname= first+' '+middle+' '+last
>>> fullname
'John Fitzgerald Kennedy'

# 2.18.

(a)

>>> flowers = ['rose', 'bougainvillea', 'yucca', 'marigold', 'daylilly', 'lilly of the valley']
>>> flowers
['rose', 'bougainvillea', 'yucca', 'marigold', 'daylilly', 'lilly of the valley']

(b)

>>> flowers.append('potato')
>>> 'potato' in flowers
True

(c)

>>> thorny = flowers[:3]
>>> thorny
['rose', 'bougainvillea', 'yucca']

(d)

>>> poisonous = flowers[5:]
>>> poisonous
['lilly of the valley']

(e)

>>> dangerous =  thorny + poisonous
>>> dangerous
['rose', 'bougainvillea', 'yucca', 'lilly of the valley']

# 2.19.

(a)

>>> answers = ['Y', 'N', 'N', 'Y', 'N', 'Y', 'Y', 'Y', 'N', 'N', 'N']
>>> numYes =  answers.count('Y')
>>> numYes
5

(b)

>>> numNo = answers.count('N')
>>> numNo
6

(c)

>>> percentYes = numYes/ len(answers)
>>> percentYes
0.45454545454545453

(d)

>>> answers.sort()
>>> answers
['N', 'N', 'N', 'N', 'N', 'N', 'Y', 'Y', 'Y', 'Y', 'Y']

(e)

>>> f = answers.index('Y')
>>> f
6

# 2.20.

(a)

>>> s = 'top'
>>> s[::-1] == 'pot'
True

# 2.26.

(a)

>>> x = 0
>>> y = 0
>>> dart_in_radius = (x -0 )**2 + (y-0)**2 <= 100
>>> dart_in_radius
True

(b)

>>> x= 10
>>> y=10
>>> dart_in_radius = (x -0 )**2 + (y-0)**2 <= 100
>>> dart_in_radius
False

(c)

>>> x=6
>>> y=-6
>>> dart_in_radius = (x -0 )**2 + (y-0)**2 <= 100
>>> dart_in_radius
True

(d)

>>> x=7
>>> y=8
>>> dart_in_radius = (x -0 )**2 + (y-0)**2 <= 100
>>> dart_in_radius
False
