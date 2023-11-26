#hw8.py
#Wilson Wu

class Pizza:

    def __init__ (self,size= str('M') ,topping=set()):
        self.size = str(size)
        self.topping= set(topping)
        
    def __repr__(self):
        return "Pizza('{}',{})".format(self.size,self.topping)

    def setSize (self,size):
        self.size = str(size)
    def getSize (self):
        return self.size
    def addTopping(self,topping):
        self.topping.add(topping)
    def removeTopping(self,topping):
        self.topping.remove(topping)
    def price(self):
        if self.size =='S':
            return 6.25 + 0.7* len(self.topping)
        elif self.size =='M':
            return 9.95 + 1.45* len(self.topping)
        else:
            return 12.95 + 1.85* len(self.topping)
    def __eq__(self,other):
        return self.size==other.size and self.topping ==other.topping
            

def orderPizza():
    print('Welcome to Python Pizza!')
    size = input('What size pizza would you like (S,M,L): ')
    p = Pizza(size)
    while True:
        topping = input('Type topping to add (or Enter to quit): ')
        if topping =='':
            break
        p.addTopping(topping)
    print('Thanks for ordering!')
    print('Your pizza costs $' + str(p.price()))
    return p


class Stack(list):

    def __repr__(self):
        return "Stack({})".format(list.__repr__(self))
    
    def push(self,item):
        self.append(item)
        
    def isEmpty(self):
        return self ==[]

class Stack(list):

    def __repr__(self):
        return "Stack({})".format(list.__repr__(self))
    
    def push(self,item):
        self.append(item)
        
    def isEmpty(self):
        return self ==[]

        
def parenthesesMatch(lst):
    s = Stack()
    for l in lst:
        if l in '({[':
            s.push(l)
        elif l in ')}]' and s.isEmpty()== True:
            return False
        elif l in ')}]' and s.isEmpty()== False:
            if l ==')' and '(' in s:
                s.pop()
            elif l =='}' and '{' in s:
                s.pop()
            elif l ==']' and '[' in s:
                s.pop()
    return s.isEmpty()
        

if __name__=='__main__':
    import doctest
    print( doctest.testfile( 'hw8TEST.py'))



