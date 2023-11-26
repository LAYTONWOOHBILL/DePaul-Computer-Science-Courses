class Volume:
    def __init__(self,num=0):
        if num >=11:
            self.num = 11
        elif num <=0:
            self.num = 0
        else:
            self.num =num
    def __repr__(self):
        return 'Volume({})'.format(self.num)
    def set(self,num):
        if num >=11:
            self.num = 11
        elif num <=0:
            self.num = 0
        else:
            self.num =num
    def get(self):
        return self.num
    def up(self,deltanum_up):
        if self.num + deltanum_up >=11:
            self.num = 11
        elif self.num + deltanum_up <=0:
            self.num = 0
        else:
            self.num += deltanum_up
        
    def down(self,deltanum_down):
        if self.num - deltanum_down >=11:
            self.num = 11
        elif self.num - deltanum_down <=0:
            self.num = 0
        else:
            self.num -= deltanum_down
    def __eq__(self,other):
        if self.num == other.num:
             return True
        else:
            return False
        
        
    
def partyVolume(x):
    f = open(x)
    num = eval(f.readline())
    contents = f.readlines()
    v=Volume(num)
    for i in contents:
        a  = i.split()
        if 'U'in a:
            v.up(eval(a[1]))
        else:
            v.down(eval(a[1]))
    return v

            

if __name__=='__main__':
    import doctest
    print( doctest.testfile( 'hw7TEST.py'))
