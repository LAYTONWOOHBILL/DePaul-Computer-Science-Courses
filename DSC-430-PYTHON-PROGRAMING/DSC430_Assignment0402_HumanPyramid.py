#Author: Wilson Wu
#Date:2019.10.09
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/6X-pN3L1BMI


def human_Pyramid(Row,Column):
    
    'Add weight human_Pyramid everyone gather from previous person half weight'
    
    Weight=64 #(one feet is 64 pound, and also is the weight u get from just only previous person wieight)
    
    if Row==0 and Column==0: #first one just give wieight and also is my base case to stop the recursive
        return 0

    else:
        
        if Column==0:

            return Weight+(human_Pyramid(Row-1,0))/2
            #for the left side person they only gather one previous person weight
            #example: (1,0)+64



        elif Row==Column:

            return Weight+(human_Pyramid(Row-1,Column-1))/2
            #for the right side person they only gather one previous person weight
            #example: (1,1)+64


        else:

            return (Weight*2)+(human_Pyramid(Row-1,Column-1))/2+(human_Pyramid(Row-1,Column))/2
            #for the middle person they gather two previous person weight
            #example: ((2,1)+64
            #((1,0)+(1,1)+64)+64


R=[human_Pyramid(0,0),human_Pyramid(1,0),human_Pyramid(1,1),human_Pyramid(2,0),human_Pyramid(2,1),human_Pyramid(2,2),human_Pyramid(3,0),
   human_Pyramid(3,1),human_Pyramid(3,2),
   human_Pyramid(3,3),
   human_Pyramid(4,0),
   human_Pyramid(4,1),
   human_Pyramid(4,2),
   human_Pyramid(4,3),
   human_Pyramid(4,4),
   ]
print(R)

