#Author: Wilson Wu
#Date:2019.09.18
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/G8p5HrCXYKM


def fuction():
    """this is Grading Logic fuction using while loop!"""
    grade = 0
    aws1=str(input("Are you submit a single uncompressed .py file? Plz answer YES or NO. "))  #input type to str
    while True:         #Because the question is step by step. So write while loop in each question, three way statement.
        if aws1.upper() == "NO":        # assignment is not submitted as a single uncompressed
            print('You grade are')
            return grade                #int
        elif aws1.upper() !="YES":      # only allow yes or no
            print('Plz answer YES or NO.')
            aws1=str(input("Are you submit a single uncompressed .py file? Plz answer YES or NO. "))    # stay in while loop
        else:
            aws2=str(input("Do you include the author’s name and date in file? Plz answer YES or NO. ")) # if yes to next question.
            while True:
                if aws2.upper() == "NO":
                    print('You grade are')
                    return grade
                elif aws2.upper() !="YES":  # only allow yes or no
                    print('Plz answer YES or NO.')
                    aws2=str(input("Do you include the author’s name and date in file? Plz answer YES or NO. "))     #stay in while loop
                else:
                    aws3=str(input("Do you include the honor statement in file? Plz answer YES or NO. "))
                    while True:
                        if aws3.upper() == "NO":
                            print('You grade are')
                            return grade
                        elif aws3.upper() !="YES":       # only allow yes or no
                            print('Plz answer YES or NO.')
                            aws3=str(input("Do you include the honor statement in file? Plz answer YES or NO. "))    #stay in while loop
                        else:
                            aws4=str(input("Do you include a link to an unlisted 3-minute YouTube video in file? Plz answer YES or NO. "))
                            while True:
                                if aws4.upper() == "NO": 
                                    print('You grade are')
                                    return grade
                                elif aws4.upper() !="YES":  # only allow yes or no
                                    print('Plz answer YES or NO.')
                                    aws4=str(input("Do you include a link to an unlisted 3-minute YouTube video in file? Plz answer YES or NO. "))   #stay in while loop
                                else:    # pass all four request, go to scoring system
                                    aws5=eval(input("Out of ten points, how would you evaluate the correctness of the code? Plz just answer Number."))  #input type to int
                                    grade += aws5           # add evaluate grade
                                    aws6=eval(input("Out of ten points, how would you evaluate the elegance of the code? Plz just answer Number."))
                                    grade += aws6
                                    aws7=eval(input("Out of ten points, how would you evaluate the code hygiene? Plz just answer Number."))
                                    grade += aws7
                                    aws8=eval(input("Out of ten points, how would you evaluate the quality of the discussion in the YouTube video? Plz just answer Number."))
                                    grade += aws8
                                    aws9=str(input("Do you submit your assignments late? Plz answer YES or NO. "))
                                    while True:
                                        if aws9.upper() == "NO":
                                            print('You grade are')
                                            return grade    #int
                                        elif aws9.upper() !="YES":  # only allow yes or no
                                            print('Plz answer YES or NO.')
                                            aws9=str(input("Do you submit your assignments late? Plz answer YES or NO. "))   #stay in while loop
                                        else:
                                            aws10=eval(input("How many hour have you been late for submit? Plz just answer Number."))
                                            print('You grade are')
                                            return grade-(grade*(aws10*1*1/100)) #calculate delay grde
                    



            
        
