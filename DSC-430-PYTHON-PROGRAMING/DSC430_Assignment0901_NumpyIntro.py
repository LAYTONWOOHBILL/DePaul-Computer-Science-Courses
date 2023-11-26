#Author: Wilson Wu
#Date:2019.11.13
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/5KUrZdbrjqQ

#Import NumPy
import numpy as np

#Use arange to create a NumPy array with 100 equally spaced values
#in the range 0 through 100 (not including 100). Name this NumPy array a.

a= np.arange(100)
print(a)

#Use arange to create a NumPy array with 10 equally spaced values
#in the range 0 through 100 (not including 100). Name this NumPy array b.

b=np.arange(0,100,10)
print(b)

#Use linspace to create a NumPy array in the range 0 through 10 (inclusive)
#with values spaced at 0.1. Call this NumPy array c.

c= np.linspace(0., 10., num=100)
print(c)

#Create a random two-dimensional array with the dimensions 10 by 10. Call this NumPy array d.

d=np.random.random((10,10))
print(d)

#Reshape a so that it is a two-dimensional array with the dimensions 10 by 10.

a=a.reshape(10,10)
print(a)

#Show the results of “a[4,5]”

a45=a[4,5]

print(a45)

#Show the results of “a[4]”.

a4=a[4]

print(a4)

#Show the sum of d.

dsum = d.sum()
print(dsum)

#Show the max of a.

amax = a.max()
print(amax)

#Show the transpose of b.

bt = b.transpose()

print(bt)

#Show the results of adding a and d.

ad =a+d

print(ad)

#Show the results of multiplying a and d.

axd=a*d

print(axd)

#Show the results of computing the dot product of a and d.

addot=np.dot(a,d)
print(addot)






