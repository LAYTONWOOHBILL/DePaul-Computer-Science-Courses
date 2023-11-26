#Author: Wilson Wu
#Date:2019.10.29
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/9HbIy4vs-FA

import csv
import statistics
import math

def main():
    variablename='Total Volume'
    mean_SM=readAndCoumputeMean_SM(variablename)
    sd_SM=readAndCoumputeSD_SM(variablename)
    median_SM=readAndCoumputeMedian_SM(variablename)
    mean_HG=readAndCoumputeMean_HG(variablename)
    sd_HG=readAndCoumputeSD_HG(variablename)
    median_HG=readAndCoumputeMedian_HG(variablename)
    mean_MML=readAndCoumputeMean_MML(variablename)
    sd_MML=readAndCoumputeSD_MML(variablename)

    print('    mean            sd              median')
    print('SM  {}      {}      {}'.format(mean_SM,sd_SM,median_SM))
    print('HG  {}      {}      {}'.format(mean_HG,sd_HG,median_HG))
    print('MMl  {}      {}      {}'.format(mean_MML,sd_MML,mean_MML))
    
def readfile_totalvolume(variablename):
    '''readfile'''
    totalvolume_lst=[]
    file='avocado.csv'
    infile = open(file, 'r')
    readline = infile.readline()
    columnlist = readline.split(',')
    index=columnlist.index(variablename)
    for i in range(0,18249):
        readline = infile.readline()
        datalist = readline.split(',') #split in to list
        totalvolume_lst.append(eval(datalist[index])) #append number to list
    #print(totalvolume_lst[:10])
    infile.close()
    return totalvolume_lst


#statistics function

def readAndCoumputeMean_SM(variablename):
    totalvolume_lst=readfile_totalvolume(variablename)
    mean_SM= statistics.mean(totalvolume_lst)
#    print(mean_SM)
    return mean_SM

def readAndCoumputeSD_SM(variablename):
    totalvolume_lst=readfile_totalvolume(variablename)
    sd_SM= statistics.stdev(totalvolume_lst)
#    print(sd_SM)
    return sd_SM

def readAndCoumputeMedian_SM(variablename):
    totalvolume_lst=readfile_totalvolume(variablename)
    median_SM= statistics.median(totalvolume_lst)
#    print(median_SM)
    return median_SM

#Home Grown

def readAndCoumputeMean_HG(variablename):
    totalvolume_lst=readfile_totalvolume(variablename)
    total=0
    for num in totalvolume_lst:
        total+=num #count
    mean_HG=total/len(totalvolume_lst)
#    print(mean_HG)
    return mean_HG

def readAndCoumputeSD_HG(variablename):
    totalvolume_lst=readfile_totalvolume(variablename)
    mean_HG=readAndCoumputeMean_HG(variablename)
    sumofsquare=0
    for num in totalvolume_lst:
        sumofsquare+=(num-mean_HG)**2  #sdv
    sd_HG=math.sqrt((1/(len(totalvolume_lst)-1)*sumofsquare))
#    print(sd_HG)
    return sd_HG

def readAndCoumputeMedian_HG(variablename):
    totalvolume_lst=readfile_totalvolume(variablename)
    totalvolume_lst.sort()
    lenlist=int(len(totalvolume_lst))
    if lenlist%2==0:
        median_HG=(totalvolume_lst[(lenlist/2)-1]+totalvolume_lst[(lenlist/2)])/2
#        print(median_HG)
        return median_HG
    else:
        median_HG=totalvolume_lst[(lenlist//2)]
#        print(median_HG)
        return median_HG

#Memory Only a single
    
def readAndCoumputeMean_MML(variablename):
    total=0
    count=0
    file='avocado.csv'
    infile = open(file, 'r')
    readline = infile.readline()
    columnlist = readline.split(',')
    index=columnlist.index(variablename)
#    print(index)
    readline = infile.readline()
    while True:
        # read line
        datalist = readline.split(',')
        total+=eval(datalist[index])
        count+=1
        readline = infile.readline()
        # check if line is not empty
        if not readline:
            break
    mean_MML=total/count
    infile.close()
    return mean_MML


def readAndCoumputeSD_MML(variablename):
    mean_MML=readAndCoumputeMean_MML(variablename)
    sumofsquare=0
    count=0
    file='avocado.csv'
    infile = open(file, 'r')
    readline = infile.readline()
    columnlist = readline.split(',')
    index=columnlist.index(variablename)
#    print(index)
    readline = infile.readline()
    while True:
        datalist = readline.split(',')
        sumofsquare+=(eval(datalist[index])-mean_MML)**2
        count+=1
        readline = infile.readline()
        # check if line is not empty
        if not readline:
            break
    sd_MML=math.sqrt((1/(count-1)*sumofsquare))
    infile.close()
    return sd_MML

#readAndComputeMedian_MML(“Total Volume”)
#Author: Wilson Wu
#Date:2019.11.25
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/zlzW83CNz6s  # I combine 2 video together 

import csv
import statistics
import math

def readfile_totalvolume(variablename):
    '''readfile'''
    totalvolume_lst=[]
    file='avocado.csv'
    infile = open(file, 'r')
    readline = infile.readline()
    columnlist = readline.split(',')
    index=columnlist.index(variablename)
    for i in range(0,18249):
        readline = infile.readline()
        datalist = readline.split(',') #split in to list
        totalvolume_lst.append(eval(datalist[index])) #append number to list
    #print(totalvolume_lst[:10])
    infile.close()
    return totalvolume_lst

def find_min_max(lst):

    a = min(lst)
    b = max(lst)
    c = len(lst)

    return a,b,c

def binary(mid,minnum,maxnum,variablename):
    count1=0
    count2=0
    midnum=maxnum
    file='avocado.csv'
    infile = open(file, 'r')
    readline = infile.readline()
    columnlist = readline.split(',')
    index=columnlist.index(variablename)
#    print(index)
    readline = infile.readline()
    while True:
        # read line
        datalist = readline.split(',')
        data=eval(datalist[index])
        if data>mid:
            count2+=1
            if minnum<data<maxnum:
                midnum=data
            readline = infile.readline()
            if not readline:
                    break
        else:
            count1+=1
            if minnum<data<maxnum:
                midnum=data
            readline = infile.readline()
            if not readline:
                break
    infile.close()
    print(count2, count1, midnum)
    return count2, count1,midnum

def readAndCoumputeMedian_SM(variablename):
    totalvolume_lst=readfile_totalvolume(variablename)
    median_SM= statistics.median(totalvolume_lst)
#    print(median_SM)
    return median_SM
    
    

def main():
    variablename='Total Volume'
    lst=readfile_totalvolume(variablename)
    minnum,maxnum,count=find_min_max(lst)
    mid=(minnum+maxnum)/2
    count2, count1, midnum =binary(mid,minnum,maxnum,variablename)
    turevalue=readAndCoumputeMedian_SM(variablename)
    while count1!=9124:
        if count1>count/2:
            maxnum=mid
            mid=(minnum+maxnum)/2
            count2, count1,midnum =binary(mid,minnum,maxnum,variablename)
        else:
            minnum=mid
            mid=(minnum+maxnum)/2
            count2, count1,midnum =binary(mid,minnum,maxnum,variablename)
            
    return midnum,turevalue

