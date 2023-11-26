#Author: Wilson Wu
#Date:2019.11.09
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/KMJsHnDPlyU

import random

from DSC430_Assignment0801_PlotGenerator import SimplePlotGenerator, RandomPlotGenerator, InteractivePlotGenerator


class PlotViewer:

    def __init__(self):
         ''' init'''   
        
        pass

    def registerPlotGenerator(self,pg):

        '''register the view'''

        self.pg = pg
        self.pg.registerview()
        
        
    def generate(self):

        '''generate'''

        return  self.pg.generate()



'''
        if self.pg == SimplePlotGenerator():
            
            return  self.pg.registerview(1)

        else:

            return self.pg.registerview(0)
'''
