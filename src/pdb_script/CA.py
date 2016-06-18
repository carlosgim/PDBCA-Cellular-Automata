
"""

The following is a one dimensional cellular automata program. It creates an array full of zeros and then assign an arbitrary value for one or various of its components, after that some rules are stablished for the system. Finally the evolution is printed as a matrix of ones and zeros.

"""

#-----------------------------------------------------------------------------------------------------------------------
#%%Call modules.
#-----------------------------------------------------------------------------------------------------------------------
import numpy as np
import matplotlib.pyplot as plt


#-----------------------------------------------------------------------------------------------------------------------
#%%Initialize variables.
#-----------------------------------------------------------------------------------------------------------------------

size = input("Please, introduce the size of the grid ")
grid = np.zeros(size)           #Creates a grid full of zeros.
grid[size/2] = 1                #Defines the intermidiate element as one.
#%%Define functions.

def update(grid):               #It updates the initial array.
    temp = np.zeros(size)
    for i in range(1,size-1):   #If the preceeding element is equal to the actual element then the new value is one,  
                                #otherwise is zero. 
        temp[i] =  (grid[i] == grid[i-1] == grid[i+1]) 
    return temp
    
def Print(grid):                #Prints the grid
    '''This function prints list with '1' for TRUE values and ' ' for FALSE values.''' 
    print ''
    for each in grid:
        if each == 1:
            print '1',
        else:
            print ' ',
#-----------------------------------------------------------------------------------------------------------------------
#%%Main programm.
#----------------------------------------------------------------------------------------------------------------------
for i in range(size/2):
  Print(grid)
  grid = update(grid)

