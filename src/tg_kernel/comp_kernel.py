#!/usr/bin/env python

"""
Create by Carlos A. Gimenez
"""

import os
         
print '+++ Set the environment +++'

os.system('gfortran tg_main.f90 tg_functions.f90 constants.f90 -o tg_kernel.x')

print 'Have fun and enjoy your meal...'
