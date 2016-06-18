#!/usr/bin/env python

"""
Create by Carlos A. Gimenez
"""

import os
         
print '+++ Push all my stuff +++'

os.system('git add -A')

msens = raw_input('Please write a nice message here: ')

mensaje = 'git commit -m '+ "'msens'"

os.system(mensaje)

pull = "git pull origin Franco"

push = 'git push origin Franco'

os.system(pull)

os.system(push)

print 'Franco..Have fun and enjoy your meal...'
