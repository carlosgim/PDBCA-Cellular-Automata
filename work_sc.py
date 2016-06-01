#!/usr/bin/env python

"""
Create by Carlos A. Gimenez
"""

import os
         
WAY = raw_input('Who are you? Santi = 1, Franco = 2 : ')

if (WAY == '1'):
  branch = 'Santiago'
else:
  branch = 'Franco'

WWD = raw_input('What do you whant to do? Update = 1, Upload = 2, Both = 3 , Initial Set = 4 :  ')
print branch
print WWD

if (WWD == '4'):

  print '+++ Set the environment +++'

  os.system('git remote add upstream https://github.com/dcamposliz/dataScience_stockMarket.git')

  os.system('git remote -v')

elif (WWD == '1'):

  print '+++ Fetch the branch +++'

  os.system('git fetch upstream')

  os.system('git checkout master')

  os.system('git rebase upstream/master')

  cmd = 'git checkout ' + branch

  os.system(cmd)

  os.system('git merge master')

  print '+++ R2-D2 is ready to work +++'

elif (WWD == '2'):

  print '+++ Push all my stuff +++'
  
  os.system('git add -A')

  msens = raw_input('Please write a nice message here: ')

  mensaje = 'git commit -m '+ "'msens'"

  os.system(mensaje)
  
  push = 'git push origin '+ branch

  print '+++ Please fill the form +++'

  os.system(push)

else:

  print '+++ Fetch the branch +++'

  os.system('git fetch upstream')

  os.system('git checkout master')

  os.system('git rebase upstream/master')

  cmd = 'git checkout ' + branch

  os.system(cmd)

  os.system('git merge master')

  print '+++ Push all my stuff +++'

  os.system('git add -A')

  msens = raw_input('Please write a nice message here: ')

  mensaje = 'git commit -m '+ "'msens'"

  os.system(mensaje)

  push = 'git push origin '+ branch

  os.system(push)

print 'Have fun and enjoy your meal...'
