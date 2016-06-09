#!/usr/bin/env python

"""
    Setup script for TG-Cellular-Automation calculations.
  
   
    Written by Carlos A. Gimenez, Natural and Exact Science Faculty
                                  Northeastern University of Argentina,
        			              Corrientes, Argentina
   
             DIRTIMEOUT        - time out limit for TG-Cellular-Automation run
          
"""
 
import os
import sys

class set_enviroment:
  """
  class for handling the  TG-Cellular-Automation setup
  """
  def __init__(self):
    """
    Initialize the global set of variables
    """
  def __install__(self):
    path = os.getcwd()+"/src/tg_kernel"
    os.chdir(path)
    os.system("cmake -DCMAKE_BUILD_TYPE=RELEASE")
    print "runing cmake"
    os.system("make")
    print "done"
  
  def __clean__(self):
    path = os.getcwd()+"/src/tg_kernel"
    os.chdir(path)
    os.system("cmake -P distclean.cmake")
    print "runing cmake clean"
    os.system("make distclean")
    print "done"

print "The TG-Cellular-Automation code is ready, enjoy your meal"
