#!/usr/bin/env python
'''
@author: Carlos A. Gimenez
'''
import src.geom_pr.dsquare

class run_each_photon:
  """
  class for running in each photon
  """
  def __init__(self):
    """
    Initialize the global set of variables
    """
    print "Choose photons or electrons"

    print "2: Photons"
    print "1: Electrons"
    proces = raw_input("write 1 or 2 ")
    
    src.geom_pr.dsquare.spamfun()

    print "You choose %s" % (proces)
    n = 100
    
#    for value in range(1,n):
#      print (value)
           
run_each_photon()
