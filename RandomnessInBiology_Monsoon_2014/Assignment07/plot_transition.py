
"""plot_transition.py: 

Last modified: Sat Jan 18, 2014  05:01PM

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2013, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"


txt = open("./transition_time.txt", "r").read()
list = filter(None, txt.split("\n"))
list = [ float(x) for x in list]
for i, j in enumerate(list[1:]):
    print j, j - list[i]

