"""globals.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2015, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

from places import *

class Cell(Place):
    """docstring for Cell"""

    def __init__(self):
        self.smoothER = SmoothER()
        self.roughER = RoughER()
        self.cytosol = Cytosol()
        for i in range(int(1e5)):
            self.cytosol.ribosomes.append(Ribosome())
        for i in range(int(1e4)):
            self.roughER.ribosomes.append(Ribosome())

    def totalRibosome(self):
        return len(self.cytosol.ribosomes) + len(self.roughER.ribosomes)


cell = Cell()
