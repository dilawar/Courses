"""places.py: 

    Contains classes for inside of cells.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2015, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

class Place(object):
    """docstring for Place"""
    def __init__(self):
        self.name = ""
        self.area = 0.0
        self.components = []
        self.density = 0.0
        self.concentration = {}


class Ribosome(Place):
    def __init__(self):
        super(Ribosome, self).__init__()
        self.mRNAPresent = False
        
        
class ER(Place):
    def __init__(self):
        super(ER, self).__init__()


class RoughER(ER):
    """docstring for RoughER"""
    def __init__(self):
        super(RoughER, self).__init__()
        self.ribosomes = []

class SmoothER(ER):
    """docstring for SmoothER"""
    def __init__(self):
        super(SmoothER, self).__init__()
        

class Golgi(Place):
    """docstring for Golgi"""
    def __init__(self):
        super(Golgi, self).__init__()


class Vesicle(Place):
    """docstring for Vesicle"""
    def __init__(self):
        super(Vesicle, self).__init__()
        
        
class Cytosol(Place):
    """docstring for Cytosol"""
    def __init__(self):
        super(Cytosol, self).__init__()
        self.ribosomes = []
        self.mRNAs = []

