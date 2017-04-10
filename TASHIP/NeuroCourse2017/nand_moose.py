import moose
import moose.utils as mu
import numpy as np
import pylab

def nand( a, b ):
    compt = moose.CubeMesh( '/compt' )
    compt.volume = 1
    species = {}
    tables = { }
    concInit  =  { 'a' : a, 'b' : b, 'r' : 1, 'x' : 1 }

    for s in [ 'a', 'b', 'r', 'x', 'x*', 'ab', 'xab' ] :
        p = moose.Pool( '%s/%s' % (compt.path, s) )
        t = moose.Table2( '%s/%s_table' % (compt.path, s ) ) 
        moose.connect( t, 'requestOut', p, 'getConc' )
        tables[ s ] = t
        p.concInit = concInit.get( s, 0.0 )
        species[ s ] = p

    # Now the reactions.
    r1 = moose.Reac( '%s/reac1' % compt.path )
    moose.connect( r1, 'sub', species[ 'a' ], 'reac' )
    moose.connect( r1, 'sub', species[ 'b' ], 'reac' )
    moose.connect( r1, 'prd', species[ 'ab' ], 'reac' )
    r1.Kf = 1
    r1.Kb = 0

    r2 = moose.Reac( '%s/reac2' % compt.path )
    moose.connect( r2, 'sub', species[ 'r' ], 'reac' )
    moose.connect( r2, 'sub', species[ 'x' ], 'reac' )
    moose.connect( r2, 'prd', species[ 'x*' ], 'reac' )
    moose.connect( r2, 'prd', species[ 'r' ], 'reac' )
    r2.Kf = 1
    r2.Kb = 0

    r3 = moose.Reac( '%s/reac3' % compt.path )
    moose.connect( r3, 'sub', species[ 'x*' ], 'reac' )
    moose.connect( r3, 'sub', species[ 'ab' ], 'reac' )
    moose.connect( r3, 'prd', species[ 'xab' ], 'reac' )
    r3.Kf = 100
    r3.Kb = 0.01

    stoich = moose.Stoich( '%s/stoich' % compt.path )
    ksolve = moose.Ksolve( '%s/ksolve' % compt.path )
    stoich.ksolve = ksolve
    stoich.compartment = compt
    stoich.path = '%s/#' % compt.path

    moose.reinit( )
    moose.start( 20 )
    return tables['x*'].vector[-1]

def main( ):
    img = [ ]
    for i in range( 0, 3, 1 ):
        row = [ ]
        for j in range( 0, 3, 1 ):
            out = nand( i, j)
            row.append( out  )
            moose.delete( '/compt' )
        img.append( row )

    print( img )
    pylab.imshow( img, interpolation = 'none' )
    pylab.colorbar( )
    pylab.title( 'NAND gate' )
    pylab.xlabel( 'A' )
    pylab.ylabel( 'B' )
    pylab.savefig( 'nand.png' )

if __name__ == '__main__':
    main()
