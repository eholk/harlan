import numpy as np
import matplotlib
import matplotlib.pyplot as plt

def loadFile(name):
    data = np.loadtxt(name,
                      delimiter=",",
                      dtype = { 'names': ('size', 'time'),
                                'formats': ('int', 'float')}
                      )

    return data

def mkplot(name, plot_name="plot", scale=1e6):
    harlan = loadFile('test.bin/bench-%s.kfc.dat' % (name))

    harlan = plt.plot(harlan['size'],
                      harlan['time'] / scale,
                      'bo',
                      label="Harlan")

    plt.ylabel("Execution Time (s)")
    plt.xlabel("Vector Size (million elements)")

    plt.legend(numpoints=1, loc=2)

    plt.tight_layout()
    plt.savefig(plot_name + '.pdf')

def nbody():
    harlan = loadFile("test.bin/bench-nbody.kfc.dat")

    plt.plot(harlan['size'],
             harlan['time'], 'bo',
             label = "Harlan")

    plt.ylabel("Execution time (s)")
    plt.xlabel("Number of bodies (thousands)")

    plt.legend(numpoints=1, loc=2) # loc=1 means upper right, 2 means
                                   # upper left.

    plt.tight_layout()
    plt.savefig("figure9-nbody.pdf", bbinches="tight")

def mkbandwidth():
    data = loadFile('mem-bandwidth.dat')

    plt.loglog(data['size'], data['time'], 'bo')

    plt.ylabel("Execution Time (ms)")
    plt.xlabel("Transfer Size (bytes)");

    #plt.legend(numpoints=1, loc=2)

    plt.tight_layout()
    plt.savefig('mem-bandwidth.pdf')

def mkbandwidth_chunks():
    data = loadFile('mem-bandwidth-chunks.dat')

    plt.semilogx(data['size']/1024, data['time'], 'bo')

    plt.ylabel("Execution Time (ms)")
    plt.xlabel("Chunk Size (KB)");

    #plt.legend(numpoints=1, loc=2)

    plt.tight_layout()
    plt.savefig('mem-bandwidth-chunks.pdf')

def do_plots():
    id = 1    

    matplotlib.rc('font', size=10)
    #matplotlib.rc('lines', linewidth=2.0)
    #matplotlib.rc('lines', markeredgewidth=2.0)
    matplotlib.rc('legend', fontsize=10)
    #size = (4, 2.5)
    size = None

    plt.figure(id, figsize=size)
    mkplot('add-vector', plot_name="figure7-vector-addition", scale=1)
    id += 1
    
    plt.figure(id, figsize=size)
    nbody()
    id += 1

    #plt.figure(id, figsize=size)
    #bfs()
    #id += 1
