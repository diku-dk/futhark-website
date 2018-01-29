#!/usr/bin/env python

import numpy
import sys
import math

arr = numpy.loadtxt(sys.argv[1])
edgesize = math.sqrt(arr.shape[0])
print numpy.reshape(arr, (int(edgesize), int(edgesize))).tolist()
