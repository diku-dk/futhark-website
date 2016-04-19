#!/bin/sh

import blur
import png
import numpy
from scipy import misc
import argparse

def main(infile, outfile, iterations):
    b = blur.blur()
    img = misc.imread(infile, mode='RGB')
    (height, width, channels) = img.shape
    blurred = b.main(iterations, img)
    w = png.Writer(width, height, greyscale=False, alpha=False, bitdepth=8)
    with open(outfile, 'wb') as f:
        w.write(f, numpy.reshape(blurred.astype(numpy.uint8), (height, width*3)))

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Simple Gaussian blur of a PNG file.')
    parser.add_argument('--output-file', metavar='OUTFILE', type=str, required=True,
                        help='Where the result is written')
    parser.add_argument('--iterations', metavar='INT', type=int, default=1,
                        help='Number of iterations to run')
    parser.add_argument('filename', metavar='INFILE', type=str,
                        help='The PNG file to blur')
    args = parser.parse_args()
    main(args.filename, args.output_file, args.iterations)
