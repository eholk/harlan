#!/bin/bash

wget http://www.scheme.com/download/pcsv8.4-a6le.tar.gz

tar xzf pcsv8.4-a6le.tar.gz

cd csv8.4/custom

./configure
make
sudo make install
