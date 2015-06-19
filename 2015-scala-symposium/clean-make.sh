#!/bin/bash
make clean && make | grep ".*:[0-9]*:.*" 
