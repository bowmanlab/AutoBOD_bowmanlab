# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

with open('20240208_test.txt', 'r') as autobod_in, open('20240208_test.clean.csv', 'w') as autobod_out:
    for line in autobod_in.readlines():
        line = line.rstrip()
        line = line.split()
        if len(line) != 15:
            continue
        else:
            line = ','.join(line)
            print(line, file = autobod_out)