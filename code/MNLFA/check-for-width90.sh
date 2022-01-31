#!/bin/bash

echo > inp_lens.txt
wc -L models/*/*.inp | tee -a inp_lens.txt
sort inp_lens.txt