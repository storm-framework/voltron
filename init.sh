#!/usr/bin/env bash

./voltron add-user --email=rjhala@eng.ucsd.edu        --password=rjhala --firstname=Ranjit --lastname=Jhala
./voltron add-user --email=nilehmann@eng.ucsd.edu     --password=nico   --firstname=Nico   --lastname=Lehmann
./voltron add-user --email=wkunkel@eng.ucsd.edu       --password=rose   --firstname=Rose   --lastname=Kunkel
#./voltron add-user --email=npolikarpova@eng.ucsd.edu  --password=nadia  --firstname=Nadia  --lastname=Polikarpova

./voltron add-class --institution=UCSD  --classname=CSE230 --instructor=rjhala@eng.ucsd.edu
./voltron add-class --institution=UCSD  --classname=CSE130 --instructor=rjhala@eng.ucsd.edu

./voltron add-group --classname=CSE230 --groupname=0 --editorlink=-M9Kx-cxRIUgCqVCtjCr
./voltron add-group --classname=CSE230 --groupname=1 --editorlink=-M9L5YBS0kgvUfuz0Ckc
./voltron add-group --classname=CSE230 --groupname=2 --editorlink=-M9L5oPt0fsruy16vntv
./voltron add-group --classname=CSE130 --groupname=3 --editorlink=-M9L5vCVa5FQ0noobA9V
./voltron add-group --classname=CSE130 --groupname=4 --editorlink=-M9L6XICO2mz_yfpDXWR
./voltron add-group --classname=CSE130 --groupname=5 --editorlink=-M9L6nLdsLy_7aXIs4MX

./voltron add-enroll --student=rjhala@eng.ucsd.edu    --classname=CSE230 --groupname=2
./voltron add-enroll --student=nilehmann@eng.ucsd.edu --classname=CSE130 --groupname=4
./voltron add-enroll --student=wkunkel@eng.ucsd.edu   --classname=CSE130 --groupname=4
./voltron add-enroll --student=wkunkel@eng.ucsd.edu   --classname=CSE230 --groupname=2
