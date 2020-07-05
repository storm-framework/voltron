#!/usr/bin/env bash

./build.sh 
cd ../dist
./voltron server --static=static/
