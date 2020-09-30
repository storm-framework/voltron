#!/usr/bin/env bash

./build.sh 
cd ../dist
./voltron server --port=8091 --static=static/
