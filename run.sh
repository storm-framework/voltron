#!/usr/bin/env bash

./build.sh 
cd ../dist
./voltron server --port=3001 --static=static/
