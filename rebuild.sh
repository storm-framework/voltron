#!/usr/bin/env bash

cd client 
yarn build --dest ../dist/static

cd ../server
stack install --fast --local-bin-path ../dist
cp -r templates ../dist
