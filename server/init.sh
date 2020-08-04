#!/usr/bin/env bash

stack run -- add-user --email=rjhala@eng.ucsd.edu             --password=rjhala --firstname=Ranjit --lastname=Jhala
stack run -- add-class --institution=UCSD  --classname=CSE230 --instructor=rjhala@eng.ucsd.edu --language=haskell
stack run -- add-class --institution=UCSD  --classname=CSE130 --instructor=rjhala@eng.ucsd.edu --language=markdown

