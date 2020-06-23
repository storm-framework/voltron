.PHONY: build clean

run: src/Model.hs
	stack run

build: src/Model.hs
	stack build

fast: src/Model.hs
	stack build --fast

model: src/Model.hs

src/Model.hs: src/Model.binah
	binah-codegen src/Model.binah src/Model.hs
