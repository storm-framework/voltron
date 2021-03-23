.PHONY: build clean

run: src/Model.hs
	stack run

build: src/Model.hs
	stack build

fast: src/Model.hs
	stack build --fast

model: src/Model.hs

src/Model.hs: src/Model.storm
	storm-codegen src/Model.storm src/Model.hs
