script ?= reports/keras.R

all: train

train:
	Rscript ${script}
