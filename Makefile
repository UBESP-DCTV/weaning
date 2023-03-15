script ?= reports/keras.R
script_test ?= reports/keras_on_test.R

all: train

train:
	Rscript ${script}

test:
	Rscript ${script_test}
