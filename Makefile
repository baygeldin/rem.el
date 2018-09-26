CASK ?= cask
EMACS ?= emacs

all: test

test:
	${CASK} exec buttercup -L .

install:
	${CASK} install

.PHONY: all test install
