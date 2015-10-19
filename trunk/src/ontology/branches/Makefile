# OBI Makefile
# James A. Overton <james@overton.ca>
#
# This Makefile is used to build OBI modules
# from ROBOT templates.
#
# WARN: This file contains significant whitespace, i.e. tabs!
# Ensure that your text editor shows you those characters.


### Definitions

SHELL := /bin/bash
OBO     := http://purl.obolibrary.org/obo/
OBI     := $(OBO)OBI_
DEV     := $(OBO)obi/dev/
MODULES := $(DEV)modules/
.DEFAULT_GOAL := all
.SECONDARY:


### Templates
#
# The `templates/` directory contains spreadsheets
# used to generate OWL files with ROBOT.
# The first step is to erase any contents of the module OWL file.
# See https://github.com/ontodev/robot/blob/master/docs/template.md
modules/%.owl: templates/%.tsv
	echo '' > $@
	robot merge \
	--input obi.owl \
	template \
	--template $< \
	annotate \
	--ontology-iri "$(MODULES)$@" \
	--output $@

# Update all modules.
# NOTE: GNU Make will compare timestamps to see which updates are required.
modules: modules/obsolete.owl modules/midlevel-assays.owl modules/epitope-assays.owl

# Get a list of templated term IDs.
terms/%.txt: templates/%.tsv
	mkdir -p terms
	cut -f 1 $^ > $@

# Get a single list of all templated term IDs.
template-terms.txt: terms/*.txt
	cat $^ \
	| grep 'OBI:' \
	| sort -u \
	> $@

# Use IDs to create a list of IRI comment lines to search for.
removed-terms.txt: template-terms.txt
	< $^ \
	sed -n 's|^OBI:\(.*\)|    <!-- $(OBI)\1 -->|p' \
	> $@

# Remove everything between matching comment lines, i.e. specific classes in the OBI OWL file
# - read remove.txt into an array
# - if a line starts with an indented comment, stop skipping
# - if a given line is in the array, then start skipping
# - then either skip the line or print it
# Replace the Ontology IRI on the third line; remove versionIRI.
obi-removed.owl: removed-terms.txt obi.owl
	awk 'FNR==NR {a[$$0];next} /^    <!--/ {s=0} $$0 in a {s=1} s==1 {next} {print}' $^ \
	| sed '3s|$(OBO)obi.owl|$(ROOT)/$@|' \
	> $@


### Debugging
#
# Get a list of all IRIs in OBI.
terms.txt: obi.owl
	robot merge --input obi.owl --output merged.owl
	rapper merged.owl \
	| grep 'rdf-syntax-ns#type' \
	| awk '{print $$1}' \
	| sort -u \
	| grep 'http' \
	> $@


### General
#
# Run all goals
all: modules

