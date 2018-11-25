SHELL=/bin/bash

# change this to match the name of python on $PATH which is version 3.6 or later.
python_exe=python3.6

datasets_dir=$(PWD)/datasets
pips=jupyter scipy pandas matplotlib scikit-learn kaggle seaborn numpy pyyaml google-cloud-bigquery xlrd graphviz geopy kaggle psutil memory_profiler pymdptoolbox cvxopt

CHICKEN_VERSION=4.12.0
CHICKEN_EGGS=random-bsd matchable typed-records linear-algebra fmt sql-de-lite

venv_name=venv

ifndef EC_SITE
with_venv=source $(venv_name)/bin/activate &&
endif

ifdef EC_SITE
with_venv=
endif

#MENU menu: show this menu
menu:
	@cat $(PWD)/Makefile | grep '^#MENU ' | sed 's/^#MENU \+//'


tf/tf:
	mkdir -p tf
	touch $@

tf/python3-exists: tf/tf
	which $(python_exe)
	touch $@

tf/$(venv_name): tf/python3-exists
	python3.6 -m venv $(venv_name)
	$(with_venv) pip3 install --upgrade pip
	$(with_venv) pip3 install --upgrade $(pips)
	touch $@



mkvenv: tf/$(venv_name)
	echo ok


PREFIX=$(PWD)/scratch/prefix

CSI=scratch/prefix/bin/csi
CSC=scratch/prefix/bin/csc

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
   PLATFORM=linux
endif
ifeq ($(UNAME_S),Darwin)
   PLATFORM=macosx
endif



$(PREFIX)/.dummy:
	mkdir -p $(PREFIX)
	touch $@

#MENU: install-chicken: install chicken scheme
install-chicken $(CSC) $(CSI): tf/tf $(PREFIX)/.dummy
	if [[ ! -e scratch/chicken-$(CHICKEN_VERSION).tar.gz ]]; then \
          cd scratch && wget http://code.call-cc.org/releases/$(CHICKEN_VERSION)/chicken-$(CHICKEN_VERSION).tar.gz ; \
        fi
	cd scratch && tar xzvf chicken-$(CHICKEN_VERSION).tar.gz
	cd scratch/chicken-$(CHICKEN_VERSION) && make PREFIX=$(PREFIX) PLATFORM=$(PLATFORM) && make PREFIX=$(PREFIX) PLATFORM=$(PLATFORM) install
	rm -rf scratch/chicken-$(CHICKEN_VERSION)
	$(PREFIX)/bin/chicken-install $(CHICKEN_EGGS)

##MENU notebook:   start jupyter notebook .. connect to http://thishost:8888 with password mlai
notebook: tf/$(venv_name)
	$(with_venv) cd $(PWD)/notebooks && JUPYTER_CONFIG_DIR=$(PWD) jupyter notebook --no-browser -y

#MENU tidy:       clean up ~ files
tidy:
	find -name \*~ -print0 | xargs -0 rm -f

#MENU proj4: compile proj4
proj4: proj4.scm proj4-lib.scm $(CSC)
	$(CSC) proj4.scm

run: proj4
	./proj4

proj4-experiment: proj4-experiment.scm proj4-lib.scm $(CSC)
	$(CSC) proj4-experiment.scm

proj4-driver: proj4-driver.scm proj4-experiment $(CSC)
	$(CSC) proj4-driver.scm


#epoch=1
epoch=2
#MENU testplan: prepare experiments for launching and create joblist file
testplan proj4-$(epoch).joblist: proj4-driver  proj4-spec-$(epoch).sexp proj4-experiment
	./proj4-driver proj4-spec-$(epoch).sexp $(epoch) $@ experiments-plan-$(epoch).sexp

proj4-miner: proj4-lib.scm proj4-miner.scm results/$(epoch)-experiment-specs.sexp
	$(CSC) proj4-miner.scm

#MENU collect: collect results of experiment into sqlite3 dbase
collect results.sexp.gz results.sqlite3:  proj4-miner
	./proj4-miner  results/$(epoch)-experiment-specs.sexp $(epoch)-results.sexp $(epoch)

proj4-figures: proj4-figures.scm 
	$(CSC) proj4-figures.scm

figures: proj4-figures #results.sexp.gz results.sqlite3 
	mkdir -p figures
	./proj4-figures $(epoch)
