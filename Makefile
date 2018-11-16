SHELL=/bin/bash

# change this to match the name of python on $PATH which is version 3.6 or later.
python_exe=python3.6

datasets_dir=$(PWD)/datasets
pips=jupyter scipy pandas matplotlib scikit-learn kaggle seaborn numpy pyyaml google-cloud-bigquery xlrd graphviz geopy kaggle psutil memory_profiler pymdptoolbox


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

#MENU notebook:   start jupyter notebook .. connect to http://thishost:8888 with password mlai
notebook: tf/$(venv_name)
	$(with_venv) cd $(PWD)/notebooks && JUPYTER_CONFIG_DIR=$(PWD) jupyter notebook --no-browser -y

#MENU tidy:       clean up ~ files
tidy:
	find -name \*~ -print0 | xargs -0 rm -f

