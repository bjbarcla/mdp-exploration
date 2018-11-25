
How to run the code:
--------------------

1) acquire host with access to python3.6, a gcc toolchain, sqlite3 and
unproxied http/https access to the internet on either MacOS or Linux platform.

2) Clone the repo from github.com
  git clone https://github.com/bjbarcla/mdp-exploration.git

3) install chicken scheme by running
  make install-chicken

4) prepare experiment jobs by running
  make testplan

5) customize launcher.sh to run your favorite batch launcher 

6) launch jobs & wait for results
  sh proj4-3.joblist

7) collect experiment results into sqlite3 database
  make collect

8) create figures (figures/figures.org)
  make figures

9) review figures
  use org-mode in emacs or visit the automatically rendered version at
  https://github.com/bjbarcla/mdp-exploration/blob/master/figures/figures.org

Overview of the code
--------------------
proj4-lib.scm
  - implements policy iteration, value-iteration and q-learning

proj4-driver.scm
  - consumes proj4-spec-3.sexp which defines the experiment plan
  - creates <rundir>/<algo>/<paramsummary>/experiment.sexp for each experiment
    specifies the algorithm to run and key parameter values
  - sets up a joblist file that executes proj4-experiment.scm for each
    experiment using launcher.sh

proj4-experiment.scm
  - runs the experiment.  
  - consumes experiment.sexp created by proj4-driver
  - frontend to algorithms implemented in proj4-lib.scm

proj4-miner.scm
  - collects results from droppings in experiment directories
  - posts results to results table in results-3.sqlite3

proj4-figures.scm
  - uses sqlite db and other droppings in experiment dir to create figures.org

proj4.scm - used for prototyping.

