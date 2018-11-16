assignment

You are being asked to explore Markov Decision Processes (MDPs):

 1. Come up with two interesting MDPs. Explain why they are
    interesting. They don't need to be overly complicated or directly
    grounded in a real situation, but it will be worthwhile if your
    MDPs are inspired by some process you are interested in or are
    familiar with. It's ok to keep it somewhat simple. For the
    purposes of this assignment, though, make sure one has a "small"
    number of states, and the other has a "large" number of states.

 2. Solve each MDP using value iteration as well as policy
    iteration. How many iterations does it take to converge? Which one
    converges faster? Why? Do they converge to the same answer? How
    did the number of states affect things, if at all?

 3. Now pick your favorite reinforcement learning algorithm and use it
    to solve the two MDPs. How does it perform, especially in
    comparison to the cases above where you knew the model, rewards,
    and so on? What exploration strategies did you choose? Did some
    work better than others?

process
- choose mdps
- implement mdps
- solve with value iteration
- solve with policy iteration
- pick an RL algo and solve.
  - exploration strategies - choose and contrast


clues

the students' answer,
where students collectively construct a single answer
Actions

I tried pymdptoolbox but observe these limitations.

I am thinking of using pymdptoolbox for value and policy iteration
algorithms and make some plots there to compare iteration time and
counts.

For Q-learning, it may make sense to follow an implementation from
scratch which constructs a class of Q-learning agent, which takes a
MDP class. For these, look at the following GitHub repo which
implements the RL algorithms of the famous Peter Norving, Stuart
Russell book - Artificial Intelligence, a Modern Approach.

https://www.amazon.com/Artificial-Intelligence-Modern-Approach-3rd/dp/0136042597

GitHub repo for Python implementation of AIMA code/examples
https://github.com/aimacode/aima-python


You have to look at mdp.py, mdp.ipynb (Notebook), rl.py, and rl.ipynb
(Notebook), and other helper functions in utils.py and notebook.py to
make your own code.

Re-factor as much as you can but you may have to write custom loops to
generate desired plots.

---

The absorbing state can be modeled in pymdptoolbox as a state where
all actions lead to itself with a probability of 1, and zero reward.

I agree that the default implementation of Q-Learning is quite
unusable, but can easily be adapted and made serviceable with a few
tweaks.

--

Besides converging graphs to compare the Policy and Value Iterations
for the two MDP problems I'm not imagining too many different types of
useful graphs. Given that I'm wondering if the 10 page maximum is
generous, especially compared to these past assignments where I was
struggling to fit in 12 or more graphs.

 


Since no answer to this yet I will try.

 

I think for a given MDP, besides converging graph you can always do -

 

1. How many iterations does VI take for various reward scenarios i.e. is the variance too high

2. How many iterations does PI take for various reward scenarios i.e. is the variance too high

3. How about VI time for various reward scenarios

4. How about PI time for various reward scenarios

5. How all of the above changes when (a) states are high, actions are low, (b) many actions but small number of states, (c)....

6. How does Q-learning compare to VI for long term utility estimate?

---

Assignment #4 help

What is the best framework to use for solving MDP problem in python

 

Concretely whch python framework have built in API for value iteration,policy iteration and Q learning?

https://github.com/dennybritz/reinforcement-learning

 

I have found this library to be the nicest one, the code is nicely written and not overly complicated and the best thing, it directly supports OpenAI Gym environments. It has implementations for VI, PI and Q learning.

 

https://github.com/dennybritz/reinforcement-learning/tree/master/DP

https://github.com/dennybritz/reinforcement-learning/tree/master/TD

 

There're also other options:

https://github.com/aimacode/aima-python/blob/master/mdp.py

https://github.com/sawcordwell/pymdptoolbox

 https://github.com/dennybritz/reinforcement-learning

 

I have found this library to be the nicest one, the code is nicely written and not overly complicated and the best thing, it directly supports OpenAI Gym environments. It has implementations for VI, PI and Q learning.

 

https://github.com/dennybritz/reinforcement-learning/tree/master/DP

Https://github.com/dennybritz/reinforcement-learning/tree/master/TD

 

There're also other options:

https://github.com/aimacode/aima-python/blob/master/mdp.py

https://github.com/sawcordwell/pymdptoolbox

 
