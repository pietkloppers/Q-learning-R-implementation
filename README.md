# Q-learning-R-implementation
The Q-learning reinforcement learning agent was developed to solve the given maze. The agent is trained using an an  ε-greedy approach. The agent can move left, right, up or down by one block at a time. The reward function is indicated in the maze (F). The agent can be incentivized to reach the target state quicker by employing a penalty of -0.05 if the agent moves into a blank space. Similarly, the agent can be incentivized to reach the target quicker by employing a penalty of -1 if the agent moves into space that has a X. 


  1    2     3    4    5    6    7    8    9   10   11   12   13   14   15
| A  |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
|    |    |XXXX|XXXX|XXXX|    |    |    |    |    |    |    |    |    |    |
|    |    |    |    |    |    |    |    |    |XXXX|XXXX|XXXX|XXXX|    |    |
|    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
|    |XXXX|XXXX|XXXX|XXXX|XXXX|XXXX|XXXX|XXXX|    |    |    |    |    |    |
|    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
|XXXX|    |    |    |    |    |    |    |    |    |XXXX|    |    |XXXX|    |
|XXXX|    |    |    |    |    |    |    |    |    |XXXX|    |    |    |    |
|    |    |    |    |    |    |    |    |    |    |XXXX|    |    |    |    |
|    |    |    |    |    |    |    |    |    |    |XXXX|    |    |XXXX|    |
|    |XXXX|XXXX|XXXX|XXXX|XXXX|XXXX|    |    |    |XXXX|    |    |XXXX|    |
|    |    |    |    |    |  F |    |    |    |    |    |    |    |XXXX|    |
|    |    |    |    |    |    |    |    |    |    |    |    |    |XXXX|    |
|    |    |    |    |    |    |    |    |    |    |    |    |    |XXXX|    |


Starting point = A
Target State= F
Agent cannot enter blocks with XXXX

r(s' = F) = 1
r(s' != F) = 0
