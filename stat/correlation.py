from scipy.stats import spearmanr, kendalltau
import numpy
import commands

def correlation(dirname):
  status, files = commands.getstatusoutput('ls ' + dirname)
  files = files.split('\n')
  steps = []
  costs = []
  count = 0
  correlation = []
  for filename in files:
    f = open(dirname + '/' + filename,'r')
    last_cost = 0
    my_costs = []
    my_steps = []
    for line in f:
      tokens = line.split(',')
      step = int(tokens[0])
      cost = int(tokens[1])
      if step < 5:
        steps.append(step)
        costs.append(cost)
        my_steps.append(step)
        my_costs.append(cost)
    f.close()
    #corr, p = spearmanr(my_steps,my_costs)
    #correlation.append(corr)
    if my_costs[0] <= my_costs[1] and my_costs[1] <= my_costs[2]:
      count = count + 1
  
  #print dirname, spearmanr(steps,costs)
  print dirname, count
  #print numpy.median(correlation), numpy.percentile(correlation, 25), numpy.mean(correlation)
  #print filename, kendalltau(steps,costs)


#print spearmanr([1,2,3,4,5],[5,6,7,8,7])
correlation("../GA/data-ex/n100-s3/v1")
correlation("../GA/data-ex/n100-s3/v2")
