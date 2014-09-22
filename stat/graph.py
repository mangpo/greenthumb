

path2tree = "../GA/data-ex/tree-n5000/tree"
path2cost = "../GA/data-ex/tree-n5000/costs-v3"
n = 5005

f = open(path2cost, 'r')
costs = []
for line in f:
  costs.append(float(line))
f.close()

neighbors = [[] for x in range(n)]
f = open(path2tree, 'r')
print "graph {"
for line in f:
  tokens = [int(x) for x in line.split()]
  print str(costs[tokens[0]]) + "_" + str(tokens[0]) + " -- " + str(costs[tokens[1]]) + "_" + str(tokens[1]) + ";"
  neighbors[tokens[0]].append(tokens[1])
  neighbors[tokens[1]].append(tokens[0])
  
print "}"
f.close()

# for x in xrange(n):
#   interest = True
#   for y in neighbors[x]:
#     if costs[y] < costs[x]:
#       interest = False

#   if interest:
#     print "node=", x, "cost=", costs[x], neighbors[x], [costs[i] for i in neighbors[x]]

vis = [False for x in range(n)]
local = []
def dfs(i,not_local):
  #print "DFS(",i,")"
  vis[i] = True

  if not not_local:
    for j in neighbors[i]:
      if costs[j] < costs[i]:
        not_local = True
        break

  total_area = 1
  for j in neighbors[i]:
    if (not vis[j]) and (costs[j] == costs[i]):
      if not_local:
        dfs(j, True)
      else:
        area = dfs(j, False)
        if area:
          total_area = total_area + area
        else:
          not_local = True
          
  if not_local:
    return None
  else:
    return total_area

sum_area = 0
for i in xrange(n):
  if not vis[i]:
    area = dfs(i,False)
    if area: #and (len(neighbors[i]) > 1):
      local.append((i,costs[i],area))
      sum_area = sum_area + area

print local
print "n = ", len(local), "area = ", sum_area
