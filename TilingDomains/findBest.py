# findBest.py
# A faster method for exhaustively finding the largest MNPP domain on n outcomes
# Exhaustively generates all such domains up to n-1, then uses a dynamic programming + ideal-counting method to speed up the last step

#findBest9.txt contains log from running this with n = 9 (took about 8.5 hours)

from GraphDraw import *
from Graph import *
from Implicit import *
from Involutions import *

from impExamples import *
import os
import errno

import time

flags = os.O_CREAT | os.O_EXCL | os.O_WRONLY


import logging






numAntichainsDB = {}

# A poset is a dictionary, indexed by numbers, that contains lists of numbers representing chains
def posetToIndex(chains, chainIntervals):
  #sort outcomes by length of chain
  outcomes = sorted(chainIntervals.keys(), key=lambda b:chainIntervals[b][1] - chainIntervals[b][0])
  lengths = tuple([chainIntervals[b][1] - chainIntervals[b][0] for b in outcomes])

  out = []
  shift = 2**len(outcomes)
  for b in outcomes:
    w = 0
    for h in range(chainIntervals[b][0], chainIntervals[b][1]):
      c = chains[b][h]
      try:
        w = w * shift + 2**outcomes.index(c)
      except:
        raise Exception(outcomes, c, chains, chainIntervals)
    out.append(w)
  return (lengths, tuple(out))

# test_G = impAlternatingDomain(7)
# test_chains = test_G.chains
# print(test_chains)
# test_intervals = {1:(1, 4), 3:(4, 5), 4:(3, 5), 6:(1, 3), 7:(1, 3)}
# # [3,4,6,7,1]
# #shift = 32
# # 3: 2^4              16
# # 4: 2^2, 2^3         4*32 + 8 = 136
# # 6: 2^0, 2^4         32 + 16 = 48
# # 7: 2^0, 2^4         32 + 16 = 48
# # 1: 2^1 ,2^2, 2^3    2*32^2 + 4*32 + 8 = 2048 + 136 = 2184
# print(posetToIndex(test_chains, test_intervals))


def countAntichains(chains, chainIntervals):
  def recCountAntichains(chains, chainIntervals, prefix):
    #actually counts ideals!
    trivial = True
    for c in chainIntervals:
      if chainIntervals[c][1] > chainIntervals[c][0]:
        trivial = False
        b = c
        break
    if trivial:
      #print(prefix)
      return 1

    def recUpperRestrict(c, loc):
      I = subChainIntervals[c]
      loc = max(I[0], loc)
      if loc >= I[1]:
        return
      subChainIntervals[c] = (I[0], loc)
      for d in chains[c][loc+1:I[1]]:
        recUpperRestrict(d, chains[d].index(c))

    def recLowerRestrict(c, loc):
      I = subChainIntervals[c]
      loc = min(I[1] + 1, loc)
      if loc < I[0]:
        return
      subChainIntervals[c] = (loc + 1, I[1])
      for d in chains[c][I[0]:loc]:
        recLowerRestrict(d, chains[d].index(c))

    def upperReset():
      for c in subChainIntervals:
        I = subChainIntervals[c]
        J = chainIntervals[c]
        subChainIntervals[c] = (I[0], J[1])

    total = 0
    subChainIntervals = chainIntervals.copy()

    for h in range(chainIntervals[b][0]-1, chainIntervals[b][1]):
      # count ideals using chains[b][1:h], and nothing else; the first iteration won't use anything from the b chain
      # collapse chain b at h, then recursively restrict the others
      if h >= chainIntervals[b][0]:
        prefix.append((b, chains[b][h]))
      subChainIntervals[b] = (h, h)
      for i in range(chainIntervals[b][0], h+1):
        c = chains[b][i]
        recLowerRestrict(c, chains[c].index(b))
      for j in range(h+1, chainIntervals[b][1]):
        c = chains[b][j]
        recUpperRestrict(c, chains[c].index(b))
      total += recCountAntichains(chains, subChainIntervals, prefix)
      upperReset()
      if h >= chainIntervals[b][0]:
        del prefix[-1]

    return total
  return recCountAntichains(chains, chainIntervals, [])

def getNumAntichains(chains, chainIntervals1, chainIntervals2):
  ind_pair_1 = posetToIndex(chains, chainIntervals1)
  ind_1 = ind_pair_1[0]
  perms_1 = ind_pair_1[1]
  if ind_1 not in numAntichainsDB:
    numAntichainsDB[ind_1] = {}
  if perms_1 not in numAntichainsDB[ind_1]:
    numAntichainsDB[ind_1][perms_1] = countAntichains(chains, chainIntervals1)

  ind_pair_2 = posetToIndex(chains, chainIntervals2)
  ind_2 = ind_pair_2[0]
  perms_2 = ind_pair_2[1]
  if ind_2 not in numAntichainsDB:
    numAntichainsDB[ind_2] = {}
  if perms_2 not in numAntichainsDB[ind_2]:
    numAntichainsDB[ind_2][perms_2] = countAntichains(chains, chainIntervals2)
  return numAntichainsDB[ind_1][perms_1] * numAntichainsDB[ind_2][perms_2]

def getExtensionSize(G, snake, size):
  b = len(G.outcomes) + 1
  while b in G.outcomes:
    b += 1
  G.extrude(b, snake)

  posets = G.getReducedPosets()
  chains = G.chains

  total_size = size
  # print("-----------")
  # print("CHAINS: " + str(G.chains))
  # print("SNAKE: " + str(snake))
  # print("COLOUR: " + str(b))
  
  for chainIntervalPair in posets:
    #print("CHAIN INTERVALS: " + str(chainIntervalPair))
    total_size += getNumAntichains(chains, chainIntervalPair[0], chainIntervalPair[1])
  G.collapse(b)
  #print("SIZE: " + str(total_size))
  #print()
  return total_size

def findBestExtrusion(G, size):
  max_size = size
  best_snake = None


  snakes = G.getMaxDomain()
  for snake in snakes:
    total_size = getExtensionSize(G, snake, size)

    if total_size >= max_size:
      max_size = total_size
      best_snake = snake

  return [max_size, best_snake]


numberCounted = 0
landmarkDistance = 10000
landmark = landmarkDistance
tilingNumbers = [1, 1, 2, 8, 62, 908, 24698, 1232944, 112018190, 18410581880, 5449192389984, 2894710651370536]
totalToCount = 0
loggerSize = 0
loggerTracks = None
startTime = time.time()

def findBestContaining(G, d):
  global totalToCount

  logging.basicConfig(filename='findingBest'+str(d)+'.log',level=logging.INFO)
  logging.info('Here we go!')
  totalToCount = tilingNumbers[d-1]/2

  if G.d >= d:
    return G


  def searchDomainsContaining(G, d):
    global totalToCount
    global numberCounted, landmarkDistance, landmark, tilingNumbers
    global loggerSize, loggerTracks, startTime
    max_size = 0
    best_dom = None
    b = 1
    while b in G.outcomes:
      b += 1

    dom = G.getMaxDomain()
    size = len(dom)

    if G.d == d - 1:
      pair = findBestExtrusion(G, size)
      G.extrude(b, pair[1])
      tracks = deepcopy(G.trackSnakes)
      G.collapse(b)


      numberCounted += size
      if numberCounted >= landmark:
        print("LANDMARK ", landmark)
        logging.info("Reached landmark " + str(landmark) + ". Time Elapsed: " + str(round(time.time() - startTime)) + ". Completion " + str(((numberCounted*1000)//totalToCount)/10) + "%.")
        landmark += landmarkDistance
      if loggerSize < pair[0]:
        loggerSize = pair[0]
        loggerTracks = tracks
        logging.info("Best: " + str(loggerTracks) + ".")
      return [pair[0], tracks]

  
    for snake in dom:
      pair = searchDomainsContaining(G.extrude(b, snake), d)
      size = pair[0]
      tracks = pair[1]
      if size > max_size:
        best_dom = deepcopy(tracks)
        max_size = size
      G.collapse(b)
  
    return [max_size, best_dom]
  
  pair = searchDomainsContaining(G, d)
  logging.info("Finished. Time Elapsed: " + str(round(time.time() - startTime)) + ". Completion " + str(((numberCounted*1000)//totalToCount)/10) + "%.")
  logging.info("Results: " + str(pair))
  #print(pair[0])
  snakes = pair[1]
  G = ImplGraph()
  for b in snakes:
    G.extrude(b, snakes[b])
  return G



# test_chains = {1: [2, 4, 3], 2: [1, 4, 3], 3: [4, 1, 2], 4: [3, 1, 2]}
# test_intervals = {1: (0, 3), 2: (0, 3), 3: (0, 3), 4: (0, 3)}

# print(countAntichains(test_chains, test_intervals))
# print()





#impG = impAlternatingDomain(8)
#s = findBestExtrusion(impG, impG.sizeOfDomain())
#print(s)
#impG.extrude(9, s[1])


impG = findBestContaining(impAlternatingDomain(3), 9)


#print(numAntichainsDB)
print(impG.chains)
size = impG.sizeOfDomain()
snakes = impG.trackSnakes
print(size)
print(snakes)














