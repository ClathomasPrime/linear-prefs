# impExamples.py
# contains example domains (just the alternating one at the moment)
# contains impProduct, which composes domains by concatenating their wiring diagrams
# also has functions that exhaustively find the largest domains on n outcomes

from Graph import *
from Implicit import *



def alternatingSnake(b):
  if b == 0:
    return []
  if b%2 == 1:
    return [b] + alternatingSnake(b-1)
  else:
    return alternatingSnake(b-1) + [b]

def impAlternatingDomain(b):
  G = ImplGraph()
  for i in range(1, b+1):
    G.extrude(i, alternatingSnake(i-1))
  return G


def impProduct(t1, t2):
  G = deepcopy(t1)
  start = list(reversed(G.outcomes))
  b = len(G.outcomes)
  for c in t2.outcomes:
    snake = start + list(map(lambda x: x + b, t2.trackSnakes[c]))
    G.extrude(b + c, snake)
  return G

def impSquare(t):
  return impProduct(t,t)


def impFindLargestDomain(d):
  return impFindLargestDomainContaining(impAlternatingDomain(3), d)

numberOfSize = {}

def impFindLargestDomainContaining(G, d):
  if G.d >= d:
    return G
  alt_size = impAlternatingDomain(d).sizeOfDomain()
  def finalSizeBound(G, size, d):
    for c in range(G.d, d):
      size *= c+1
    return size

  def searchDomainsContaining(G, d):
    if G.d not in numberOfSize:
      numberOfSize[G.d] = 0
    numberOfSize[G.d] += 1

    if d == G.d:
      return [G.sizeOfDomain(), G.trackSnakes]

    #if d == G.d - 1:
      #return G.findBestExtrusion()


    max_size = 0
    best_dom = None
    b = 1
    while b in G.outcomes:
      b += 1

    dom = G.getMaxDomain()
    size = len(dom)

    #if finalSizeBound(G, size, d) <= alt_size:
      #print("BLAP")
      #return [size, G.trackSnakes]

  
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
  #print(pair[0])
  snakes = pair[1]
  G = ImplGraph()
  for b in snakes:
    G.extrude(b, snakes[b])
  return G



#find largest exhaustively, but at each step only extend the largest of the domains seen so far
#the keep argument is the number of domains to extend at each step
def impFindLargestDomainKeep(d, keep):
  return impFindLargestDomainContainingKeep(impAlternatingDomain(3), d, keep)

def impFindLargestDomainContainingKeep(G, d, keep):
  if G.d >= d:
    return G

  def searchDomainsContaining(G, d):
    if d == G.d:
      return [G.sizeOfDomain(), G.trackSnakes]
      #return G.findBestExtrusion()


    max_size = 0
    best_dom = None
    b = 1
    while b in G.outcomes:
      b += 1

    dom = G.getMaxDomain()
    size = len(dom)

    keep_doms = []
    keep_sizes = []
    threshold = 0
  
    for snake in dom:
      G.extrude(b, snake)
      new_size = G.sizeOfDomain()
      if new_size > threshold:
        keep_doms.append(snake)
        keep_sizes.append(new_size)
        if len(keep_doms) > keep:
          min_size = keep_sizes[0]
          min_index = 0
          for i in range(1, len(keep_doms)):
            if keep_sizes[i] < min_size:
              min_index = i
          keep_sizes.pop(min_index)
          keep_doms.pop(min_index)
          threshold = min(keep_sizes)
      G.collapse(b)          
    for snake in keep_doms:
      pair = searchDomainsContaining(G.extrude(b, snake), d)
      size = pair[0]
      tracks = pair[1]
      if size > max_size:
        best_dom = deepcopy(tracks)
        max_size = size
      G.collapse(b)
  
    return [max_size, best_dom]
  
  pair = searchDomainsContaining(G, d)
  print(pair[0])
  snakes = pair[1]
  G = ImplGraph()
  for b in snakes:
    G.extrude(b, snakes[b])
  return G

