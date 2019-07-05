# Graph.py
# Represents MNPP domains using the rhombus tiling method
# Actually creates the graph of a tiling domain
# Now everything is faster doing this implicitly using wiring diagrams - this is only useful for visualization

import math
import bisect
from copy import deepcopy

def increasingFuncExists(s1, s2):
  #s1, s2 must be sorted lists of numbers
  if len(s1) > len(s2):
    return False
  #s1 = sorted(s1)
  #s2 = sorted(s2)
  shift = 0
  for i in range(len(s1)):
    while(i + shift < len(s2) and s1[i] > s2[i + shift]):
      shift += 1
    if i + shift >= len(s2):
      return False
  return True

class Graph:
  def __init__(self):
    self.d = 0
    self.computeVectors()

    self.outcomes = []

    self.upV = {0:[]}
    self.downV = {0:[]}

    self.E = []
  
    self.invV = {}
    self.poset = []

    self.trackSnakes = {}

  def addVertexIfNew(self, v):
    if v not in self.upV:
      self.upV[v] = []
      self.downV[v] = []

  def delVertexIfIsolated(self, v):
    if not self.upV[v] and not self.downV[v]:
      self.upV.pop(v)
      self.downV.pop(v)

  def addEdge(self, u, v):
    self.E.append([u, v])
    self.addVertexIfNew(u)
    self.addVertexIfNew(v)
    if v not in self.upV[u]:
      bisect.insort(self.upV[u], v)
      bisect.insort(self.downV[v], u)

  def deleteEdge(self, i):
    e = self.E[i]
    self.upV[e[0]].remove(e[1])
    self.downV[e[1]].remove(e[0])
    self.delVertexIfIsolated(e[0])
    self.delVertexIfIsolated(e[1])
    
    self.E.pop(i)

  def mutateEdge(self, i, u, v):
    e = self.E[i]
    self.upV[e[0]].remove(e[1])
    self.downV[e[1]].remove(e[0])

    self.delVertexIfIsolated(e[0])
    self.delVertexIfIsolated(e[1])

    self.E[i][0] = u
    self.E[i][1] = v

    self.addVertexIfNew(u)
    self.addVertexIfNew(v)
    if v not in self.upV[u]:
      bisect.insort(self.upV[u], v)
      bisect.insort(self.downV[v], u)

  def computeVectors(self):
    scale = 2 + self.d/40
    angles = [(i-0.5) * math.pi/self.d for i in range(1, self.d+1)]
    self.vectors = [[round(-scale*math.cos(theta), 5), round(scale*math.sin(theta), 5)] for theta in angles]

  def leftOf(self, v, w):
    return increasingFuncExists(self.getIndexSetRep(v), self.getIndexSetRep(w))

  def getIndexSetRep(self, v):
    ls = []
    p = 0
    while v >= 2**p:
      if (v//2**p) % 2 > 0:
        ls.append(p)
      p += 1
    return ls

  def getSetRep(self, v):
    ls = []
    p = 0
    while v >= 2**p:
      if (v//2**p) % 2 > 0:
        ls.append(self.outcomes[p])
      p += 1
    return ls

  def getNumRep(self, ls):
    num = 0
    for x in ls:
      y = self.outcomes.index(x)
      num += 2**y
    return num

  def positionVertex(self, v):
    def subsetToSum(vecs):
      return [sum([v[0] for v in vecs]), sum([v[1] for v in vecs])]
    return subsetToSum([self.vectors[self.outcomes.index(j)] for j in self.getSetRep(v)])

  def vectorInVertex(self, b, v):
    p = self.outcomes.index(b)
    return (v // 2**p) % 2 > 0

  def addVectorToVertex(self, b, v):
    if not self.vectorInVertex(b, v):
      return v + 2**self.outcomes.index(b)
    return v

  def removeVectorFromVertex(self, b, v):
    if self.vectorInVertex(b, v):
      return v - 2**self.outcomes.index(b)
    return v

  def isSnake(self, snake):
    v = 0
    for i in range(len(snake)):
      w = self.addVectorToVertex(snake[i], v)
      if w not in self.upV[v]:
        return False
      v = w
    return True

  def extrude(self, b, snake):
    if not self.isSnake(snake):
      raise Exception("Tried to extrude on a non-snake.")
    if b in self.outcomes:
      raise Exception("Tried to extrude with an existing basis vector.")

    self.trackSnakes[b] = snake
    self.outcomes.append(b)
    self.d += 1
    self.computeVectors();

    #empty set
    v = 0

    snake_verts = [0]
    for i in range(len(snake)):
      v = self.addVectorToVertex(snake[i], v)
      snake_verts.append(v)

    for j in range(len(self.E)):
      e = self.E[j]
      comparable_vert_0 = snake_verts[len(self.getSetRep(e[0]))]
      comparable_vert_1 = snake_verts[len(self.getSetRep(e[1]))]
      u = e[0]
      v = e[1]
      if self.leftOf(comparable_vert_0, u) and self.leftOf(comparable_vert_1, v):

        x = self.addVectorToVertex(b, u)
        y = self.addVectorToVertex(b, v)
        
        self.mutateEdge(j, x, y)

    for v in snake_verts:
      self.addEdge(v, self.addVectorToVertex(b, v))

    for i in range(len(snake_verts)-1):
      self.addEdge(snake_verts[i], snake_verts[i+1])

    return self

  def collapse(self, b):
    snake_verts = []
    i = 0
    while i < len(self.E):
      e = self.E[i]
      if (not self.vectorInVertex(b, e[0])) and self.vectorInVertex(b, e[1]):
          #delete edge entirely
          self.deleteEdge(i)
          snake_verts.append(e[0])
      else:
        i += 1

    i = 0
    while i < len(self.E):
      e = self.E[i]
      if self.vectorInVertex(b, e[0]):
        e[0] = self.removeVectorFromVertex(b,e[0])
        e[1] = self.removeVectorFromVertex(b,e[1])

        if e[0] in snake_verts and e[1] in snake_verts:
          #duplicate edge; delete entirely
          self.deleteEdge(i)
          i -= 1
      i += 1

    #remove vector from basis of names
    def removeVectorSlot(ind, v):
      v1 = v % 2**ind
      v2 = v // 2**ind
      return v1 + v2 * 2**(ind-1)

    ind = self.outcomes.index(b)
    
    edges = self.E.copy()
    #rebuild graph from edges
    self.upV = {}
    self.downV = {}
    self.E = []
    for e in edges:
      self.addEdge(removeVectorSlot(ind, e[0]), removeVectorSlot(ind, e[1]))

    #remove vector, original snake, reduce dimension, and recompute basis vectors
    self.trackSnakes.remove(b)

    self.outcomes.pop(ind)

    self.d -= 1



    self.computeVectors()

    return self
  
  def getEdgeLabel(self, u, v):
    ind = round(math.log(v - u, 2))
    return self.outcomes[ind]

  def getInversion(self, u, v1, v2):
    return (self.getEdgeLabel(u, v1), self.getEdgeLabel(u, v2))

  def getInversionOnLeft(self, u, v):
    v_index = self.upV[u].index(v)
    if v_index == 0:
      return None
    return self.getInversion(u, self.upV[u][v_index-1], v)

  def getInversionOnRight(self, u, v):
    v_index = self.upV[u].index(v)
    if v_index == len(self.upV[u])-1:
      return None
    return self.getInversion(u, v, self.upV[u][v_index+1])

  def computePoset(self):
    self.invV = {}
    self.poset = []

    for u in self.upV:
      if len(self.upV[u]) == 1:
        if self.downV[u]:
          v1 = self.downV[u][0]
          v2 = self.downV[u][-1]
          left_inv = self.getInversionOnLeft(v1, u)
          right_inv = self.getInversionOnRight(v2, u)
          #if left_inv is not None and right_inv is not None:
            #self.poset.append([left_inv, right_inv])

        

      elif len(self.upV[u]) > 1:
        right_inv = self.getInversion(u, self.upV[u][0], self.upV[u][1])
        self.invV[u] = [right_inv]

        if self.downV[u]:
          #add poset relation across leftmost edge
          v = self.downV[u][0]
          i = self.upV[v].index(u)
          if i > 0:
            left_inv = self.getInversion(v, self.upV[v][i-1], self.upV[v][i])
            self.poset.append([left_inv, right_inv])


        for i in range(1, len(self.upV[u])-1):
          right_inv = self.getInversion(u, self.upV[u][i], self.upV[u][i+1])
          self.invV[u].append(right_inv)

        left_inv = right_inv
        if self.downV[u]:
          v = self.downV[u][-1]
          i = self.upV[v].index(u)
          if i < len(self.upV[v])-1:
            right_inv = self.getInversion(v, self.upV[v][i], self.upV[v][i+1])
            self.poset.append([left_inv, right_inv])

  # Extends poset of inversions to the transitive closure
  def completeInversionOrder(self):
    self.invBelow = {}
    for i in range(self.d):
      for j in range(i+1, self.d):
        inv = (self.outcomes[i], self.outcomes[j])
        self.invBelow[(self.outcomes[i], self.outcomes[j])] = {inv}
    for e in self.poset:
      inv0 = e[0]
      inv1 = e[1]
      for inv2 in self.invBelow[inv0]:
        if inv2 not in self.invBelow[inv1]:
          self.invBelow[inv1].add(inv2)

  def invLessEqual(self, inv0, inv1):
    return inv0 in self.invBelow[inv1]

  def getValueRestriction(self, i, j, k): 
    indi = self.outcomes.index(i)
    indj = self.outcomes.index(j)
    indk = self.outcomes.index(k)
    ls = sorted([indi, indj, indk])
    indi = self.outcomes[ls[0]]
    indj = self.outcomes[ls[1]]
    indk = self.outcomes[ls[2]]
    if self.invLessEqual((i,j), (j,k)):
      return str(j) + " >> " + str(i) + " " + str(k)
    else:
      return str(j) + " << " + str(i) + " " + str(k)
      #return str(i) + " " + str(k) + " >> " + str(j)

  def getVRSystem(self):
    sys = ""
    for i in range(self.d):
      for j in range(i+1, self.d):
        for k in range(j+1, self.d):
          sys += self.getValueRestriction(self.outcomes[i], self.outcomes[j], self.outcomes[k]) + "\n"
    return sys

  def positionTile(self, u, inv):
    c1 = self.outcomes.index(inv[0])
    c2 = self.outcomes.index(inv[1])
    pu = self.positionVertex(u)
    x = pu[0] + (self.vectors[c1][0] + self.vectors[c2][0])/2
    y = pu[1] + (self.vectors[c1][1] + self.vectors[c2][1])/2
    return [x,y]

  def getMaxDomain(self):
    subSnakes = {}

    def getSnakes(u):
      if not self.upV[u]:
        subSnakes[u] = [[]]
        return subSnakes[u]
      
      snakes = []
      for v in self.upV[u]:
        c = self.getEdgeLabel(u, v)
        if v in subSnakes:
          snakes += [[c] + s for s in subSnakes[v]]
        else:
          snakes += [[c] + s for s in getSnakes(v)]
      subSnakes[u] = snakes
      return snakes
      
    return getSnakes(0)
    

  def sizeOfDomain(self):
    #return len(self.getMaxDomain())
    numSnakes = {}

    def countSnakes(u):
      if not self.upV[u]:
        numSnakes[u] = 1
        return 1
      num = 0
      for v in self.upV[u]:
        if v in numSnakes:
          num += numSnakes[v]
        else:
          num += countSnakes(v)
      numSnakes[u] = num
      return num
    
    return countSnakes(0)

  def localBestSnake(self):
    for b in range(1, self.d+2):
      if b not in self.outcomes:
        break
    snakes = self.getMaxDomain()
    best_snake = snakes[0]
    self.extrude(b, best_snake)
    best_size = self.sizeOfDomain()
    self.collapse(b)
    for snake in snakes:
      self.extrude(b, snake)
      new_size = self.sizeOfDomain()
      self.collapse(b)
      if new_size > best_size:
        best_size = new_size
        best_snake = snake
    return best_snake

  def randomSnake(self):
    import random
    v = 0
    snake = []
    while self.upV[v]:
      w = random.choice(self.upV[v])
      snake.append(self.getEdgeLabel(v, w))
      v = w
    return snake

  def randomExtrusion(self):
    for b in range(1, self.d+2):
      if b not in self.outcomes:
        break
    self.extrude(b, self.randomSnake())

  def getTrackSnakes(self):
    return self.trackSnakes