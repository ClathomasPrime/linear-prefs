# Implicit.py
# Represents MNPP domains implicity by a sequence of "snakes"
# Equivalent to a wiring diagram (AKA pseudoline arrangement)
# ImplGraph.explicit() turns an implicit domain into a rhombus tiling (see Graph.py)

from Graph import *

class ImplGraph:
  def __init__(self):
    self.d = 0
    self.outcomes = []
    self.basis = {}
    self.trackSnakes = {}
    self.upV = {}
    self.downV = {}

    self.chains = {}

  def explicit(self):
    G = Graph()
    for b in self.outcomes:
      G.extrude(b, self.trackSnakes[b])
    return G

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

  def getEdgeLabel(self, u, v):
    ind = round(math.log(v - u, 2))
    return self.outcomes[ind]

  def reduceVertexToPrefix(self, b, u):
    ind = self.outcomes.index(b)
    return u%(2**ind)

  def isNeighbour(self, u, v):
    b = self.getEdgeLabel(u, v) #asking whether u has edge coloured b
    x = self.reduceVertexToPrefix(b, u) #x <- u \cap [1, b]
    if x not in self.basis[b]: # basis: snakes we extruded on indexed by colour
      return False
    b_ind = self.outcomes.index(b)
    for c in self.outcomes[b_ind+1:]:
      v = self.reduceVertexToPrefix(c, u)
      if v in self.basis[c]:
        ind = self.basis[c].index(v)
        if ind < len(self.basis[c])-1:
          direc = self.trackSnakes[c][ind]
          direc_ind = self.outcomes.index(direc)
          if direc_ind > b_ind and self.vectorInVertex(c, u):
            return False
          if direc_ind < b_ind and not self.vectorInVertex(c, u):
            return False
    return True
   

  def getNeighbours(self, u):
    if u in self.upV:
      return self.upV[u]
    N = []
    for b in self.outcomes:
      v = self.addVectorToVertex(b, u)
      if u != v and self.isNeighbour(u, v):
        N.append(v)
        if v in self.downV:
          self.downV[v].append(u)
        else:
          self.downV[v] = [u]
    self.upV[u] = N
    return N

  def getDownNeighbours(self, u):
    if u == 0:
      return []
    if u in self.downV:
      return self.downV[u]
    else:
      raise Exception("This didn't work.")


  def isSnake(self, snake):
    u = 0
    for i in range(len(snake)):
      v = self.addVectorToVertex(snake[i], u)
      if not self.isNeighbour(u, v):
        return False
      u = v
    return True

  def extrude(self, b, snake):
    if not self.isSnake(snake):
      raise Exception("Tried to extrude on a non-snake.")
    if b in self.outcomes:
      raise Exception("Tried to extrude with an existing basis vector.")

    self.d += 1
    self.trackSnakes[b] = snake
    self.outcomes.append(b)

    u = 0
    new_snake_vertices = [u]
    for h in range(0, len(snake)):
      u = self.addVectorToVertex(snake[h], u)
      new_snake_vertices.append(u)

    self.basis[b] = new_snake_vertices


    self.upV = {}
    self.downV = {}


    for h in range(0, len(snake)):
      c = snake[h]
      c_ind = self.outcomes.index(c)
      ints_before = 0
      for i in range(0, h):
        d_ind = self.outcomes.index(snake[i])
        if c_ind > d_ind:
          ints_before += 1
      for i in range(h+1, len(snake)):
        d_ind = self.outcomes.index(snake[i])
        if c_ind < d_ind:
          ints_before += 1
      self.chains[c].insert(ints_before, b)
    self.chains[b] = snake.copy()


    return self

  def collapse(self, b):
    self.d -= 1
    self.outcomes.remove(b)
    self.basis.pop(b)
    self.trackSnakes.pop(b)
    self.upV = {}
    self.downV = {}
    del self.chains[b]
    for c in self.chains:
      self.chains[c].remove(b)

  def getMaxDomainModSymmetries(self):
    subSnakes = {}

    def getSnakes(u):
      neighbours = self.getNeighbours(u)
      if not neighbours:
        subSnakes[u] = [[]]
        return subSnakes[u]
      
      snakes = []
      for v in neighbours:
        c = self.getEdgeLabel(u, v)
        if v in subSnakes:
          snakes += [[c] + s for s in subSnakes[v]]
        else:
          snakes += [[c] + s for s in getSnakes(v)]
      subSnakes[u] = snakes
      return snakes
      
    return getSnakes(0)


  def getMaxDomain(self):
    subSnakes = {}

    def getSnakes(u):
      neighbours = self.getNeighbours(u)
      if not neighbours:
        subSnakes[u] = [[]]
        return subSnakes[u]
      
      snakes = []
      for v in neighbours:
        c = self.getEdgeLabel(u, v)
        if v in subSnakes:
          snakes += [[c] + s for s in subSnakes[v]]
        else:
          snakes += [[c] + s for s in getSnakes(v)]
      subSnakes[u] = snakes
      return snakes
      
    return getSnakes(0)

  def sizeOfDomain(self):
    numSnakes = {}

    def countSnakes(u):
      neighbours = self.getNeighbours(u)
      if not neighbours:
        numSnakes[u] = 1
        return 1
      
      num = 0
      for v in neighbours:
        if v in numSnakes:
          num += numSnakes[v]
        else:
          num += countSnakes(v)
      numSnakes[u] = num
      return num

    return countSnakes(0)

  def getReducedPosets(self):
    posets = []
    last_colour = self.outcomes[-1]
    snake = self.trackSnakes[last_colour]
    for h in range(len(snake)):
      chainIntervalsLeft = {}
      chainIntervalsRight = {}
      posets.append([chainIntervalsLeft, chainIntervalsRight])
      lowerLims = {}
      upperLims = {}

      c = snake[h]
      c_ind = self.outcomes.index(c)
      for b in self.outcomes:
        if b != c and b != last_colour:
          onLeftOfIntersection = None
          s_loc = self.chains[b].index(last_colour)
          c_loc = self.chains[b].index(c)
          b_ind = self.outcomes.index(b)
          if b_ind < c_ind:
            # b snake is before both c snake and last snake
            if s_loc <= c_loc:
              # b snake hits last snake first, then c snake
              # "up then right"
              # On left side of intersection
              onLeftOfIntersection = True

              if b in lowerLims:
                min_loc = lowerLims[b]
              else:
                min_loc = 0
              max_loc = s_loc

              for d in self.chains[b][c_loc+1:]:
                if d not in upperLims:
                  upperLims[d] = len(self.chains[d])
                upperLims[d] = min(upperLims[d], self.chains[d].index(b))
            else:
              # b snake hits c snake first, then last snake
              # "right then up"
              # On right side of intersection
              onLeftOfIntersection = False
              
              min_loc = s_loc + 1
              if b in upperLims:
                max_loc = upperLims[b]
              else:
                max_loc = len(self.chains[b])

              for d in self.chains[b][:c_loc]:
                if d not in lowerLims:
                  lowerLims[d] = 0
                lowerLims[d] = max(lowerLims[d], self.chains[d].index(b)+1)

          if b_ind > c_ind:
            # b snake is between c snake and last snake
            if s_loc <= c_loc:
              #b chain hits new snake first
              # On right side of intersection
              onLeftOfIntersection = False

              min_loc = s_loc + 1
              if b in upperLims:
                max_loc = min(c_loc, upperLims[b])
              else:
                max_loc = c_loc
            else:
              # b chain hits c chain first
              # On left side of intersection
              onLeftOfIntersection = True

              if b in lowerLims:
                min_loc = max(c_loc + 1, lowerLims[b])
              else:
                min_loc = c_loc + 1
              max_loc = s_loc

          if onLeftOfIntersection:
            chainIntervalsLeft[b] = (min_loc, max_loc)
          else:
            chainIntervalsRight[b] = (min_loc, max_loc)

    return posets
