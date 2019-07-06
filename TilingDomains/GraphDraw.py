# GraphDraw.py
# Has functions to draw domains using graphviz
# If choosePositions is True, then the nodes will be positioned using the rhombus tiling as a basis
# Else graphviz will put them wherever

# You need graphviz for this, and you must add it to your PATH

import graphviz as gvz

def drawGraph(G, graphName="test_graph", view=True, choosePositions=True, showLabels=True):
  dot = gvz.Digraph(engine='neato')

  def name(v):
    return 'node'+str(v)

  def label(ls):
    if showLabels:
      out = ""
      for i in ls:
        out += str(i)
      return "{" + out + "}"
    return ""

  for v in G.upV:
    if choosePositions:
      p = G.positionVertex(v)
      dot.node(name(v), label(G.getSetRep(v)), pos=str(p[0])+","+str(p[1])+"!")
    else:
      dot.node(name(v), label(G.getSetRep(v)))

  for e in G.E:
    dot.edge(name(e[0]), name(e[1]))

  dot.render('test-output/' + graphName + '.gv', view=view)

def drawPoset(G, graphName="test_graph", view=True, choosePositions=True, showLabels=True):
  scale = 1
  G.computePoset()

  dot = gvz.Digraph(engine='neato')

  def name(inv):
    return 'node'+str(inv)

  def label(inv):
    if showLabels:
      return "(" + str(inv[0]) + ", " + str(inv[1]) + ")"
    return ""

  for v in G.invV:
    for inv in G.invV[v]:
      if choosePositions:
        p = G.positionTile(v, inv)
        dot.node(name(inv), label(inv), pos=str(p[0]*2)+","+str(p[1]*2)+"!")
      else:
        dot.node(name(inv), label(inv))

  for e in G.poset:
    dot.edge(name(e[0]), name(e[1]), color='blue')

  dot.render('test-output/' + graphName + '.gv', view=view)

def drawGraphAndPoset(G, graphName="test_graph", view=True, choosePositions=True, showLabels=True):
  scale = 2
  dot = gvz.Digraph(engine='neato')

  def name(v):
    return 'node'+str(v)

  def label(ls):
    if showLabels:
      out = ""
      for i in ls:
        out += str(i)
      return "{" + out + "}"
    return ""

  for v in G.upV:
    if choosePositions:
      p = G.positionVertex(v)
      dot.node(name(v), label(G.getSetRep(v)), pos=str(p[0]*scale)+","+str(p[1]*scale)+"!")
    else:
      dot.node(name(v), label(G.getSetRep(v)))

  for e in G.E:
    dot.edge(name(e[0]), name(e[1]))

  G.computePoset()

  def invName(inv):
    return 'node'+str(inv)

  def invLabel(inv):
    if showLabels:
      return "(" + str(inv[0]) + ", " + str(inv[1]) + ")"
    return ""

  for v in G.invV:
    for inv in G.invV[v]:
      if choosePositions:
        p = G.positionTile(v, inv)
        dot.node(invName(inv), invLabel(inv), color='blue', pos=str(p[0]*scale)+","+str(p[1]*scale)+"!")
      else:
        dot.node(invName(inv), invLabel(inv), color='blue')

  for e in G.poset:
    dot.edge(invName(e[0]), invName(e[1]), color='blue')

  dot.render('test-output/' + graphName + '.gv', view=view)
