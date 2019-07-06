from GraphDraw import *
from Graph import *
from Implicit import *

from impExamples import *
import os
import errno

flags = os.O_CREAT | os.O_EXCL | os.O_WRONLY

# For trying stuff
# Currently just displays the alternating domain on 9 outcomes

n = 9
impG = impAlternatingDomain(n)
G = impG.explicit()
d = G.d


G.computePoset()
G.completeInversionOrder()

size = G.sizeOfDomain()
snakes = G.getTrackSnakes()

try:
  file_handle = os.open("test-output/best/info"+str(d)+".txt", flags)
except OSError as e:
  if e.errno == errno.EEXIST:  # Failed as the file already exists.
    pass
  else:  # Something unexpected went wrong so reraise the exception.
    raise
else:  # No exception, so the file must have been created successfully.
  with os.fdopen(file_handle, 'w') as F:
    


    F.write(str(G.getVRSystem()))
    F.write("Size of domain: " + str(size))
    F.write("")
    F.write(str(snakes))


drawGraph(G, "best/graphs/bestgraph"+str(d))

drawPoset(G, "best/posets/bestposet"+str(d))

drawGraphAndPoset(G, "best/both/best"+str(d))
