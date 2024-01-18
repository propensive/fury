package fury

import anticipation.*

enum Activity:
  case Progress(stage: Text, progress: Double)
  case Complete
