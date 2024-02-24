package fury

import profanity.*
import vacuous.*
import acyclicity.*

class CliFrontEnd()(using Terminal) extends FrontEnd:
  private var graph: Optional[Dag[Task]] = Unset
  private var active: List[Task] = Nil

  def schedule(task: Task): Unit = ()
  def update(task: Task): Unit = ()
  def complete(task: Task): Unit = ()
  def inform[InfoType](info: InfoType) = ()