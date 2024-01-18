package fury

import feudalism.*
import galilei.*
import rudiments.*
import anticipation.*


class Flow():
  val sources: Mutex[Map[WorkPath, Bytes]] = Mutex(Map())
  val classes: Mutex[List[Directory]] = Mutex(Nil)
  val tasty: Mutex[Map[WorkPath, Bytes]] = Mutex(Map())
  val sjsir: Mutex[Map[WorkPath, Bytes]] = Mutex(Map())
  val javascript: Mutex[Map[WorkPath, Bytes]] = Mutex(Map())
  val files: Mutex[Map[WorkPath, Bytes]] = Mutex(Map())
  val active: Mutex[Map[Text, Int]] = Mutex(Map())