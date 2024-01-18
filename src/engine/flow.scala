/*
    Fury, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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