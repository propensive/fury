/*
    Fury, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import gossamer.*
import turbulence.*, basicIo.jvm
import ambience.*, environments.jvm, systemProperties.jvm
import galilei.*, fileApi.galileiApi, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.strict
import cellulose.*

object Main:
  def main(args: IArray[Text]): Unit =
    import unsafeExceptions.canThrowAny
    val workspace = Workspace(Properties.user.dir())
    
    Io.println(workspace.debug)
    Io.println(t"")
    Io.println(workspace.commands.debug)
    Io.println(workspace.mounts.debug)
    
    workspace.projects.foreach: (name, project) =>
      Io.println(t"$name:")
      Io.println(project.debug)

