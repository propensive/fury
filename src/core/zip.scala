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

import rudiments.*
import deviation.*
import gossamer.*
import serpentine.*
import galilei.*
import turbulence.*
import ambience.*

import java.io as ji
import java.nio.file as jnf
import java.util.zip.*

object Zip:

  sealed trait ZipEntry:
    def path: Relative

  case class ZipPath(path: Relative, diskPath: DiskPath) extends ZipEntry
  case class Entry(path: Relative, in: ji.InputStream) extends ZipEntry

  def read(file: galilei.File): LazyList[Entry] =
    val zipFile = ZipFile(file.javaFile).nn
    
    zipFile.entries.nn.asScala.to(LazyList).filter(!_.getName.nn.endsWith("/")).map: entry =>
      Entry(Relative.parse(entry.getName.nn.show), zipFile.getInputStream(entry).nn)

  // 00:00:00, 1 January 2000
  val epoch = jnf.attribute.FileTime.fromMillis(946684800000L)

  def write(base: galilei.File, path: galilei.DiskPath, inputs: LazyList[ZipEntry],
                prefix: Maybe[Bytes] = Unset)
           (using Environment)
           : Unit throws StreamCutError | IoError =
    
    val tmpPath = Fury.tmpDir.tmpPath()
    base.copyTo(tmpPath)
    val uri: java.net.URI = java.net.URI.create(t"jar:file:${tmpPath.fullname}".s).nn
    
    val fs =
      try jnf.FileSystems.newFileSystem(uri, Map("zipinfo-time" -> "false").asJava).nn
      catch case exception: jnf.ProviderNotFoundException =>
        throw AppError(t"Could not create JAR filesystem: ${uri.toString}")

    val dirs = unsafely(inputs.map(_.path).map(_.parent)).to(Set).flatMap: dir =>
      (0 to dir.parts.length).map(dir.parts.take(_)).map(Relative(0, _)).to(Set)
    .to(List).map(_.show+t"/").sorted

    dirs.foreach: dir =>
      val dirPath = fs.getPath(dir.s).nn
      
      if jnf.Files.notExists(dirPath) then
        jnf.Files.createDirectory(dirPath)
        jnf.Files.setAttribute(dirPath, "creationTime", epoch)
        jnf.Files.setAttribute(dirPath, "lastAccessTime", epoch)
        jnf.Files.setAttribute(dirPath, "lastModifiedTime", epoch)

    inputs.foreach:
      case Entry(path, in) =>
        val entryPath = fs.getPath(path.show.s).nn
        jnf.Files.copy(in, entryPath, jnf.StandardCopyOption.REPLACE_EXISTING)
        jnf.Files.setAttribute(entryPath, "creationTime", epoch)
        jnf.Files.setAttribute(entryPath, "lastAccessTime", epoch)
        jnf.Files.setAttribute(entryPath, "lastModifiedTime", epoch)
      
      case ZipPath(path, file) =>
        val filePath = fs.getPath(path.show.s).nn
        jnf.Files.copy(file.javaPath, filePath, jnf.StandardCopyOption.REPLACE_EXISTING)
        jnf.Files.setAttribute(filePath, "creationTime", epoch)
        jnf.Files.setAttribute(filePath, "lastAccessTime", epoch)
        jnf.Files.setAttribute(filePath, "lastModifiedTime", epoch)
    
    fs.close()

    val fileOut = ji.BufferedOutputStream(ji.FileOutputStream(path.javaFile).nn)
    
    prefix.option.foreach: prefix =>
      fileOut.write(prefix.mutable(using Unsafe))
      fileOut.flush()
    
    fileOut.write(jnf.Files.readAllBytes(tmpPath.javaPath))
    fileOut.close()
    java.nio.file.Files.delete(tmpPath.javaPath)
