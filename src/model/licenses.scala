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

import gossamer.*
import digression.*
import rudiments.*

enum License(name: Text, id: LicenseId, category: Text):
  case Afl3 extends License(t"Academic Free License v3.0", lid"afl-3.0", t"afl")
  case Apache2 extends License(t"Apache license 2.0", lid"apache-2.0", t"apache")
  case Artistic2 extends License(t"Artistic license 2.0", lid"artistic-2.0", t"artistic")
  case Bsl1 extends License(t"Boost Software License 1.0", lid"bsl-1.0", t"bsl")
  case Bsd2Clause extends License(t"BSD 2-clause \"Simplified\" license", lid"bsd-2-clause", t"bsd")
  
  case Bsd3Clause extends License(t"BSD 3-clause \"New\" or \"Revised\" license", lid"bsd-3-clause",
      t"bsd")
  
  case Bsd3ClauseClear extends License(t"BSD 3-clause Clear license", lid"bsd-3-clause-clear",
      t"bsd")
  
  case Cc extends License(t"Creative Commons license family", lid"cc", t"cc")
  case Cc01 extends License(t"Creative Commons Zero v1.0 Universal", lid"cc0-1.0", t"cc")
  case CcBy4 extends License(t"Creative Commons Attribution 4.0", lid"cc-by-4.0", t"cc")
  
  case CcBySa4 extends License(t"Creative Commons Attribution Share Alike 4.0", lid"cc-by-sa-4.0",
      t"cc")
  
  case Wtfpl extends License(t"Do What The F*ck You Want To Public License", lid"wtfpl", t"wtfpl")
  case Ecl2 extends License(t"Educational Community License v2.0", lid"ecl-2.0", t"ecl")
  case Epl1 extends License(t"Eclipse Public License 1.0", lid"epl-1.0", t"epl")
  case Epl2 extends License(t"Eclipse Public License 2.0", lid"epl-2.0", t"epl")
  case Eupl11 extends License(t"European Union Public License 1.1", lid"eupl-1.1", t"eupl")
  case Agpl3 extends License(t"GNU Affero General Public License v3.0", lid"agpl-3.0", t"agpl")
  case Gpl extends License(t"GNU General Public License family", lid"gpl", t"gpl")
  case Gpl2 extends License(t"GNU General Public License v2.0", lid"gpl-2.0", t"gpl")
  case Gpl3 extends License(t"GNU General Public License v3.0", lid"gpl-3.0", t"gpl")
  case Lgpl extends License(t"GNU Lesser General Public License family", lid"lgpl", t"lgpl")
  case Lgpl21 extends License(t"GNU Lesser General Public License v2.1", lid"lgpl-2.1", t"lgpl")
  case Lgpl3 extends License(t"GNU Lesser General Public License v3.0", lid"lgpl-3.0", t"lgpl")
  case Isc extends License(t"ISC", lid"isc", t"isc")
  case Lppl13c extends License(t"LaTeX Project Public License v1.3c", lid"lppl-1.3c", t"lppl")
  case MsPl extends License(t"Microsoft Public License", lid"ms-pl", t"ms")
  case Mit extends License(t"MIT", lid"mit", t"mit")
  case Mpl extends License(t"Mozilla Public License 2.0", lid"mpl-2.0", t"mpl")
  case Osl3 extends License(t"Open Software License 3.0", lid"osl-3.0", t"osl")
  case PostgreSql extends License(t"PostgreSQL License", lid"postgresql", t"postgresql")
  case Ofl11 extends License(t"SIL Open Font License 1.1", lid"ofl-1.1", t"ofl")
  case Ncsa extends License(t"University of Illinois/NCSA Open Source License", lid"ncsa", t"ncsa")
  case Unlicense extends License(t"The Unlicense", lid"unlicense", t"unlicense")
  case Zlib extends License(t"zLib License", lid"zlib", t"zlib")

extension (sc: StringContext)
  def lid(): LicenseId = unsafely(LicenseId(sc.parts(0).show))
