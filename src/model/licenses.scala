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

import anticipation.*
import rudiments.*
import punctuation.*
import gossamer.*

enum LicenseGroup:
  case Afl, Apache, Artistic, Bsl, Bsd, Cc, Wtfpl, Ecl, Epl, Eupl, Agpl, Gpl, Lgpl, Isc, Lppl, Ms,
      Mit, Mpl, Osl, PostgreSql, Ofl, Ncsa, Unlicense, Zlib

enum License(name: InlineMd, id: LicenseId, group: LicenseGroup):
  case Afl3
  extends License(md"Academic Free License v3.0", LicenseId.unsafe(t"afl-3.0"), LicenseGroup.Afl)
  
  case Apache2
  extends License(md"Apache license 2.0", LicenseId.unsafe(t"apache-2.0"), LicenseGroup.Apache)
  
  case Artistic2
  extends License
   (md"Artistic license 2.0", LicenseId.unsafe(t"artistic-2.0"), LicenseGroup.Artistic)
  
  case Bsl1
  extends License(md"Boost Software License 1.0", LicenseId.unsafe(t"bsl-1.0"), LicenseGroup.Bsl)
  
  case Bsd2Clause
  extends License
   (md"BSD 2-clause _Simplified_ license", LicenseId.unsafe(t"bsd-2-clause"), LicenseGroup.Bsd)
  
  case Bsd3Clause
  extends License
   (md"BSD 3-clause _New_ or _Revised_ license",
    LicenseId.unsafe(t"bsd-3-clause"),
    LicenseGroup.Bsd)

  case Bsd3ClauseClear
  extends License
   (md"BSD 3-clause Clear license", LicenseId.unsafe(t"bsd-3-clause-clear"), LicenseGroup.Bsd)
  
  case Cc
  extends License(md"Creative Commons license family", LicenseId.unsafe(t"cc"), LicenseGroup.Cc)
  
  case Cc01
  extends License
   (md"Creative Commons Zero v1.0 Universal", LicenseId.unsafe(t"cc0-1.0"), LicenseGroup.Cc)
  
  case CcBy4
  extends License
   (md"Creative Commons Attribution 4.0", LicenseId.unsafe(t"cc-by-4.0"), LicenseGroup.Cc)
  
  case CcBySa4
  extends License
   (md"Creative Commons Attribution Share Alike 4.0",
    LicenseId.unsafe(t"cc-by-sa-4.0"),
    LicenseGroup.Cc)

  case Wtfpl
  extends License
   (md"Do What The F*ck You Want To Public License", LicenseId.unsafe(t"wtfpl"), LicenseGroup.Wtfpl)
  
  case Ecl2
  extends License(md"Educational Community License v2.0", LicenseId.unsafe(t"ecl-2.0"), LicenseGroup.Ecl)
  
  case Epl1 extends License(md"Eclipse Public License 1.0", LicenseId.unsafe(t"epl-1.0"), LicenseGroup.Epl)
  case Epl2 extends License(md"Eclipse Public License 2.0", LicenseId.unsafe(t"epl-2.0"), LicenseGroup.Epl)
  
  case Eupl11
  extends License(md"European Union Public License 1.1", LicenseId.unsafe(t"eupl-1.1"), LicenseGroup.Eupl)
  
  case Agpl3
  extends License(md"GNU Affero General Public License v3.0", LicenseId.unsafe(t"agpl-3.0"), LicenseGroup.Agpl)
  
  case Gpl extends License(md"GNU General Public License family", LicenseId.unsafe(t"gpl"), LicenseGroup.Gpl)
  case Gpl2 extends License(md"GNU General Public License v2.0", LicenseId.unsafe(t"gpl-2.0"), LicenseGroup.Gpl)
  case Gpl3 extends License(md"GNU General Public License v3.0", LicenseId.unsafe(t"gpl-3.0"), LicenseGroup.Gpl)
  
  case Lgpl
  extends License(md"GNU Lesser General Public License family", LicenseId.unsafe(t"lgpl"), LicenseGroup.Lgpl)
  
  case Lgpl21
  extends License(md"GNU Lesser General Public License v2.1", LicenseId.unsafe(t"lgpl-2.1"), LicenseGroup.Lgpl)
  
  case Lgpl3
  extends License(md"GNU Lesser General Public License v3.0", LicenseId.unsafe(t"lgpl-3.0"), LicenseGroup.Lgpl)
  
  case Isc extends License(md"ISC", LicenseId.unsafe(t"isc"), LicenseGroup.Isc)
  
  case Lppl13c
  extends License(md"LaTeX Project Public License v1.3c", LicenseId.unsafe(t"lppl-1.3c"), LicenseGroup.Lppl)
  
  case MsPl extends License(md"Microsoft Public License", LicenseId.unsafe(t"ms-pl"), LicenseGroup.Ms)
  case Mit extends License(md"MIT", LicenseId.unsafe(t"mit"), LicenseGroup.Mit)
  case Mpl extends License(md"Mozilla Public License 2.0", LicenseId.unsafe(t"mpl-2.0"), LicenseGroup.Mpl)
  case Osl3 extends License(md"Open Software License 3.0", LicenseId.unsafe(t"osl-3.0"), LicenseGroup.Osl)
  
  case PostgreSql
  extends License(md"PostgreSQL License", LicenseId.unsafe(t"postgresql"), LicenseGroup.PostgreSql)
  
  case Ofl11 extends License(md"SIL Open Font License 1.1", LicenseId.unsafe(t"ofl-1.1"), LicenseGroup.Ofl)
  
  case Ncsa
  extends License
      (md"University of Illinois/NCSA Open Source License", LicenseId.unsafe(t"ncsa"), LicenseGroup.Ncsa)
  
  case Unlicense extends License(md"The Unlicense", LicenseId.unsafe(t"unlicense"), LicenseGroup.Unlicense)
  case Zlib extends License(md"zLib License", LicenseId.unsafe(t"zlib"), LicenseGroup.Zlib)
