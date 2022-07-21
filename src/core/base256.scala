package irk

import gastronomy.*
import gossamer.*
import rudiments.*

trait Base256 extends EncodingScheme

object Base256:
  private val charset: Text = List(t"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
      t"ႠႡႢႣႤႥႦႧႨႩႪႫႬႮႯႰႱႲႴႵႶႷႸႹႻႼႾႿჀჁჂჃჄჅაბგდევზთიკლმნოპჟრსტუფქღყშჩცძწჭხჯჰჱჲჳჴჵჶჷჸჹჺჾ",
      t"БГДЖЗИЙЛПФЦЧШЩЪЫЬЭЮЯбвгджзийклмнптфцчшщъыьэюяΔΘΛΞΠΣΦΨΩαβγδεζηθιλμξπρςστυφψωϊϋ",
      t"ϐϑϔϕϖϗϘϙϚϛϜϝϞϟϠϡϢϣϤϥϦϧϨϪϫϬϭϮϯϰϱϵ϶ϷϸϻϼϽϾϿ123456789").join
 
  given ByteEncoder[Base256] = bytes =>
    unsafely(bytes.map(_.toInt + 128).map[Char](charset(_)).map(_.show).join)