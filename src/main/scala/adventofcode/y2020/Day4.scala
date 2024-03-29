package adventofcode.y2020

import adventofcode.common.NumberHelper.isInRange

object Day4 extends Year2020 {
  override val day = 4

  override def runDay(input: String): Unit = {
    val passports = input.split("\n\n").map(Passport.parsePassport)

    val allRequiredFieldsPassports = passports.filter(_.allRequiredFields).toSeq

    printDayPart(1, allRequiredFieldsPassports.size, "valid passports: %s")
    printDayPart(2, allRequiredFieldsPassports.count(_.allFieldsValid), "passports with valid fields: %s")
  }

  private object Passport {
    private val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

    private val hgtRE = """(\d+)(cm|in)""".r
    private val hclRE = """#[0-9a-f]{6}""".r
    private val eclValues = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    private val pidRE = """[0-9]{9}""".r

    def parsePassport(str: String): Passport = Passport(parseFields(str))
    private def parseFields(str: String) = str.split("\\s")
      .map(_.split(":")).map { case Array(k, v) => (k, v) }.toMap

    private val fieldValidators = Map(
      "byr" -> ((_: String).toIntOption.exists(isInRange(_, 1920, 2002))),
      "iyr" -> ((_: String).toIntOption.exists(isInRange(_, 2010, 2020))),
      "eyr" -> ((_: String).toIntOption.exists(isInRange(_, 2020, 2030))),
      "hgt" -> ((_: String) match {
        case hgtRE(hgt, "cm") => isInRange(hgt.toInt, 150, 193)
        case hgtRE(hgt, "in") => isInRange(hgt.toInt, 59, 76)
        case _ => false
      }),
      "hcl" -> (hclRE.matches(_)),
      "ecl" -> (eclValues.contains(_)),
      "pid" -> (pidRE.matches(_)),
    )
  }

  private case class Passport(fields: Map[String, String]) {
    def allRequiredFields: Boolean = Passport.requiredFields.subsetOf(fields.keySet)

    def allFieldsValid: Boolean = Passport.fieldValidators.forall {
      case (fieldKey, validator) => validator(fields(fieldKey))
    }
  }
}
