package adventofcode.y2020

object Day4 extends Year2020 {
  override val day = 4

  private val passports = inputString.split("\n\n").map(Passport.parsePassport)

  private val allRequiredFieldsPassports = passports.filter(_.allRequiredFields).toSeq

  printDayPart(1, allRequiredFieldsPassports.size, "valid passports: %s")
  printDayPart(2, allRequiredFieldsPassports.count(_.allFieldsValid), "passports with valid fields: %s")

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
      "byr" -> ((_: String).toIntOption.exists(yr => 1920 <= yr && yr <= 2002)),
      "iyr" -> ((_: String).toIntOption.exists(yr => 2010 <= yr && yr <= 2020)),
      "eyr" -> ((_: String).toIntOption.exists(yr => 2020 <= yr && yr <= 2030)),
      "hgt" -> ((_: String) match {
        case hgtRE(hgtStr, "cm") =>
          val hgt = hgtStr.toInt
          150 <= hgt && hgt <= 193
        case hgtRE(hgtStr, "in") =>
          val hgt = hgtStr.toInt
          59 <= hgt && hgt <= 76
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
