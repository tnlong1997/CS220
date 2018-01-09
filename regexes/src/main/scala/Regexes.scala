import scala.util.matching.Regex

object Regexes extends hw.regex.RegexLike {
	def notAlphanumeric : Regex = new Regex("\\W+")
	
	def phone : Regex = {
		new Regex("[(]\\d{3}[)]\\s\\d{3}[-]\\d{4}")
	}

	def time : Regex = {
		new Regex("(0[0-9]|1[0-9]|2[0-3])[:][0-5][0-9]")
	}

	def zip : Regex = {
		new Regex("\\d{5}([-]\\d{4})?")
	}

	def comment : Regex = {
		new Regex("^([/][*])(.|\\n)*([*][/])$")
	}

	def numberPhrase : Regex = {
		new Regex("((twenty)|(thirty)|(forty)|(fifty)|(sixty)|(seventy)|(eighty)|(ninety))([-]((one)|(two)|(three)|(four)|(five)|(six)|(seven)|(eight)|(nine)))?")
	}

	def roman : Regex = {
		new Regex("([X]{0,3})((I)|(II)|(III)|(IV)|(V)|(VI)|(VII)|(VIII)|(IV))")
	}

	def date : Regex = {
		new Regex("(\\d{4}[-](((0[13578]|1[02])[-](0[1-9]|[12][0-9]|3[01]))|((0[469]|11)[-](0[1-9]|[12][0-9]|30))|(02[-](0[1-9]|1[0-9]|2[0-8])))|(\\d{2}(([02468][048])|([13579][26]))[-](02)[-](29)))")
		//new Regex("\\d{2}([02468][048]|[13579][26])[-](02)[-]29")
	}

	def evenParity : Regex = {
		new Regex("(([02468]*[13579]){2})*([02468]*)$")
	}
}