shellPrompt := { state =>
  def textColor(color: Int)      = { s"\033[38;5;${color}m" }
  def backgroundColor(color:Int) = { s"\033[48;5;${color}m" }
  def reset                      = { s"\033[0m" }

  def formatText(str: String)(txtColor: Int, backColor: Int) = {
    s"${textColor(txtColor)}${backgroundColor(backColor)}${str}${reset}"
  }
  val red    = 1
  val green  = 2
  val yellow = 11
  val white  = 15
  val black  = 16
  val orange = 166

  formatText(s"[${name.value}]")(white, orange) + " " +
  formatText("\u276f")(green, black) +
  formatText("\u276f")(yellow, black) +
  formatText("\u276f ")(red, black)
}

libraryDependencies += "com.lihaoyi" % "ammonite" % "0.7.0" % "test" cross CrossVersion.full
initialCommands in (Test, console) := """ammonite.Main().run()"""
// sbt projectName/test:console
