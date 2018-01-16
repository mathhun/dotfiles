addCommandAlias("c", "~compile")
addCommandAlias("C", "compile")
addCommandAlias("s", "console")
addCommandAlias("l", "reload")
addCommandAlias("i", "inspect")

addCommandAlias("r", "~run")
addCommandAlias("R", "run")
addCommandAlias("m", "runMain")

addCommandAlias("p", "package")
addCommandAlias("a", "assembly")

addCommandAlias("t", "~test")
addCommandAlias("T", "test")
addCommandAlias("to", "~testOnly")
addCommandAlias("TO", "testOnly")
addCommandAlias("tq", "~testQuick")
addCommandAlias("TQ", "testQuick")

addCommandAlias("beginDebug", List(
  "set maxErrors := 5",
  "set logLevel := Level.Error",
  "set triggeredMessage := Watched.clearWhenTriggered"
).mkString(";", ";", ""))

addCommandAlias("endDebug", List(
  "set maxErrors := 100",
  "set logLevel := Level.Info",
  "set triggeredMessage := Watched.defaultTriggeredMessage"
).mkString(";", ";", ""))
