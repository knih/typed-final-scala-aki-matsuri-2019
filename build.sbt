val dottyVersion = "0.18.1-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "typed-final-example",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions += "-Ykind-polymorphism",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += ("org.typelevel" %% "cats-core" % "2.0.0").withDottyCompat(scalaVersion.value)
  )

