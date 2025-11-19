scalaVersion := "2.13.0"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % Test

testFrameworks += new TestFramework("utest.runner.Framework")