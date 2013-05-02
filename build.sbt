import AssemblyKeys._ 

assemblySettings

name := "chocola"

version := "0.01-prealpha"

scalaVersion := "2.10.0"

resolvers += "choco.repos" at "http://www.emn.fr/z-info/choco-repo/mvn/repository/"

libraryDependencies += "choco" % "choco-solver" % "13.03"
