name := "lucepe"

version := "0.1"

scalaVersion := "2.11.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"     

resolvers += "codehaus Repository" at "http://repository.codehaus.org/" 

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.3"

libraryDependencies += "com.google.protobuf" % "protobuf-java" % "2.5.0"

libraryDependencies += "com.espertech" % "esper" % "5.0.0"

libraryDependencies += "org.apache.lucene" % "lucene-spatial" % "4.8.1"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5" % "test"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

