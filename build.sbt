name := "stoplight"

version := "1.0"

fork := true

javaOptions in run += "-Djava.library.path=/Users/marc/Downloads/opencv-2.4.11/build/lib/"

libraryDependencies  ++= Seq(
  "joda-time" % "joda-time" % "2.7",
  "org.scalanlp" %% "breeze" % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2"
)