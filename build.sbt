name := "BooleanDsl"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.scalatest"     %% "scalatest"       % "3.2.3"   % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test,
  "org.scalacheck"    %% "scalacheck"      % "1.15.1"  % Test
)
