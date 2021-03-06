name := "Parens"

version := "0.00000000000001"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-encoding", "UTF-8")

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest"        % "1.6.1" % "test",
    "org.jmock"      % "jmock"            % "2.5.1" % "test",
    "org.jmock"      % "jmock-legacy"     % "2.5.1" % "test",
    "cglib"      % "cglib-nodep"      % "2.1_3" % "test",
    "org.objenesis"  % "objenesis"        % "1.0"   % "test",
    "junit"      % "junit"            % "4.8.2" % "test"
)
