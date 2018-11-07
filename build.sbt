import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConverters._

name := "beacon"
version := "0.1"
scalaVersion := "2.12.7"
libraryDependencies ++= Seq(
  "ch.qos.logback"         % "logback-classic"                  % "1.2.3",
  "com.adrianhurt"         %% "play-bootstrap"                  % "1.2-P26-B3",
  "com.github.j5ik2o"      %% "scala-rakuten-item-search-api"   % "1.0.3",
  "com.github.t3hnar"      %% "scala-bcrypt"                    % "3.1",
  "jp.t2v"                 %% "play2-auth"                      % "0.16.0-SNAPSHOT",
  "jp.t2v"                 %% "play2-auth-test"                 % "0.16.0-SNAPSHOT" % Test,
  "mysql"                  % "mysql-connector-java"             % "6.0.6",
  "org.flywaydb"           %% "flyway-play"                     % "4.0.0",
  "org.postgresql"         % "postgresql"                       % "42.0.0",
  "org.scalatestplus.play" %% "scalatestplus-play"              % "3.1.2" % Test,
  "org.scalikejdbc"        %% "scalikejdbc"                     % "3.2.3",
  "org.scalikejdbc"        %% "scalikejdbc-config"              % "3.2.3",
  "org.scalikejdbc"        %% "scalikejdbc-jsr310"              % "2.5.2",
  "org.scalikejdbc"        %% "scalikejdbc-play-initializer"    % "2.6.+",
  "org.scalikejdbc"        %% "scalikejdbc-syntax-support-macro"% "3.2.3",
  "org.scalikejdbc"        %% "scalikejdbc-test"                % "3.2.3" % Test,
  "org.skinny-framework"   %% "skinny-orm"                      % "2.3.7",
  "com.github.scopt"       %% "scopt"                           % "3.6.0"
)

lazy val envConfig = settingKey[Config]("env-config")

envConfig := {
  val env = sys.props.getOrElse("env", "dev")
  ConfigFactory.parseFile(file("env") / (env + ".conf"))
}

flywayLocations := envConfig.value.getStringList("flywayLocations").asScala
flywayDriver := envConfig.value.getString("jdbcDriver")
flywayUrl := envConfig.value.getString("jdbcUrl")
flywayUser := envConfig.value.getString("jdbcUserName")
flywayPassword := envConfig.value.getString("jdbcPassword")

lazy val commonSettings = Seq(
  name := "beacon",
  version := "0.1",
  scalaVersion := "2.12.7"
)

lazy val app = (project in file("app")).
  settings(commonSettings: _*)