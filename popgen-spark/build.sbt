import scala.util.Properties

name := """zsi-bio"""

version := "1.0"

scalaVersion := "2.10.4"

val DEFAULT_SPARK_VERSION = "1.6.3"
val DEFAULT_HADOOP_VERSION = "2.7.1"

lazy val sparkVersion = Properties.envOrElse("SPARK_VERSION", DEFAULT_SPARK_VERSION)
lazy val hadoopVersion = Properties.envOrElse("SPARK_HADOOP_VERSION", DEFAULT_HADOOP_VERSION)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "3.0.0-M15" % "test",
  "org.apache.spark" % "spark-core_2.10" % sparkVersion % "provided",
//  "org.apache.spark" % "spark-sql_2.10" % sparkVersion,
  "org.apache.hadoop" % "hadoop-client" % hadoopVersion % "provided",
  "org.bdgenomics.adam" % "adam-core" % "0.16.0"
    exclude("org.apache.spark", "spark-core_2.10")
    exclude("org.apache.hadoop", "hadoop-client")
    exclude("org.bdgenomics.utils", "utils-metrics_2.10"),
  "org.bdgenomics.utils" % "utils-misc_2.10" % "0.2.3"
    exclude("org.apache.spark", "spark-core_2.10")
    exclude("org.apache.hadoop", "hadoop-client"),
  "org.seqdoop" % "hadoop-bam" % "7.2.1"
    exclude("org.apache.hadoop", "hadoop-client"),
  "ai.h2o" % "sparkling-water-core_2.10" % sparkVersion,
  "ai.h2o" % "h2o-algos" % "3.8.2.11",
  "org.apache.systemml" % "systemml" % "0.11.0-incubating"
    exclude("org.apache.spark", "spark-core_2.10")
    exclude("org.apache.hadoop", "hadoop-client")
    exclude("org.apache.hadoop", "hadoop-mapreduce-client-core")
    exclude("org.apache.hadoop", "hadoop-mapreduce-client-app")
    exclude("org.apache.hadoop", "hadoop-mapreduce-client-jobclient")
    exclude("org.apache.hadoop", "hadoop-yarn-api"),
  "com.databricks" % "spark-csv_2.10" % "1.5.0" % "provided",
  "com.databricks" % "spark-csv_2.10" % "1.5.0",
  "com.github.haifengl" % "smile-core" % "1.2.0",
  "com.github.karlhigley" %% "spark-neighbors" % "0.2.2",
  "org.apache.spark" %% "spark-hive" % "1.5.0"
  // "org.ddahl" % "rscala_2.11" % "1.0.13"

)

/*resolvers ++= Seq(
  "Job Server Bintray" at "https://dl.bintray.com/spark-jobserver/maven",
  "OSS Sonatype" at "https://repo1.maven.org/maven2/"
)*/

parallelExecution in Test := false
fork := true

mainClass in assembly := Some("com.zsibio.PopulationStratification")
assemblyJarName in assembly := "zsi-bio-popgen.jar"

assemblyMergeStrategy in assembly := {
  case PathList("org", "apache", "commons", xs@_*) => MergeStrategy.first
  /*case PathList("scala", xs@_*) => MergeStrategy.first
    a nasty workaround!!!
  case PathList("org", xs@_*) => MergeStrategy.first
  case PathList("javax", xs@_*) => MergeStrategy.first
  end*/
  case PathList("fi", "tkk", "ics", xs@_*) => MergeStrategy.first
  case PathList("com", "esotericsoftware", xs@_*) => MergeStrategy.first
  case PathList("org", "objectweb", xs@_*) => MergeStrategy.last
  case PathList("javax", "xml", xs@_*) => MergeStrategy.first
  case PathList("javax", "servlet", xs@_*) => MergeStrategy.first
  case PathList("javax", "annotation", xs@_*) => MergeStrategy.first
  case PathList("javax", "activation", xs@_*) => MergeStrategy.first
  case PathList("javax", "transaction", xs@_*) => MergeStrategy.first
  case PathList("javax", "mail", xs@_*) => MergeStrategy.first
  case PathList("com", "twitter", xs@_*) => MergeStrategy.first
  case PathList("org", "slf4j", xs@_*) => MergeStrategy.first
  //META-INF/maven/com.google.guava/guava/pom.xml
  // Added
  case PathList("htsjdk", xs@_*) => MergeStrategy.first
  case PathList("org", "apache", "bcel", xs@_*) => MergeStrategy.first
  case PathList("org", "apache", "regexp", xs@_*) => MergeStrategy.first
  case PathList("io", "netty", xs@_*) => MergeStrategy.first
  case PathList("com", "codahale", "metrics", xs@_*) => MergeStrategy.first
  case PathList("com", "google", "common", xs@_*) => MergeStrategy.first
  case PathList("org", "apache", "spark", "unused", xs@_*) => MergeStrategy.first
  case PathList("edu", "umd", "cs", "findbugs", xs@_*) => MergeStrategy.first
  case PathList("net", "jcip", "annotations", xs@_*) => MergeStrategy.first
  case PathList("org", "apache", "jasper", xs@_*) => MergeStrategy.first
  case PathList("org", "fusesource", xs@_*) => MergeStrategy.first
  case "parquet.thrift" => MergeStrategy.first
  case PathList(ps@_*) if ps.last endsWith ".html" => MergeStrategy.first
  case "application.conf" => MergeStrategy.concat
  case PathList("org", "apache", "hadoop", xs@_*) => MergeStrategy.first
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
  //case "META-INF/ECLIPSEF.RSA"     => MergeStrategy.discard
  case "META-INF/mimetypes.default" => MergeStrategy.first
  case ("META-INF/ECLIPSEF.RSA") => MergeStrategy.first
  case ("META-INF/mailcap") => MergeStrategy.first
  case ("plugin.properties") => MergeStrategy.first
  case ("META-INF/maven/org.slf4j/slf4j-api/pom.xml") => MergeStrategy.first
  case ("META-INF/maven/com.google.guava/guava/pom.xml") => MergeStrategy.first
  case ("META-INF/maven/org.slf4j/slf4j-api/pom.properties") => MergeStrategy.first
  // case ("META-INF/io.netty.versions.properties") => MergeStrategy.first
  case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
  case x if x.endsWith("pom.properties") => MergeStrategy.first
  case x if x.endsWith("pom.xml") => MergeStrategy.first
  case x if x.endsWith("plugin.xml") => MergeStrategy.first
  //case ("META-INF/maven/com.google.guava/guava/pom.xml") => MergeStrategy.first
  case ("META-INF/native/osx/libjansi.jnilib") => MergeStrategy.first  
  case ("META-INF/native/windows32/jansi.dll") => MergeStrategy.first
  case ("META-INF/native/windows64/jansi.dll") => MergeStrategy.first
  case ("log4j.properties") => MergeStrategy.first
  case ("git.properties") => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}


/*lazy val copyDocAssetsTask = taskKey[Unit]("Copy doc assets")

copyDocAssetsTask := {
  val sourceDir = file("resources/doc-resources")
  val targetDir = (target in(Compile, doc)).value
  IO.copyDirectory(sourceDir, targetDir)
}

copyDocAssetsTask <<= copyDocAssetsTask triggeredBy (doc in Compile)*/

// net.virtualvoid.sbt.graph.Plugin.graphSettings

