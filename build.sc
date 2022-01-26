import mill._
import mill.scalalib.publish._
import scalalib._
import scalafmt._
import coursier.maven.MavenRepository

val spinalVer = "1.6.0"
val defaultVersions = Map(
  "spinalhdl-core" -> ("com.github.spinalhdl", spinalVer, false),
  "spinalhdl-lib" -> ("com.github.spinalhdl", spinalVer, false),
  "spinalhdl-idsl-plugin" -> ("com.github.spinalhdl", spinalVer, false),
  "scalatest" -> ("org.scalatest", "3.2.10", false)
)

val commonScalaVersion = "2.12.13"

def getVersion(dep: String) = {
  val (org, ver, cross) = defaultVersions(dep)
  val version = sys.env.getOrElse(dep + "Version", ver)
  if (cross)
    ivy"$org:::$dep:$version"
  else
    ivy"$org::$dep:$version"
}

object meowv64 extends SbtModule with ScalafmtModule {
  override def scalaVersion = commonScalaVersion

  override def millSourcePath = os.pwd

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("spinalhdl-core"),
    getVersion("spinalhdl-lib"),
    getVersion("scalatest")
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("spinalhdl-idsl-plugin")
  )

  object test
      extends Tests
      with TestModule.ScalaTest
      with ScalafmtModule {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      getVersion("spinalhdl-core"),
      getVersion("spinalhdl-lib"),
      getVersion("scalatest")
    )

    override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
      getVersion("spinalhdl-idsl-plugin")
    )
  }
}
