import mill._
import mill.scalalib.publish._
import scalalib._
import scalafmt._
import coursier.maven.MavenRepository
import $ivy.`com.goyeau::mill-scalafix:0.2.5`
import com.goyeau.mill.scalafix.ScalafixModule

// learned from https://github.com/OpenXiangShan/fudian/blob/main/build.sc
val defaultVersions = Map(
  "chisel3" -> ("edu.berkeley.cs", "3.5.0-RC2", false),
  "chisel3-plugin" -> ("edu.berkeley.cs", "3.5.0-RC2", true),
  "chiseltest" -> ("edu.berkeley.cs", "0.5.0-RC2", false),
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

object meowv64 extends SbtModule with ScalafmtModule with ScalafixModule {
  override def scalaVersion = commonScalaVersion

  override def millSourcePath = os.pwd

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel3"),
    getVersion("scalatest")
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("chisel3-plugin")
  )

  override def scalacOptions = super.scalacOptions() ++
    Seq("-deprecation", "-unchecked", "-Xsource:2.11") ++ // for chisel3
    Seq("-Ywarn-unused", "-Ywarn-adapted-args", "-deprecation") // for scalafix

  override def scalafixIvyDeps = Agg(
    ivy"com.github.liancheng::organize-imports:0.5.0"
  )

  object test
      extends Tests
      with TestModule.ScalaTest
      with ScalafmtModule
      with ScalafixModule {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      getVersion("scalatest"),
      getVersion("chiseltest")
    )

    override def scalafixIvyDeps = Agg(
      ivy"com.github.liancheng::organize-imports:0.5.0"
    )

    override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
      getVersion("chisel3-plugin")
    )
  }
}
