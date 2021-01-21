import mill._, scalalib._

object Translator extends ScalaModule {
  def scalaVersion = "2.13.4"
  def ivyDeps = Agg (ivy"org.scala-lang.modules::scala-parser-combinators:1.2.0-M1")
  def unmanagedClasspath = T {
    if (!os.exists(millSourcePath / "lib")) Agg()
    else Agg.from(os.list(millSourcePath / "lib").map(PathRef(_)))
  }
}
