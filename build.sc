import mill._, scalalib._

object Translator extends ScalaModule {
  def scalaVersion = "2.13.4"
  //def ivyDeps = Agg (ivy"org.erlang.otp:jinterface:1.6.1")
  def unmanagedClasspath = T {
    if (!os.exists(millSourcePath / "lib")) Agg()
    else Agg.from(os.list(millSourcePath / "lib").map(PathRef(_)))
  }
}