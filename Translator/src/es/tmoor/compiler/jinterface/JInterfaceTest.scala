package es.tmoor.compiler.jinterface

import Helpers._
import com.ericsson.otp.erlang._

object JInterfaceTest {
  val self = new OtpSelf("scalaNode@localhost", "cookie")
  val remote = new OtpPeer("erlNode@localhost")
  self.publishPort
  val connection = self.connect(remote)
  val jPid = self.pid
  val msg = Array(325, 0.9065, ("int x = 5;", "request".eAtm, Array((2, "hello"), "14.3".eAtm))).eAuto
  connection.send("c89_compiler", msg)

  val recv = connection.receiveMsg
  println(recv)
}