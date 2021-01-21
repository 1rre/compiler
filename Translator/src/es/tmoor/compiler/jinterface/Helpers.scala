package es.tmoor.compiler.jinterface

import com.ericsson.otp.erlang.{
  OtpErlangAtom,
  OtpErlangBinary,
  OtpErlangDouble,
  OtpErlangInt,
  OtpErlangList,
  OtpErlangMap,
  OtpErlangObject,
  OtpErlangString,
  OtpErlangTuple,
  OtpNode
}

object Helpers {
  case class Atom(s: String)
  implicit class otpNode(args: Tuple2[String, String]) {
    def node = new OtpNode(args._1, args._2)
  }
  implicit class erlInt(obj: Int) {
    def eInt = new OtpErlangInt(obj)
  }
  implicit class erlDouble(obj: Double) {
    def eDbl = new OtpErlangDouble(obj)
  }
  implicit class erlString(obj: String) {
    def eBin = new OtpErlangBinary(obj)
    def eLst = new OtpErlangList(obj)
    def eStr = new OtpErlangString(obj)
    def eAtm = new OtpErlangAtom(obj)
    def node = new OtpNode(obj)
  }
  implicit class erlAtom(obj: Atom) {
    def eAtm = new OtpErlangAtom(obj.s)
  }
  implicit class erlArray(obj: Array[_]) {
    def eLst = new OtpErlangList(obj.map(_.eAuto))
    def eTpl = new OtpErlangTuple(obj.map(_.eAuto))
  }
  implicit class erlTuple(obj: Product) {
    def eLst = new OtpErlangList(obj.productIterator.map(_.eAuto).toArray)
    def eTpl = new OtpErlangTuple(obj.productIterator.map(_.eAuto).toArray)
  }
  implicit class erl(obj: Any) {
    def eAuto: OtpErlangObject = obj match {
      case i: Int => i.eInt
      case d: Double => d.eDbl
      case s: String => s.eStr
      case s: Atom => s.eAtm
      case a: Array[_] => a.eLst
      case t: Product => t.eTpl
      case o: OtpErlangObject => o
      case _ => sys.error("Type not found")
    }
  }
}