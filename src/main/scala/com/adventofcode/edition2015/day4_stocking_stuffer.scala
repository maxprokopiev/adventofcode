package com.adventofcode.edition2015
import java.security.MessageDigest

class StockingStuffer {
  private def md5(s: String): String = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_)).mkString
  }

  def findNumberFor(secretKey: String, prefix: String): Int = {
    Stream.from(1).find((i: Int) => {
      md5(secretKey + i.toString).startsWith(prefix)
    }).getOrElse(0)
  }
}