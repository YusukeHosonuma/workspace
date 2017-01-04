import java.security.AlgorithmParameters

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

object CipherSample {

  def main(args: Array[String]) = {

    val message = "hello!"

    println("original: %s".format(message))

    val secretKeyArr = new Array[Byte](128 / 8)
    val secretKey = new SecretKeySpec(secretKeyArr, "AES")

    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.ENCRYPT_MODE, secretKey)
    val encrypt = cipher.doFinal(message.getBytes)

    println("encrypt : %s".format(toHexString(encrypt)))

    val cipher2 = Cipher.getInstance("AES")
    cipher2.init(Cipher.DECRYPT_MODE, secretKey)
    val decrypt = cipher2.doFinal(encrypt)

    println("decrypt : %s".format(new String(decrypt)))
  }

  def toHexString(bytes: Array[Byte]): String = {
    bytes.map(byte => {
      val b1 = (byte & 0xF0) / 16
      val b2 = (byte & 0x0F)
      List(b1, b2).map(b => b match {
        case n if (n < 10) => n.toString
        case n => ('A' + (n - 10)).toChar
      }).mkString
    }).mkString(" ")
  }
}