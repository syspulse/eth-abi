package ethabi.types

import org.scalatest.{Matchers, WordSpec}
import ethabi.util.Hex

class DynamicBytesSpec extends WordSpec with Matchers {
  "test dynamic bytes encode(length = 20)" in {
    val bytes = Array.fill[Byte](20)(0x24)
    // 0000000000000000000000000000000000000000000000000000000000000014 +
    // 2424242424242424242424242424242424242424000000000000000000000000
    Hex.bytes2Hex(DynamicBytes.typeInfo.encode(DynamicBytes(bytes))) shouldBe "00000000000000000000000000000000000000000000000000000000000000142424242424242424242424242424242424242424000000000000000000000000"
  }

  "test dynamic bytes decode(length = 20)" in {
    val bytes = Array.fill[Byte](20)(0x24)
    val encoded = Hex.hex2Bytes("00000000000000000000000000000000000000000000000000000000000000142424242424242424242424242424242424242424000000000000000000000000")
    val (result, consumed) = DynamicBytes.typeInfo.decode(encoded, 0)
    result.value shouldBe bytes
    consumed shouldBe 64
  }

  "test dynamic bytes encode(length = 50)" in {
    val bytes = Array.fill[Byte](50)(0x24)
    // 0000000000000000000000000000000000000000000000000000000000000032 +
    // 2424242424242424242424242424242424242424242424242424242424242424 +
    // 2424242424242424242424242424242424240000000000000000000000000000
    Hex.bytes2Hex(DynamicBytes.typeInfo.encode(DynamicBytes(bytes))) shouldBe "000000000000000000000000000000000000000000000000000000000000003224242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424240000000000000000000000000000"
  }

  "test dynamic bytes decode(length = 50)" in {
    val bytes = Array.fill[Byte](50)(0x24)
    val encoded = Hex.hex2Bytes("000000000000000000000000000000000000000000000000000000000000003224242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424240000000000000000000000000000")
    val (result, consumed) = DynamicBytes.typeInfo.decode(encoded, 0)
    result.value shouldBe bytes
    consumed shouldBe encoded.length
  }
}