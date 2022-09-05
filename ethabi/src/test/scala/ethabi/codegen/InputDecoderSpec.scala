package codegen

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.io.Source

class InputDecoderSpec extends AnyWordSpec with Matchers {
  
  "test Uniswap transfer tx" in {
    val json = Source.fromResource("UNI-abi.json").getLines().mkString("\n") //abi
    val selector = "transfer"
    val encoded = "0x000000000000000000000000f6bdeb12aba8bf4cfbfc2a60c805209002223e22000000000000000000000000000000000000000000000005a5a62f156c710000"
    
    val r = Decoder.decodeInput(json,selector,encoded)
    info(s"${r.mkString("(", ", ", ")")}")
    r.size shouldBe (2)
    r(0) shouldBe "000000000000000000000000f6bdeb12aba8bf4cfbfc2a60c805209002223e22"
    r(1) shouldBe "000000000000000000000000000000000000000000000005a5a62f156c710000"
  }
}