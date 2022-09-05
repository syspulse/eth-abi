package codegen

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
//import org.scalatest.TryValues._
import scala.io.Source

class InputDecoderSpec extends AnyWordSpec with Matchers {
  
  "test Uniswap transfer tx" in {
    val json = Source.fromResource("UNI-abi.json").getLines().mkString("\n") //abi
    val selector = "transfer"
    val encoded = "0x000000000000000000000000f6bdeb12aba8bf4cfbfc2a60c805209002223e22000000000000000000000000000000000000000000000005a5a62f156c710000"
    
    val r = Decoder.decodeFromJson(json,selector,encoded)
    //info(s"${r.mkString("(", ", ", ")")}")
    info(s"$r")

    val s = r.get

    s.size shouldBe (2)
    s(0).toString shouldBe "(dst,address,0xf6bdeb12aba8bf4cfbfc2a60c805209002223e22)"
    s(1).toString shouldBe "(rawAmount,uint256,104170000000000000000)"
  }

  "test Uniswap transfer tx invalid" in {
    val json = Source.fromResource("UNI-abi.json").getLines().mkString("\n") //abi
    val selector = "transferFrom"
    val encoded = "0x000000000000000000000000f6bdeb12aba8bf4cfbfc2a60c805209002223e22"
    
    val r = Decoder.decodeFromJson(json,selector,encoded)
    info(s"$r")
    
    r.isFailure == (true)
  }

  "test USDT transfer tx" in {
    val json = Source.fromResource("USDT-abi.json").getLines().mkString("\n") //abi
    val selector = "transfer"
    val input = "0xa9059cbb00000000000000000000000002779c44ced3c325a6fc7a598e302e760fd0f93f0000000000000000000000000000000000000000000000000000000005f5e100"
    val encoded = input.drop("0xa9059cbb".size)
        
    val r = Decoder.decodeFromJson(json,selector,encoded)
    //info(s"${r.mkString("(", ", ", ")")}")
    info(s"$r")

    val s = r.get

    s.size shouldBe (2)
    s(0).toString shouldBe "(_to,address,0x02779c44ced3c325a6fc7a598e302e760fd0f93f)"
    s(1).toString shouldBe "(_value,uint256,100000000)"
  }


}