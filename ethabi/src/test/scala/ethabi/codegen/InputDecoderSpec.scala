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


  "test Uniswap transfer parameters" in {
    val json1 = Source.fromResource("UNI-abi.json").getLines().mkString("\n") //abi
    val json2 = Source.fromResource("USDT-abi.json").getLines().mkString("\n") //abi
    
    val defs1 = Decoder.loadAbi(json1)
    val defs2 = Decoder.loadAbi(json2)

    val sig1 = defs1.filter(d => d.isFunction).find(_.name.get == "transfer")
    val sig2 = defs2.filter(d => d.isFunction).find(_.name.get == "transfer")
    info(s"${sig1.get.inputs.get(0).name}, ${sig1.get.inputs.get(1).name}")
    info(s"${sig2.get.inputs.get(0).name}, ${sig2.get.inputs.get(1).name}")
        
  }

  "test USDT transfer on transferFrom" in {
    // tx: https://etherscan.io/tx/0xaa27709c88aaa03fb4f81ab525c6d2a589237b3efc7e1dc4bea9d1571cee507a
    val json = Source.fromResource("USDT-abi.json").getLines().mkString("\n") //abi
    val selector = "transfer"
    val input = "0x23b872dd000000000000000000000000b29c9f94d4c9ffa71876802196fb9b396bca631f000000000000000000000000ec30d02f10353f8efc9601371f56e808751f396f000000000000000000000000000000000000000000000000000000004b5eae1a"
    val encoded = input.drop("0x23b872dd".size)
        
    val r = Decoder.decodeFromJson(json,selector,encoded)
    info(s"$r")
  }

  "test USDT transferFrom" in {
    // tx: https://etherscan.io/tx/0xaa27709c88aaa03fb4f81ab525c6d2a589237b3efc7e1dc4bea9d1571cee507a
    val json = Source.fromResource("USDT-abi.json").getLines().mkString("\n") //abi
    val selector = "transferFrom"
    val input = "0x23b872dd000000000000000000000000b29c9f94d4c9ffa71876802196fb9b396bca631f000000000000000000000000ec30d02f10353f8efc9601371f56e808751f396f000000000000000000000000000000000000000000000000000000004b5eae1a"
    val encoded = input.drop("0x23b872dd".size)
        
    val r = Decoder.decodeFromJson(json,selector,encoded)
    info(s"$r")

    val s = r.get

    s.size shouldBe (3)
    //s(0).toString shouldBe "(_to,address,0x02779c44ced3c325a6fc7a598e302e760fd0f93f)"
    //s(1).toString shouldBe "(_value,uint256,100000000)"
  }

  "test USDT event Transfer" in {
    // contract: 0xdac17f958d2ee523a2206206994597c13d831ec7
    // "topics": [
      // "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef", 
      // "0x000000000000000000000000cdbb7436f9d4c21b7627065d1556db29597981f4", 
      // "0x00000000000000000000000080a25bb487e89e79599c9acae6dbc6b8a5f1bcdc", 
      // "0x0000000000000000000000000000000000000000000000000000000000000703"
      //]
    val abi = Decoder.loadAbi(Source.fromResource("USDT-abi.json").getLines().mkString("\n"))
    val selector = "Transfer"
    val topics = Seq( 
      "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef",
      "0x000000000000000000000000cdbb7436f9d4c21b7627065d1556db29597981f4",
      "0x00000000000000000000000080a25bb487e89e79599c9acae6dbc6b8a5f1bcdc", 
      "0x0000000000000000000000000000000000000000000000000000000000000703"
    )
      
    val data = topics.drop(1).map(_.drop(2))
    val encoded = data.mkString("")
    info(s"$encoded")

    val r = Decoder.decodeEvent(abi,selector,encoded)
    info(s"$r")

    val s = r.get

    s.size shouldBe (3)
    //s(0).toString shouldBe "(_to,address,0x02779c44ced3c325a6fc7a598e302e760fd0f93f)"
    //s(1).toString shouldBe "(_value,uint256,100000000)"
  }

}