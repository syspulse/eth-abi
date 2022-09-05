package codegen

import io.circe.jawn.decode
import io.circe.generic.auto._

import ethabi.util.Hex
import ethabi.types.TupleType
import ethabi.types.SolType
import ethabi.types.TypeInfo

object Decoder {
  import scala.reflect.runtime
  import runtime.universe.TermName
  private val mirror = runtime.universe.runtimeMirror(getClass.getClassLoader)
  
  private def encoder(tpe: String, arg: Option[String]): (Option[SolType], TypeInfo[SolType]) = {
    val isGenerated = tpe.startsWith("Int") || tpe.startsWith("Uint") || tpe.startsWith("Bytes")
    val modulePath = if (isGenerated) s"ethabi.types.generated.$tpe" else s"ethabi.types.$tpe"
    val module = mirror.staticModule(modulePath)
    val instance = mirror.reflect(mirror.reflectModule(module).instance)
    val method = module.info.member(TermName("from")).asMethod
    val result = arg.map(instance.reflectMethod(method)(_).asInstanceOf[SolType])
    // force eval lazy
    val encoderMethod = module.info.member(TermName("typeInfo")).asMethod
    val typeInfo = instance.reflectMethod(encoderMethod)().asInstanceOf[TypeInfo[SolType]]
    (result, typeInfo)
  }

  def decodeInput(json:String, function:String, input:String) = {
    val defs = decode[Seq[AbiDefinition]](json).getOrElse(Seq())
    val selector = function
    
    val abiDef = defs.filter(d => d.isFunction || d.isConstant).find(_.name.get == selector).get
    
    //if (abiDef.outputs.get.exists(_.isTupleTpe)) throw new Exception("tuple type unsupported now")
    
    //val types = abiDef.outputs.get.map(_.tpe.toString)
    val (types,names) = abiDef.inputs.get.map(t => (t.tpe.toString,t.name)).unzip
      
    def helper[T]: Option[T] = None
    
    val (_, encoders) = types.map(t => (t, helper[String])).map((encoder _).tupled).unzip
    val (results, _) = TupleType.decode(Hex.hex2Bytes(input), 0, encoders)
    val r = encoders.zip(results).zip(names).map(p => {
      val n = p._2
      val t = p._1._1.name
      val v = p._1._1.encode(p._1._2)

      val (name,typ,value) = t match {
        case "address" => (n,t,Hex.bytes2Hex(v,true))
        case "uint256" => (n,t,BigInt(v))
        case _ => (n,t,v)
      }
      
      (name, typ, value)
    })
    r
  }  
}