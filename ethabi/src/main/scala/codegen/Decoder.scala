package codegen

import io.circe.jawn.decode
import io.circe.generic.auto._

import ethabi.util.Hex
import ethabi.types.TupleType
import ethabi.types.SolType
import ethabi.types.TypeInfo
import scala.util.Failure
import scala.util.Try
import scala.util.Success

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

  def loadAbi(json:String) = {
    decode[Seq[AbiDefinition]](json).getOrElse(Seq())
  }

  def decodeFromJson(json:String, function:String, input:String) = {
    val defs = loadAbi(json)
    decodeInput(defs,function,input)
  }

  def decodeEvent(defs:Seq[AbiDefinition], event:String, input:String):Try[Seq[(String,String,Any)]] = 
    decodeData(defs,event,(d) => {d.isEvent},input)

  def decodeFunction(defs:Seq[AbiDefinition], function:String, input:String):Try[Seq[(String,String,Any)]] = 
    decodeData(defs,function,(d) => {d.isFunction || d.isConstant},input)  

  def decodeInput(defs:Seq[AbiDefinition], function:String, input:String):Try[Seq[(String,String,Any)]] = 
    decodeFunction(defs,function,input)

  def decodeData(defs:Seq[AbiDefinition], selector:String, filter:(AbiDefinition)=>Boolean, input:String):Try[Seq[(String,String,Any)]] = {  
    
    val abiDef = defs.filter(filter).find(_.name.get == selector)
    
    if(!abiDef.isDefined) return Failure(new Exception(s"selector '${selector}' not found"))
    
    try {
      //if (abiDef.outputs.get.exists(_.isTupleTpe)) throw new Exception("tuple type unsupported now")    
      //val types = abiDef.outputs.get.map(_.tpe.toString)
      val (types,names) = abiDef.get.inputs.get.map(t => (t.tpe.toString,t.name)).unzip
        
      def helper[T]: Option[T] = None
      
      val (_, encoders) = types.map(t => (t, helper[String])).map((encoder _).tupled).unzip
      val (results, _) = TupleType.decode(Hex.hex2Bytes(input), 0, encoders)
      val r = encoders.zip(results).zip(names).map(p => {
        val n = p._2
        val t = p._1._1.name
        val v = p._1._1.encode(p._1._2)

        val (name,typ,value) = t match {
          case "address" => (n,t,Hex.bytes2Hex(v.takeRight(20),true))
          case "uint256" | "int256" => (n,t,BigInt(v))
          case "uint32" => (n,t,BigInt(v).toLong)
          case "int32" => (n,t,BigInt(v).toInt)
          case "uint8" => (n,t,BigInt(v).toInt)
          case "int8" => (n,t,BigInt(v).toByte.toInt)
          case _ => (n,t,v)
        }
        
        (name, typ, value)
      })
      Success(r)
    } catch {
      case e:Exception => Failure(e)
      case e:AssertionError => Failure(e)
      case e:Error => Failure(e)
    } 
  }
}