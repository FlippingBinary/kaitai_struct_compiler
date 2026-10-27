package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.JavaScriptTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils, ExternalType}

class JavaScriptCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalDoc
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with FetchInstances
    with EveryWriteIsExpression
    with SwitchIfOps {
  import JavaScriptCompiler._

  override val translator = new JavaScriptTranslator(typeProvider, importList)

  private var subIOCapturedVars: List[String] = Nil

  override def indent: String = "  "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.js"

  override def outImports(topClass: ClassSpec) = {
    val impList = importList.toList
    val quotedImpList = impList.map((x) => s"'$x'")
    val defineArgs = ("'exports'" +: quotedImpList).mkString(", ")
    val exportsArgs = ("exports" +: quotedImpList.map((x) => s"require($x)")).mkString(", ")
    val argClasses = types2class(topClass.name, false) +: impList.map((x) => x.split('/').last)
    val rootArgs = argClasses.map((x) => if (x == "KaitaiStream") s"root.$x" else s"root.$x || (root.$x = {})").mkString(", ")
    val factoryParams = argClasses.map((x) => if (x == "KaitaiStream") x else s"${x}_").mkString(", ")

    "(function (root, factory) {\n" +
      indent + "if (typeof define === 'function' && define.amd) {\n" +
      indent * 2 + s"define([$defineArgs], factory);\n" +
      indent + "} else if (typeof exports === 'object' && exports !== null && typeof exports.nodeType !== 'number') {\n" +
      indent * 2 + s"factory($exportsArgs);\n" +
      indent + "} else {\n" +
      indent * 2 + s"factory($rootArgs);\n" +
      indent + "}\n" +
      s"})(typeof self !== 'undefined' ? self : this, function ($factoryParams) {"
  }

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    importList.add("kaitai-struct/KaitaiStream")
  }

  override def fileFooter(name: String): Unit = {
    out.puts(s"${type2class(name)}_.${type2class(name)} = ${type2class(name)};")
    out.puts("});")
  }

  override def externalTypeDeclaration(extType: ExternalType): Unit = {
    val className = type2class(extType.name.head)
    importList.add(s"./$className")
  }

  override def classHeader(name: List[String]): Unit = {
    val shortClassName = type2class(name.last)

    val addNameExpr = if (name.size > 1) {
      s" = ${types2class(name.takeRight(2), false)}"
    } else {
      ""
    }

    out.puts
    out.puts(s"var $shortClassName$addNameExpr = (function() {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = {
    out.puts
    out.puts(s"return ${type2class(name.last)};")
    out.dec
    out.puts("})();")
  }

  override def classConstructorHeader(name: List[String], parentClassName: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val endianSuffix = if (isHybrid) {
      ", _is_le"
    } else {
      ""
    }

    val paramsList = Utils.join(params.map((p) => paramName(p.id)), ", ", ", ", "")

    out.puts(s"function ${type2class(name.last)}(_io, _parent, _root$endianSuffix$paramsList) {")
    out.inc
    out.puts("this._io = _io;")
    out.puts("this._parent = _parent;")
    if (name == rootClassName) {
      out.puts("this._root = _root || this;")
    } else {
      out.puts("this._root = _root;")
    }

    if (isHybrid)
      out.puts("this._is_le = _is_le;")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))

    if (config.readStoresPos) {
      out.puts("this._debug = {};")
    }
    out.puts
  }

  override def classConstructorFooter: Unit = {
    if (config.readWrite) {
      out.puts("Object.defineProperty(this, '_dirty', { value: true, writable: true, enumerable: false });")
    }
    out.dec
    out.puts("}")
  }

  override def runRead(name: List[String]): Unit = {
    out.puts("this._read();")
  }

  override def runReadCalc(): Unit = {
    out.puts
    out.puts(s"if (this._is_le === true) {")
    out.inc
    out.puts("this._readLE();")
    out.dec
    out.puts("} else if (this._is_le === false) {")
    out.inc
    out.puts("this._readBE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("throw new KaitaiStream.UndecidedEndiannessError();")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val suffix = endian match {
      case Some(e) => Utils.upperUnderscoreCase(e.toSuffix)
      case None => ""
    }
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._read$suffix = function() {")
    out.inc
  }

  override def readFooter() = {
    if (config.readWrite) {
      out.puts("this._dirty = false;")
    }
    out.dec
    out.puts("}")
  }

  override def fetchInstancesHeader(): Unit = {
    out.puts
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._fetchInstances = function() {")
    out.inc
  }

  override def fetchInstancesFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def attrInvokeFetchInstances(baseExpr: Ast.expr, exprType: DataType, dataType: DataType): Unit = {
    out.puts(s"${expression(baseExpr)}._fetchInstances();")
  }

  override def attrInvokeInstance(instName: InstanceIdentifier): Unit = {
    out.puts(s"this.${publicMemberName(instName)};")
  }

  override def instanceHasValueIfHeader(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (${privateMemberName(instName)} !== undefined) {")
    out.inc
  }

  override def instanceHasValueIfFooter(): Unit = {
    out.dec
    out.puts("}")
  }

  //<editor-fold desc="writing">

  override def runWriteCalc(): Unit = {
    out.puts
    out.puts("if (this._is_le === true) {")
    out.inc
    out.puts("this._write_SeqLE();")
    out.dec
    out.puts("} else if (this._is_le === false) {")
    out.inc
    out.puts("this._write_SeqBE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("throw new KaitaiStream.UndecidedEndiannessError();")
    out.dec
    out.puts("}")
  }

  override def writeHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val className = type2class(typeProvider.nowClass.name.last)
    val suffix = endian match {
      case Some(e) => Utils.upperUnderscoreCase(e.toSuffix)
      case None => ""
    }

    if (endian.isEmpty) {
      out.puts
      out.puts(s"$className.prototype._write = function(io) {")
      out.inc
      out.puts("this._write_Seq(io);")
      out.puts("this._fetchInstances();")
      out.puts("this._io.writeBackChildStreams();")
      out.dec
      out.puts("}")
    }

    out.puts(s"$className.prototype._write_Seq$suffix = function(${if (endian.isEmpty) "io" else ""}) {")
    out.inc
    if (endian.isEmpty) {
      out.puts("if (io !== undefined) {")
      out.inc
      out.puts("this._io = io;")
      out.dec
      out.puts("}")
      out.puts("if (this._dirty) {")
      out.inc
      out.puts("throw new KaitaiStream.ConsistencyNotCheckedError(\"consistency not checked: _check() has not been called since the last modification of the object\");")
      out.dec
      out.puts("}")
    }
  }

  override def writeFooter(): Unit = {
    out.dec
    out.puts("}")
  }

  override def writeInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._write${idToSetterStr(instName)} = function() {")
    out.inc
    instanceClearWriteFlag(instName)
  }

  override def writeInstanceFooter(): Unit = {
    out.dec
    out.puts("}")
  }

  override def checkHeader(): Unit = {
    out.puts
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._check = function() {")
    out.inc
  }

  override def checkFooter(): Unit = {
    out.puts("this._dirty = false;")
    out.dec
    out.puts("}")
  }

  override def checkInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (this._enabled${idToSetterStr(instName)}) {")
    out.inc
  }

  override def checkInstanceFooter(): Unit = {
    out.dec
    out.puts("}")
  }

  override def internalEnumIntType(basedOn: IntType): DataType = basedOn

  override def attrSetProperty(base: Ast.expr, propName: Identifier, value: String): Unit = {
    out.puts(s"${expression(base)}.${idToStr(propName)} = $value;")
  }

  override def attrPrimitiveWrite(
    io: String,
    valueExpr: Ast.expr,
    dataType: DataType,
    defEndian: Option[FixedEndian],
    exprTypeOpt: Option[DataType]
  ): Unit = {
    val expr = expression(valueExpr)

    val stmt = dataType match {
      case t: ReadableType =>
        s"$io.write${Utils.capitalize(t.apiCall(defEndian))}($expr)"
      case BitsType1(bitEndian) =>
        s"$io.writeBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1, ${translator.boolToInt(valueExpr)})"
      case BitsType(width: Int, bitEndian) =>
        s"$io.writeBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width, $expr)"
      case _: BytesType =>
        s"$io.writeBytes($expr)"
    }
    out.puts(stmt + ";")
  }

  override def attrBytesLimitWrite(io: String, expr: Ast.expr, size: String, term: Int, padRight: Int): Unit =
    out.puts(s"$io.writeBytesLimit(${expression(expr)}, $size, $term, $padRight);")

  override def attrUserTypeInstreamWrite(io: String, valueExpr: Ast.expr, dataType: DataType, exprType: DataType) = {
    out.puts(s"${expression(valueExpr)}._write_Seq($io);")
  }

  override def exprStreamToByteArray(io: String): String =
    s"$io.toByteArray()"

  override def exprIORemainingSize(io: String): String =
    s"$io.size - $io.pos"

  override def attrBasicCheck(checkExpr: Ast.expr, actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if (${expression(checkExpr)}) {")
    out.inc
    out.puts(s"throw new KaitaiStream.ConsistencyError($msgStr, ${expression(expected)}, ${expression(actual)});")
    out.dec
    out.puts("}")
  }

  override def attrObjectsEqualCheck(actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if (!KaitaiStream.objectsEqual(${expression(actual)}, ${expression(expected)})) {")
    out.inc
    out.puts(s"throw new KaitaiStream.ConsistencyError($msgStr, ${expression(expected)}, ${expression(actual)});")
    out.dec
    out.puts("}")
  }

  override def attrParentParamCheck(actualParentExpr: Ast.expr, ut: UserType, shouldDependOnIo: Option[Boolean], msg: String): Unit = {
    val (expectedParent, dependsOnIo) = ut.forcedParent match {
      case Some(USER_TYPE_NO_PARENT) => ("null", false)
      case Some(fp) =>
        (expression(fp), userExprDependsOnIo(fp))
      case None => ("this", false)
    }
    if (shouldDependOnIo.map(shouldDepend => dependsOnIo != shouldDepend).getOrElse(false))
      return

    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if (${expression(actualParentExpr)} !== $expectedParent) {")
    out.inc
    out.puts(s"throw new KaitaiStream.ConsistencyError($msgStr, $expectedParent, ${expression(actualParentExpr)});")
    out.dec
    out.puts("}")
  }

  override def attrIsEofCheck(io: String, expectedIsEof: Boolean, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    val eofExpr = s"$io.isEof()"
    val ifExpr = if (expectedIsEof) {
      s"!($eofExpr)"
    } else {
      eofExpr
    }
    out.puts(s"if ($ifExpr) {")
    out.inc
    out.puts(s"throw new KaitaiStream.ConsistencyError($msgStr, 0, ${exprIORemainingSize(io)});")
    out.dec
    out.puts("}")
  }

  override def condIfIsEofHeader(io: String, wantedIsEof: Boolean): Unit = {
    val eofExpr = s"$io.isEof()"
    val ifExpr = if (!wantedIsEof) {
      s"!($eofExpr)"
    } else {
      eofExpr
    }

    out.puts(s"if ($ifExpr) {")
    out.inc
  }

  override def condIfIsEofFooter: Unit = condIfFooter

  override def blockScopeHeader: Unit = {
    out.puts("{")
    out.inc
  }

  override def blockScopeFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatCommonHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts(s"for (var i = 0; i < ${privateMemberName(id)}.length; i++) {")
    out.inc
  }

  override def condRepeatCommonFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def pushPosForSubIOWriteBackHandler(io: String): Unit =
    out.puts(s"var _pos2 = $io.pos;")

  override def seekRelative(io: String, relPos: String): Unit =
    out.puts(s"$io.seek($io.pos + ($relPos));")

  override def allocateIOFixed(varName: Identifier, size: String): String = {
    val langName = idToStr(varName)
    val ioName = s"_io_$langName"

    out.puts(s"var $ioName = new $kstreamName($size);")
    ioName
  }

  override def subIOWriteBackHeader(subIO: String, rep: RepeatSpec, process: Option[ProcessExpr]): String = {
    val parentIoName = "parent"
    out.puts("var _this = this;")
    val indexArg = rep match {
      case NoRepeat => None
      case _ => Some("i")
    }
    val processValArg = process.map(proc => proc match {
      case _: ProcessXor | _: ProcessRotate | _: ProcessCustom => Some("_process_val")
      case _ => None
    }).getOrElse(None)
    subIOCapturedVars = (Some(subIO) ++ indexArg ++ processValArg).toList
    out.puts(s"(function(${subIOCapturedVars.mkString(", ")}) {")
    out.inc
    out.puts(s"$subIO.setWriteBackHandler(_pos2, function($parentIoName) {")
    out.inc

    translator.inSubIOWriteBackHandler = true

    parentIoName
  }

  override def subIOWriteBackFooter(subIO: String): Unit = {
    translator.inSubIOWriteBackHandler = false

    out.dec
    out.puts("});")
    out.dec
    out.puts(s"})(${subIOCapturedVars.mkString(", ")});")
    subIOCapturedVars = Nil
  }

  override def addChildIO(io: String, childIO: String): Unit =
    out.puts(s"$io.addChildStream($childIO);")

  private def customProcessExpr(name: List[String], args: Seq[Ast.expr]): String = {
    val nameInit = name.init
    val pkgName = if (nameInit.isEmpty) "" else nameInit.mkString("-") + "/"
    val procClass = type2class(name.last)

    importList.add(s"$pkgName$procClass")

    s"new ${procClass}_.${procClass}(${args.map(expression).mkString(", ")})"
  }

  override def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec, dataType: BytesType, exprTypeOpt: Option[DataType]): Unit = {
    val srcExpr = expression(Identifier.itemExpr(varSrc, rep))

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val argStr = if (translator.inSubIOWriteBackHandler) "_process_val" else expression(xorValue)
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        s"$kstreamName.$procName($srcExpr, $argStr)"
      case ProcessZlib =>
        s"$kstreamName.unprocessZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val argStr = if (translator.inSubIOWriteBackHandler) "_process_val" else expression(rotValue)
        val rotExpr = if (isLeft) {
          s"8 - ($argStr)"
        } else {
          argStr
        }
        s"$kstreamName.processRotateLeft($srcExpr, $rotExpr, 1)"
      case ProcessCustom(name, args) =>
        val procName = if (translator.inSubIOWriteBackHandler) {
          "_process_val"
        } else {
          val n = s"_process_${idToStr(varSrc)}"
          out.puts(s"var $n = ${customProcessExpr(name, args)};")
          n
        }
        s"$procName.encode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false, dataType, dataType)
  }

  override def attrUnprocessPrepareBeforeSubIOHandler(proc: ProcessExpr, varSrc: Identifier): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"var _process_val = ${expression(xorValue)};")
      case ProcessRotate(_, rotValue) =>
        out.puts(s"var _process_val = ${expression(rotValue)};")
      case ProcessZlib => // no process arguments
      case ProcessCustom(name, args) =>
        out.puts(s"var _process_val = ${customProcessExpr(name, args)};")
    }
  }

  override def instanceWriteFlagDeclaration(attrName: InstanceIdentifier): Unit = {}

  override def instanceWriteFlagInit(attrName: InstanceIdentifier): Unit = {
    // `_shouldWrite*` and `_enabled*` needs to be non-enumerable
    out.puts(s"Object.defineProperty(this, '_shouldWrite${idToSetterStr(attrName)}', { value: false, writable: true, enumerable: false });")
    out.puts(s"Object.defineProperty(this, '_enabled${idToSetterStr(attrName)}', { value: true, writable: true, enumerable: false });")
  }

  override def instanceSetWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"this._shouldWrite${idToSetterStr(instName)} = this._enabled${idToSetterStr(instName)};")
  }

  override def instanceClearWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"this._shouldWrite${idToSetterStr(instName)} = false;")
  }

  override def instanceEnabledSetter(instName: InstanceIdentifier): Unit = {
    // the flag is a plain property, so it can be assigned directly by the user
  }

  override def instanceCheckWriteFlagAndWrite(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (this._shouldWrite${idToSetterStr(instName)}) {")
    out.inc
    out.puts(s"this._write${idToSetterStr(instName)}();")
    out.dec
    out.puts("}")
  }

  override def instanceReturnNullIfDisabled(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (!this._enabled${idToSetterStr(instName)}) {")
    out.inc
    out.puts("return null;")
    out.dec
    out.puts("}")
    out.puts
  }

  override def instanceInvalidate(instName: InstanceIdentifier): Unit = {
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._invalidate${idToSetterStr(instName)} = function() {")
    out.inc
    out.puts(s"${privateMemberName(instName)} = undefined;")
    out.dec
    out.puts("}")
  }

  //</editor-fold>

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def universalDoc(doc: DocSpec): Unit = {
    // JSDoc docstring style: https://jsdoc.app/about-getting-started
    out.puts
    out.puts( "/**")

    doc.summary.foreach(summary => out.putsLines(" * ", summary))

    // https://jsdoc.app/tags-see
    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(" * ", s"@see $text")
      case UrlRef(url, text) =>
        out.putsLines(" * ", s"@see {@link $url|$text}")
    }

    out.puts( " */")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if (this._is_le) {")
    out.inc
    leProc()
    out.dec
    out.puts("} else {")
    out.inc
    beProc()
    out.dec
    out.puts("}")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, rep: RepeatSpec): String = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        s"$kstreamName.$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$kstreamName.processZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName.processRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val nameInit = name.init
        val pkgName = if (nameInit.isEmpty) "" else nameInit.mkString("-") + "/"
        val procClass = type2class(name.last)

        importList.add(s"$pkgName$procClass")

        out.puts(s"var _process = new ${procClass}_.${procClass}(${args.map(expression).mkString(", ")});")
        s"_process.decode($srcExpr)"
    }
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val langName = idToStr(varName)
    val memberCall = privateMemberName(varName)

    val ioName = s"_io_$langName"

    val args = getRawIdExpr(varName, rep)

    out.puts(s"var $ioName = new $kstreamName($args);")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[i]"
      case _ => s"$memberName[$memberName.length - 1]"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"var io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"var _pos = $io.pos;")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  override def attrDebugStart(attrId: Identifier, attrType: DataType, io: Option[String], rep: RepeatSpec): Unit = {
    val debugName = attrDebugName(attrId, rep, false)

    val ioProps = io match {
      case None => ""
      case Some(x) => s"start: $x.pos, ioOffset: $x.byteOffset"
    }

    val enumNameProps = attrType match {
      case t: EnumType => s"""enumName: \"${types2class(t.enumSpec.get.name, false)}\""""
      case _ => ""
    }

    out.puts(s"$debugName = { $ioProps${if (ioProps != "" && enumNameProps != "") ", " else ""}$enumNameProps };")
  }

  override def attrDebugArrInit(id: Identifier, attrType: DataType): Unit =
    out.puts(s"this._debug.${idToStr(id)}.arr = [];")

  override def attrDebugEnd(attrId: Identifier, attrType: DataType, io: String, rep: RepeatSpec): Unit = {
    val debugName = attrDebugName(attrId, rep, true)

    out.puts(s"$debugName.end = $io.pos;")
  }

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  // TODO: replace this with UniversalFooter
  override def condIfFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit =
    out.puts(s"${privateMemberName(id)} = [];")

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("var i = 0;")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.push($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("i++;")
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: expr): Unit = {
    out.puts(s"for (var i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    out.puts("var i = 0;")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"var $tmpName = $expr;")
    out.puts(s"${privateMemberName(id)}.push($tmpName);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("i++;")
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"var $id = $expr;")

  override def parseExpr(dataType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        if (terminator.length == 1) {
          val term = terminator.head & 0xff
          s"$io.readBytesTerm($term, $include, $consume, $eosError)"
        } else {
          s"$io.readBytesTermMulti(${translator.doByteArrayLiteral(terminator)}, $include, $consume, $eosError)"
        }
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val (parent, root) = if (t.isExternal(typeProvider.nowClass)) {
          ("null", "null")
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          (parent, "this._root")
        }
        val addEndian = t.classSpec.get.meta.endian match {
          case Some(InheritedEndian) => ", this._is_le"
          case _ => ""
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        s"new ${types2class(t.name, t.isExternal(typeProvider.nowClass))}($io, $parent, $root$addEndian$addParams)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        if (term.length == 1) {
          val t = term.head & 0xff
          s"$kstreamName.bytesTerminate($expr1, $t, $include)"
        } else {
          s"$kstreamName.bytesTerminateMulti($expr1, ${translator.doByteArrayLiteral(term)}, $include)"
        }
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    out.puts(s"$id._read();")
  }

  override def tryFinally(tryBlock: () => Unit, finallyBlock: () => Unit): Unit = {
    out.puts("try {")
    out.inc
    tryBlock()
    out.dec
    out.puts("} finally {")
    out.inc
    finallyBlock()
    out.dec
    out.puts("}")
  }

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: BooleanType | _: EnumType | _: StrType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"switch (${expression(on)}) {")

  override def switchCaseFirstStart(condition: Ast.expr): Unit =
    switchCaseStart(condition)

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"case ${expression(condition)}:")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.puts("break;")
    out.dec
  }

  override def switchElseStart(): Unit = {
    out.puts("default:")
    out.inc
  }

  override def switchEnd(): Unit =
    out.puts("}")

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"var ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
  }

  private def switchCmpExpr(condition: Ast.expr): String =
    expression(
      Ast.expr.Compare(
        NAME_SWITCH_ON,
        Ast.cmpop.Eq,
        condition
      )
    )

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if (${switchCmpExpr(condition)}) {")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"else if (${switchCmpExpr(condition)}) {")
    out.inc
  }

  override def switchIfCaseEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def switchIfElseStart(): Unit = {
    out.puts("else {")
    out.inc
  }

  override def switchIfEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  //</editor-fold>

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"Object.defineProperty(${type2class(className.last)}.prototype, '${publicMemberName(instName)}', {")
    out.inc
    if (config.readWrite) {
      out.puts("configurable: true,")
    }
    out.puts("get: function() {")
    out.inc
  }

  override def attributeSetter(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attrName match {
      case instName: InstanceIdentifier =>
        val className = type2class(typeProvider.nowClass.name.last)

        out.puts(s"Object.defineProperty($className.prototype, '${publicMemberName(instName)}', {")
        out.inc
        out.puts("set: function(v) {")
        out.inc
        out.puts("this._dirty = true;")
        out.puts(s"${privateMemberName(instName)} = v;")
        out.dec
        out.puts("}")
        out.dec
        out.puts("});")
      case _ =>
        // seq attributes and parameters are plain properties in JavaScript
    }
  }

  override def instanceFooter: Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("});")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (${privateMemberName(instName)} !== undefined)")
    out.inc
    instanceReturn(instName, dataType, false)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(BigInt, EnumValueSpec)]): Unit = {
    out.puts(s"${type2class(curClass.last)}.${type2class(enumName)} = Object.freeze({")
    out.inc

    // Name to ID mapping
    enumColl.foreach { case (id, label) =>
      out.puts(s"${enumValue(enumName, label.name)}: ${translator.doIntLiteral(id)},")
    }
    out.puts

    // ID to name mapping
    enumColl.foreach { case (id, label) =>
      val idStr = if (id < 0) {
        "\"" + id.toString + "\""
      } else {
        id.toString
      }
      out.puts(s"""$idStr: "${enumValue(enumName, label.name)}",""")
    }

    out.dec
    out.puts("});")
    out.puts
  }

  def enumValue(enumName: String, label: String) = Utils.upperUnderscoreCase(label)

  override def debugClassSequence(seq: List[AttrSpec]) = {
    //val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    //out.puts(s"SEQ_FIELDS = [$seqStr]")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    val className = type2class(typeProvider.nowClass.name.last)

    out.puts
    out.puts(s"${className}.prototype.toString = function() {")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)};")
    out.dec
    out.puts("}")
  }

  def idToStr(id: Identifier): String = JavaScriptCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String =
    id match {
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case _ => idToStr(id)
    }

  override def privateMemberName(id: Identifier): String =
    JavaScriptCompiler.privateMemberName(id, translator.inSubIOWriteBackHandler)

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = JavaScriptCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attr: AttrLikeSpec,
    checkExpr: Ast.expr,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr] = None
  ): Unit =
    attrValidate(attr, s"!(${translator.translate(checkExpr)})", err, useIo, actual, expected)

  override def attrValidateInEnum(
    attr: AttrLikeSpec,
    et: EnumType,
    valueExpr: Ast.expr,
    err: ValidationNotInEnumError,
    useIo: Boolean
  ): Unit = {
    val enumSpec = et.enumSpec.get
    val enumRef = types2class(enumSpec.name, enumSpec.isExternal(typeProvider.nowClass))
    attrValidate(attr, s"!Object.prototype.hasOwnProperty.call($enumRef, ${translator.translate(valueExpr)})", err, useIo, valueExpr, None)
  }

  private def attrValidate(
    attr: AttrLikeSpec,
    failCondExpr: String,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr]
  ): Unit = {
    val errArgsStr = expected.map(expression) ++ List(
      expression(actual),
      if (useIo) expression(Ast.expr.InternalName(IoIdentifier)) else "null",
      expression(Ast.expr.Str(attr.path.mkString("/", "/", "")))
    )
    out.puts(s"if ($failCondExpr) {")
    out.inc
    val errObj = s"new ${ksErrorName(err)}(${errArgsStr.mkString(", ")})"
    if (attrDebugNeeded(attr.id)) {
      val debugName = attrDebugName(attr.id, attr.cond.repeat, true)
      out.puts(s"var _err = $errObj;")
      out.puts(s"$debugName.validationError = _err;")
      out.puts("throw _err;")
    } else {
      out.puts(s"throw $errObj;")
    }
    out.dec
    out.puts("}")
  }

  def attrDebugName(attrId: Identifier, rep: RepeatSpec, end: Boolean) = {
    val arrIndexExpr = rep match {
      case NoRepeat => ""
      case _: RepeatExpr => ".arr[i]"
      case RepeatEos | _: RepeatUntil => s".arr[${privateMemberName(attrId)}.length${if (end) " - 1" else ""}]"
    }

    s"this._debug.${idToStr(attrId)}$arrIndexExpr"
  }
}

object JavaScriptCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new JavaScriptCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_${Utils.lowerCamelCase(name)}"
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
      case OuterSizeIdentifier(innerId) => s"${idToStr(innerId)}__outer_size"
      case InnerSizeIdentifier(innerId) => s"${idToStr(innerId)}__inner_size"
    }

  def idToSetterStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.upperCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.upperCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToSetterStr(innerId)
    }

  def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  def privateMemberName(id: Identifier, inSubIOWriteBackHandler: Boolean): String =
    if (inSubIOWriteBackHandler) {
      s"_this.${idToStr(id)}"
    } else {
      privateMemberName(id)
    }

  override def kstreamName: String = "KaitaiStream"

  // FIXME: probably KaitaiStruct will emerge some day in JavaScript runtime, but for now it is unused
  override def kstructName: String = ???

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => s"KaitaiStream.EOFError"
    case _ => s"KaitaiStream.${err.name}"
  }

  def types2class(types: List[String], isExternal: Boolean): String = {
    // If the first segment of the name path refers to an external format module
    // (which is the only way how external types can be referenced), we must
    // prepend the name of top-level module (which ends with an underscore `_`
    // according to our own convention for clarity) before the path because of
    // https://github.com/kaitai-io/kaitai_struct/issues/1074
    val prefix = if (isExternal) {
      s"${type2class(types.head)}_."
    } else {
      ""
    }
    prefix + types.map(type2class).mkString(".")
  }
}
