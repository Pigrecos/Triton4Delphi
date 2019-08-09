unit Triton.OperandWrapper;

{$Z4}

interface
   uses System.SysUtils,Winapi.Windows,
        Triton.Define,
        Triton.BitVector,
        Triton.Register,
        Triton.Immediate,
        Triton.MemoryAccess;

type

 _OpWrapp = record
   imm     : HandleImmediate;
   mem     : HandleMemAcc;
   reg     : HandleReg;
   Tipo    : operand_e;
   Size    : UInt32;
   BitSize : UInt32;
   high    : UInt32;
   low     : UInt32;
 end;

 OpWrapper = record
   private
     FHOp         : HandleOperandWrapper;
     FInternalOpW : _OpWrapp;
     procedure GetInternalData;
   public
     imm     : Immediate;
     mem     : MemAccess;
     reg     : Registro;
     Tipo    : operand_e;
     Size    : UInt32;
     BitSize : UInt32;
     high    : UInt32;
     low     : UInt32;
     procedure   Create(imm :Immediate );overload;
     procedure   Create(mem: MemAccess); overload;
     procedure   Create(reg: Registro); overload;
     procedure   CreateFrom(other: OpWrapper); overload;
     procedure   Free;
     procedure   SetImmediate(imm:Immediate);
     procedure   SetMemory(mem: MemAccess);
     procedure   setRegister(reg: Registro);
     function    ToStr: string;

     class Operator Explicit(hOp: HandleOperandWrapper):OpWrapper;
     class Operator Explicit(rOp: OpWrapper):HandleOperandWrapper;
     class Operator Implicit(rOp: OpWrapper):HandleOperandWrapper;

     class operator Equal(Op1,Op2: OpWrapper ):Boolean;
     class operator NotEqual(Op1,Op2: OpWrapper ):Boolean;
     class operator LessThan(Op1,Op2: OpWrapper ):Boolean;
 end;

        function RefOpWrapToOpWrap(HOpW: HandleOperandWrapper): _OpWrapp; cdecl;  external Triton_dll Name 'RefOpWrapToOpWrap';


 (*  Operand Wrapper  =========================================================================== *)

        //! Immediate constructor.
        function OPOperandWrapperI(imm: HandleImmediate): HandleOperandWrapper;cdecl;  external Triton_dll Name 'OPOperandWrapperI';
        //! Memory constructor.
        function OPOperandWrapperM(mem : HandleMemAcc):HandleOperandWrapper;cdecl;  external Triton_dll Name 'OPOperandWrapperM';
        //! Register constructor.
        function OPOperandWrapperR(reg: HandleReg):HandleOperandWrapper; cdecl;  external Triton_dll Name 'OPOperandWrapperR';
        //! Constructor by copy.
        function OPOperandWrapperFrom(other:HandleOperandWrapper):HandleOperandWrapper;cdecl;  external Triton_dll Name 'OPOperandWrapperFrom';
        //! Destructor.
	      procedure  OPDelete(Handle: HandleOperandWrapper); cdecl;  external Triton_dll Name 'OPDelete';
        //! Sets the immediate operand.
        procedure OPsetImmediate(hOp: HandleOperandWrapper; imm: HandleImmediate);cdecl;  external Triton_dll Name 'OPsetImmediate';
        //! Sets the memory operand.
        procedure OPsetMemory(hOp: HandleOperandWrapper; mem: HandleMemAcc); cdecl;  external Triton_dll Name 'OPsetMemory';
        //! Sets the register operand.
        procedure OPsetRegister(hOp: HandleOperandWrapper; reg: HandleReg);  cdecl;  external Triton_dll Name 'OPsetRegister';


implementation

{ OpWrapper }

procedure OpWrapper.GetInternalData;
begin
    FInternalOpW := RefOpWrapToOpWrap(FHOp);

    imm     := Immediate(FInternalOpW.imm);
    mem     := MemAccess(FInternalOpW.mem);
    reg     := Registro(FInternalOpW.reg);
    Tipo    := FInternalOpW.tipo;
    Size    := FInternalOpW.Size;
    BitSize := FInternalOpW.BitSize;
    high    := FInternalOpW.high;
    low     := FInternalOpW.low;
end;


procedure OpWrapper.Create(imm: Immediate);
begin
    ZeroMemory(@self,SizeOf(OpWrapper));
    FHOp := OPOperandWrapperI(handleImmediate(imm));

    GetInternalData
end;

procedure OpWrapper.Create(mem: MemAccess);
begin
    ZeroMemory(@self,SizeOf(OpWrapper));
    FHOp := OPOperandWrapperM(HandleMemAcc(mem));

    GetInternalData
end;

procedure OpWrapper.Create(reg: Registro);
begin
    ZeroMemory(@self,SizeOf(OpWrapper));
    FHOp := OPOperandWrapperR(HandleReg(reg));

    GetInternalData
end;


procedure OpWrapper.CreateFrom(other: OpWrapper);
begin
    ZeroMemory(@self,SizeOf(OpWrapper));
    FHOp := OPOperandWrapperFrom( HandleOperandWrapper(other));

    GetInternalData
end;

class operator OpWrapper.Equal(Op1, Op2: OpWrapper): Boolean;
begin
    if Op1.Tipo <>  Op2.Tipo then Exit(False);

    case Op1.Tipo of
        OP_IMM: Result := Op1.imm = Op2.imm;
        OP_MEM: Result := Op1.mem = Op2.mem;
        OP_REG: Result := Op1.reg = Op2.reg;
        else
          raise Exception.Create('OperandWrapper::operator=: Invalid type operand.');
      end;
end;

class operator OpWrapper.NotEqual(Op1, Op2: OpWrapper): Boolean;
begin
    Result := not (Op1  = Op2);
end;

class operator OpWrapper.LessThan(Op1, Op2: OpWrapper): Boolean;
begin
    if Op1.Tipo <  Op2.Tipo then Exit(True);

    if Op1.Tipo >  Op2.Tipo then Exit(False);

    case Op1.Tipo of
        OP_IMM: Result := Op1.imm < Op2.imm;
        OP_MEM: Result := Op1.mem < Op2.mem;
        OP_REG: Result := Op1.reg < Op2.reg;
        else
          raise Exception.Create('OperandWrapper::operator<: Invalid type operand.');
      end;
end;

class operator OpWrapper.Explicit(hOp: HandleOperandWrapper): OpWrapper;
begin
    ZeroMemory(@Result,SizeOf(OpWrapper));
    Result.FHOp  := hOp ;

    Result.GetInternalData
end;

class operator OpWrapper.Explicit(rOp: OpWrapper): HandleOperandWrapper;
begin
    Result := rOp.FHOp;
end;

class operator OpWrapper.Implicit(rOp: OpWrapper): HandleOperandWrapper;
begin
    Result := rOp.FHOp;
end;

procedure OpWrapper.Free;
begin
   OPDelete(FHOp);
   Self := default(OpWrapper)
end;

procedure OpWrapper.SetImmediate(imm: Immediate);
begin
    OPsetImmediate(FHOp,HandleImmediate(imm)) ;

    GetInternalData
end;

procedure OpWrapper.SetMemory(mem: MemAccess);
begin
    OPsetMemory(FHOp,HandleMemAcc(mem));

    GetInternalData
end;

procedure OpWrapper.setRegister(reg: Registro);
begin
    OPsetRegister(FHOp,HandleReg(reg))  ;

    GetInternalData
end;

function OpWrapper.ToStr: string;
begin
    case Tipo of
        OP_IMM: Result := imm.ToStr;
        OP_MEM: Result := mem .ToStr;
        OP_REG: Result := reg.ToStr;
        else
          raise Exception.Create('OperandWrapper::operator tostr(): Invalid type operand.');
      end;
end;

end.
