unit Triton.MemoryAccess;

{$Z4}

interface
   uses System.SysUtils,Winapi.Windows,
        Triton.Define,
        Triton.BitVector,
        Triton.Register,
        Triton.Immediate,
        Triton.AstNode;

type
  _memAccess = record
   address      : UInt64;
   pcRelative   : UInt64;
   segmentReg   : HandleReg;
   baseReg      : HandleReg;
   indexReg     : HandleReg;
   displacement : HandleImmediate;
   scale        : HandleImmediate;
   leaAST       : HandleAbstractNode;
   Tipo         : operand_e;
   Size         : UInt32;
   BitSize      : UInt32;
   BitVec       : _BV;
 end;

 MemAccess = record
   private
     FHMem        : HandleMemAcc;
     FInternalMem : _memAccess;
     procedure GetInternalData;
   public
     address      : UInt64;
     pcRelative   : Uint64;
     segmentReg   : Registro;
     baseReg      : Registro;
     indexReg     : Registro;
     displacement : Immediate;
     scale        : Immediate;
     leaAST       : AbstractNode;
     Tipo         : operand_e;
     Size         : UInt32;
     BitSize      : UInt32;
     //bitvector
     high        : UInt32;
     low         : UInt32;
     VectorSize  : UInt32;
     MaxValue    : UInt64;
     procedure   Create;overload;
     procedure   Create(address:uint64; size: uint32); overload;
     procedure   Create(other: MemAccess); overload;
     procedure   Free;
     procedure   setAddress(addr: UInt64);
     procedure   setPcRelative(addr: UInt64);
     procedure   setSegmentRegister(segment: Registro);
     procedure   setBaseRegister(base: Registro);
     procedure   setIndexRegister(index: Registro);
     procedure   setDisplacement(displacement: Immediate) ;
     procedure   setScale(scale: Immediate) ;
     procedure   setLeaAst(ast: AbstractNode);
     function    ToStr: string;

     class Operator Explicit(hMem: HandleMemAcc):MemAccess;
     class Operator Explicit(rMem: MemAccess):HandleMemAcc;
     class Operator Implicit(rMem: MemAccess):HandleMemAcc;

     class operator Equal(Mem1,Mem2: MemAccess ):Boolean;
     class operator NotEqual(Mem1,Mem2: MemAccess ):Boolean;
     class operator LessThan(Mem1,Mem2: MemAccess ):Boolean;
 end;
        

        function RefMemToMem(HMem: HandleMemAcc): _memAccess; cdecl;  external Triton_dll Name 'RefMemToMem';


     (*  Memoria  =========================================================================== *)

        //! Constructor.
        function MEMCreateMemory: HandleMemAcc;cdecl;  external Triton_dll Name 'MEMCreateMemory';
        //! Constructor.
        function MEMCreateMemoryS(address: uint64; size: uint32): HandleMemAcc;cdecl;  external Triton_dll Name 'MEMCreateMemoryS';
        //! Constructor.
	      function MEMCreateMemoryFrom(other: HandleMemAcc): HandleMemAcc;cdecl;  external Triton_dll Name 'MEMCreateMemoryFrom';
        //! Destructor.
	      procedure MEMDelete(Handle: HandleMemAcc); cdecl;  external Triton_dll Name 'MEMDelete';
        //! Sets the address of the memory access.
        procedure MEMsetAddress(hMem: HandleMemAcc; addr: UInt64);cdecl;  external Triton_dll Name 'MEMsetAddress';
        //! LEA - Sets pc relative.
        procedure MEMsetPcRelative(hMem: HandleMemAcc; addr: UInt64); cdecl;  external Triton_dll Name 'MEMsetPcRelative';
        //! LEA - Sets the segment register operand.
        procedure MEMsetSegmentRegister(hMem: HandleMemAcc; segment: HandleReg); cdecl;  external Triton_dll Name 'MEMsetSegmentRegister';
        //! LEA - Sets the base register operand.
        procedure MEMsetBaseRegister(hMem: HandleMemAcc; base: HandleReg); cdecl;  external Triton_dll Name 'MEMsetBaseRegister';
        //! LEA - Sets the index register operand.
	      procedure MEMsetIndexRegister(hMem: HandleMemAcc; index: HandleReg); cdecl;  external Triton_dll Name 'MEMsetIndexRegister';
        //! LEA - Sets the displacement operand.
        procedure MEMsetDisplacement(hMem: HandleMemAcc; displacement: HandleImmediate);cdecl;  external Triton_dll Name 'MEMsetDisplacement';
        //! LEA - Sets the scale operand.
        procedure MEMsetScale(hMem: HandleMemAcc; scale: HandleImmediate); cdecl;  external Triton_dll Name 'MEMsetScale';
        //! Sets the AST of the memory access (LEA).
        procedure MEMsetLeaAst(hMem: HandleMemAcc; ast: HandleAbstractNode);cdecl;  external Triton_dll Name 'MEMsetLeaAst';


implementation

{ MemAccess }

procedure MemAccess.GetInternalData;
begin
    FInternalMem := RefMemToMem(FHMem);

    address      := FInternalMem.address;
    pcRelative   := FInternalMem.pcRelative;
    segmentReg   := Registro(FInternalMem.segmentReg);
    baseReg      := Registro(FInternalMem.baseReg);
    indexReg     := Registro(FInternalMem.indexReg);
    displacement := Immediate(FInternalMem.displacement);
    scale        := Immediate(FInternalMem.scale);
    leaAST       := FInternalMem.leaAST;
    Tipo         := FInternalMem.Tipo;
    Size         := FInternalMem.Size;
    BitSize      := FInternalMem.BitSize;
    //BitVector
    high       := FInternalMem.BitVec.high;
    low        := FInternalMem.BitVec.low;
    VectorSize := FInternalMem.BitVec.VectorSize;
    MaxValue   := FInternalMem.BitVec.MaxValue;
end;

procedure MemAccess.Create;
begin
    ZeroMemory(@Self,SizeOf(MemAccess));
    FHMem := MEMCreateMemory;

    GetInternalData
end;

procedure MemAccess.Create(address: uint64; size: uint32);
begin
    ZeroMemory(@Self,SizeOf(MemAccess));
    FHMem := MEMCreateMemoryS(address,size);

    GetInternalData
end;

procedure MemAccess.Create(other: MemAccess);
begin
    ZeroMemory(@Self,SizeOf(MemAccess));
    FHMem := MEMCreateMemoryFrom(HandleMemAcc(other));

    GetInternalData
end;

class operator MemAccess.Equal(Mem1, Mem2: MemAccess): Boolean;
begin
    if mem1.address <> mem2.address then Exit(False);

    if mem1.Size <> mem2.Size  then Exit(False);

    if mem1.baseReg <> mem2.baseReg then Exit(False);

    if mem1.indexReg <> mem2.indexReg  then Exit(False);

    if mem1.scale <> mem2.scale then Exit(False);

    if mem1.displacement <>  mem2.displacement  then Exit(False);

    if mem1.segmentReg <> mem2.segmentReg then Exit(False);

    if mem1.pcRelative <> mem2.pcRelative  then Exit(False);

    Result := True;
end;

class operator MemAccess.Explicit(hMem: HandleMemAcc): MemAccess;
begin
    ZeroMemory(@Result,SizeOf(MemAccess));
    Result.FHMem  := hMem ;

    Result.GetInternalData ;
end;

class operator MemAccess.Explicit(rMem: MemAccess): HandleMemAcc;
begin
    Result := rMem.FHMem;
end;

class operator MemAccess.Implicit(rMem: MemAccess): HandleMemAcc;
begin
    Result := rMem.FHMem;
end;

procedure MemAccess.Free;
begin
   MEMDelete(FHMem);
   Self := default(MemAccess);
end;

class operator MemAccess.LessThan(Mem1, Mem2: MemAccess): Boolean;
var
  seed1, seed2 : UInt64 ;
begin
    seed1 := 0;
    seed2 := 0;

      (*
       * Golden ratio 32-bits -> 0x9e3779b9
       * Golden ratio 64-bits -> 0x9e3779b97f4a7c13
       *)
      seed1 := seed1 xor Mem1.address + $9e3779b97f4a7c13 + (seed1 shl 6) + (seed1 shr 2);
      seed1 := seed1 xor Mem1.Size    + $9e3779b97f4a7c13 + (seed1 shl 6) + (seed1 shr 2);

      seed2 := seed2 xor Mem2.address + $9e3779b97f4a7c13 + (seed2 shl 6) + (seed2 shr 2);
      seed2 := seed2 xor Mem2  .Size  + $9e3779b97f4a7c13 + (seed2 shl 6) + (seed2 shr 2);

      Result := seed1 < seed2;
end;

class operator MemAccess.NotEqual(Mem1, Mem2: MemAccess): Boolean;
begin
    Result := not (Mem1 = Mem2)
end;

procedure MemAccess.setAddress(addr: UInt64);
begin
   MEMsetAddress(FHMem, addr);

   GetInternalData
end;

procedure MemAccess.setPcRelative(addr: UInt64);
begin
   MEMsetPcRelative(FHMem, addr);

   GetInternalData
end;

procedure MemAccess.setSegmentRegister(segment: Registro);
begin
   MEMsetSegmentRegister(FHMem, HandleReg(segment)) ;

   GetInternalData
end;

procedure MemAccess.setBaseRegister(base: Registro);
begin
   MEMsetBaseRegister(FHMem, HandleReg(base)) ;

   GetInternalData
end;

procedure MemAccess.setIndexRegister(index: Registro);
begin
   MEMsetIndexRegister(FHMem, HandleReg(index)) ;

   GetInternalData
end;

procedure MemAccess.setDisplacement(displacement: Immediate);
begin
   MEMsetDisplacement(FHMem, HandleImmediate(displacement));

   GetInternalData
end;

procedure MemAccess.setScale(scale: Immediate);
begin
   MEMsetScale(FHMem, HandleImmediate(scale)) ;

   GetInternalData
end;

procedure MemAccess.setLeaAst(ast: AbstractNode);
begin
    MEMsetLeaAst(FHMem, ast);

    GetInternalData
end;

function MemAccess.ToStr: string;
begin
    Result := '[@0x'+ IntToHex(address,8) +']:'+ IntToStr(BitSize) ;
    Result := Result + ' bv[' + IntToStr(high)+ '..' + IntToStr(low)+ ']';
end;

end.
