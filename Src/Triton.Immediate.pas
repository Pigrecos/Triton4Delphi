unit Triton.Immediate;

{$Z4}

interface
    uses System.SysUtils,Winapi.Windows,
        Triton.Define,
        Triton.BitVector;

type
  _Imm = record
   value   : UInt64;
   Tipo    : operand_e;
   Size    : UInt32;
   BitSize : UInt32;
   BitVec  : _BV;
 end;

 Immediate = record
   private
      FHImm        : HandleImmediate;
      FInternalImm : _Imm;
      procedure GetInternalData;
   Public
      value       : UInt64;
      Tipo        : operand_e;
      Size        : UInt32;
      BitSize     : UInt32;
      //bitvector
      high        : UInt32;
      low         : UInt32;
      VectorSize  : UInt32;
      MaxValue    : UInt64;
      procedure   Create; overload;
      procedure   Create(value: uint64; size: uint32); overload;
      procedure   Create(other: Immediate ); overload;
      procedure   Free;
      procedure  SetValue(v: uint64; size: uint32);
      function   ToStr: string;

      class Operator Explicit(hImm: HandleImmediate):Immediate;
      class Operator Explicit(rImm: Immediate):HandleImmediate;
      class operator Implicit(rImm: Immediate): HandleImmediate;
      class operator Equal(imm1,imm2: Immediate ):Boolean;
      class operator NotEqual(imm1,imm2: Immediate ):Boolean;
      class operator LessThan(imm1,imm2: Immediate ):Boolean;
 end;

        function RefImmToImm(HImm: HandleImmediate): _Imm; cdecl;  external Triton_dll Name 'RefImmToImm';


  (*  Immediate =========================================================================== *)

        //! Constructor.
        function IMMCreateImmediate: HandleImmediate;cdecl;  external Triton_dll Name 'IMMCreateImmediate';
        //! Constructor.
        function IMMCreateImmediateS(value: uint64; size: uint32  ): HandleImmediate;cdecl;  external Triton_dll Name 'IMMCreateImmediateS';
        //! Constructor by copy.
        function IMMCreateImmediateFrom(other: HandleImmediate): HandleImmediate;cdecl;  external Triton_dll Name 'IMMCreateImmediateFrom';
        //! Destructor.
	      procedure  IMMDelete(Handle: HandleImmediate); cdecl;  external Triton_dll Name 'IMMDelete';
        //! Sets the value of the operand.
        procedure  IMMsetValue(vImm: HandleImmediate; v: uint64; size: uint32);cdecl;  external Triton_dll Name 'IMMsetValue';


implementation

{ Immediate }

procedure Immediate.GetInternalData;
begin
    FInternalImm := RefImmToImm(FHImm);

    value       := FInternalImm.value;
    Tipo        := FInternalImm.Tipo;
    Size        := FInternalImm.Size;
    BitSize     := FInternalImm.BitSize;
    //BitVector
    high        := FInternalImm.BitVec.high;
    low         := FInternalImm.BitVec.low;
    VectorSize  := FInternalImm.BitVec.VectorSize;
    MaxValue    := FInternalImm.BitVec.MaxValue;
end;

procedure Immediate.Create;
begin
    ZeroMemory(@self,SizeOf(Immediate));
    FHImm  := IMMCreateImmediate;

    GetInternalData;
end;

procedure Immediate.Create(value: uint64; size: uint32);
begin
    ZeroMemory(@self,SizeOf(Immediate));
    FHImm  := IMMCreateImmediateS(value,Size);

    GetInternalData;
end;

procedure Immediate.Create(other: Immediate);
begin
    ZeroMemory(@self,SizeOf(Immediate));
    FHImm  := IMMCreateImmediateFrom(HandleImmediate(other));

    GetInternalData;
end;

procedure Immediate.SetValue(v: uint64; size: uint32);
begin
    IMMsetValue(FHImm,v,size) ;

    GetInternalData
end;

function Immediate.ToStr: string;
begin
    Result := Format('0x%x:%d bv[%d..%d]',[value,BitSize,high,low])
end;

class operator Immediate.Equal(imm1, imm2: Immediate): Boolean;
begin
    Result := True;
    if imm1.Value <> imm2.Value then
      Result := False;
    if imm1.Size <> imm2.Size then
      Result := False;
end;

class operator Immediate.LessThan(imm1, imm2: Immediate): Boolean;
var
  seed1, seed2 : UInt64 ;
begin
    seed1 := 0;
    seed2 := 0;

      (*
       * Golden ratio 32-bits -> 0x9e3779b9
       * Golden ratio 64-bits -> 0x9e3779b97f4a7c13
       *)
      seed1 := seed1 xor imm1.Value + $9e3779b97f4a7c13 + (seed1 shl 6) + (seed1 shr 2);
      seed1 := seed1 xor imm1.Size  + $9e3779b97f4a7c13 + (seed1 shl 6) + (seed1 shr 2);

      seed2 := seed2 xor imm2.Value + $9e3779b97f4a7c13 + (seed2 shl 6) + (seed2 shr 2);
      seed2 := seed2 xor imm2.Size  + $9e3779b97f4a7c13 + (seed2 shl 6) + (seed2 shr 2);

      Result := seed1 < seed2;
end;

class operator Immediate.NotEqual(imm1, imm2: Immediate): Boolean;
begin
    Result := not(imm1 = imm2) ;
end;

class operator Immediate.Explicit(rImm: Immediate): HandleImmediate;
begin
    Result := rImm.FHImm;
end;

class operator Immediate.Implicit(rImm: Immediate): HandleImmediate;
begin
    Result := rImm.FHImm;
end;

class operator Immediate.Explicit(hImm: HandleImmediate): Immediate;
begin
    ZeroMemory(@Result,SizeOf(Immediate));
    Result.FHImm  := hImm;

    Result.GetInternalData ;
end;

procedure Immediate.Free;
begin
    IMMDelete(FHImm);
    Self := default(Immediate)
end;

end.
