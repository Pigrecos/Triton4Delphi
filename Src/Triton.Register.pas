unit Triton.Register;

{$Z4}

interface
   uses System.SysUtils,Winapi.Windows,
        Triton.Define,
        Triton.BitVector;

type
  _Reg = record
    name        : PAnsiChar;
    id          : register_e;
    parent      : register_e;
    Tipo        : operand_e;
    Size        : UInt32;
    BitSize     : UInt32;
    BitVec      : _BV;
 end;

 Registro = record
   private
     FHReg        : HandleReg;
     FInternalReg : _Reg;
     procedure GetInternalData;
   public
     name        : AnsiString;
     id          : register_e;
     parent      : register_e;
     Tipo        : operand_e;
     Size        : UInt32;
     BitSize     : UInt32;
     //bitvector
     high        : UInt32;
     low         : UInt32;
     VectorSize  : UInt32;
     MaxValue    : UInt64;
     procedure   Create;overload;
     procedure   Create(regId : register_e ; name: PAnsiChar; parent: register_e; high, low: uint32; vmutable: Boolean); overload;
     procedure   Create(cpu : HandleCpuInterface; regId: register_e); overload;
     procedure   Create(other: Registro); overload;
     function    isOverlapWith(other: Registro): Boolean ;
     procedure   Free;
     function    ToStr: string;

     class Operator Explicit(hReg: HandleReg):Registro;
     class Operator Explicit(rReg: Registro):HandleReg;
     class Operator Implicit(rReg: Registro):HandleReg;
     class operator Equal(Reg1,Reg2: Registro ):Boolean;
     class operator NotEqual(Reg1,Reg2: Registro ):Boolean;
     class operator LessThan(Reg1,Reg2: Registro ):Boolean;
 end;
        

        function RefRegToReg(HReg: HandleReg): _Reg; cdecl;  external Triton_dll Name 'RefRegToReg';


  (*  Register  =========================================================================== *)

        //! Constructor.
        function REGCreateRegister: HandleReg;cdecl;  external Triton_dll Name 'REGCreateRegister';
        //! Constructor.
        function REGCreateRegisterS(regId : register_e ; name: PAnsiChar; parent: register_e; high, low: uint32; vmutable: Boolean): HandleReg;cdecl;  external Triton_dll Name 'REGCreateRegisterS';
        //! Constructor.
	      function RegCreateRegisterC(cpu : HandleCpuInterface; regId: register_e): HandleReg;cdecl;  external Triton_dll Name 'RegCreateRegisterC';
        //! Constructor by copy.
        function REGCreateRegisterFrom(other: HandleReg): HandleReg;cdecl;  external Triton_dll Name 'REGCreateRegisterFrom';

        //! Destructor.
        procedure REGDelete(Handle: HandleReg);cdecl;  external Triton_dll Name 'REGDelete';
        //! Returns true if `other` and `self` overlap.
        function REGisOverlapWith(r1, other: HandleReg): Boolean ;cdecl;  external Triton_dll Name 'REGisOverlapWith';

implementation

{ Registro }

procedure Registro.GetInternalData;
begin
    FInternalReg := RefRegToReg(FHReg);

    name        := AnsiString(FInternalReg.name);
    id          := FInternalReg.id;
    parent      := FInternalReg.parent;
    Tipo        := FInternalReg.Tipo;
    Size        := FInternalReg.Size;
    BitSize     := FInternalReg.BitSize;
    //BitVector
    high       := FInternalReg.BitVec.high;
    low        := FInternalReg.BitVec.low;
    VectorSize := FInternalReg.BitVec.VectorSize;
    MaxValue   := FInternalReg.BitVec.MaxValue;
end;

procedure Registro.Create;
begin
    ZeroMemory(@Self,SizeOf(Registro));
    FHReg := REGCreateRegister;

    GetInternalData
end;

procedure Registro.Create(regId: register_e; name: PAnsiChar;  parent: register_e; high, low: uint32; vmutable: Boolean);
begin
    ZeroMemory(@Self,SizeOf(Registro));
    FHReg := REGCreateRegisterS(regId,name,parent,high,low,vmutable);

    GetInternalData
end;

procedure Registro.Create(cpu: HandleCpuInterface; regId: register_e);
begin
    ZeroMemory(@Self,SizeOf(Registro));
    FHReg := REGCreateRegisterC(cpu,regId);

    GetInternalData
end;

procedure Registro.Create(other: Registro);
begin
    ZeroMemory(@Self,SizeOf(Registro));
    FHReg := REGCreateRegisterFrom(HandleReg( other));

    GetInternalData
end;

class operator Registro.Equal(Reg1, Reg2: Registro): Boolean;
begin
    Result := Reg1.id = Reg2.id;
end;

class operator Registro.NotEqual(Reg1, Reg2: Registro): Boolean;
begin
    Result := not(Reg1 = Reg2)
end;

class operator Registro.LessThan(Reg1, Reg2: Registro): Boolean;
begin
    Result := Reg1.id < Reg2.id
end;

class operator Registro.Explicit(hReg: HandleReg): Registro;
begin
    ZeroMemory(@Result,SizeOf(Registro));
    Result.FHReg  := hReg;

    Result.GetInternalData ;
end;

class operator Registro.Explicit(rReg: Registro): HandleReg;
begin
    Result := rReg.FHReg;
end;

class operator Registro.Implicit(rReg: Registro): HandleReg;
begin
    Result := rReg.FHReg;
end;

function Registro.isOverlapWith(other: Registro): Boolean;
begin
    Result := REGisOverlapWith(FHReg,other);
end;

procedure Registro.Free;
begin
   REGDelete(FHReg);
   Self := default(Registro);
end;

function Registro.ToStr: string;
begin
     Result := string(name) +':' + IntToStr(BitSize);
    Result := Result + ' bv[' + IntToStr(high)+ '..' + IntToStr(low)+ ']';
end;

end.
