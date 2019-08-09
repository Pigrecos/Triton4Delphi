unit Triton.BitVector;

{$Z4}

interface
    uses System.SysUtils,Winapi.Windows,
        Triton.Define;

type
_BV = record
   high        : UInt32;
   low         : UInt32;
   VectorSize  : UInt32;
   MaxValue    : UInt64;
 end;

 BitVector = record
   private
     FHBV        : HandleBV;
     FInternalBV : _BV;
   public
    high        : UInt32;
    low         : UInt32;
    VectorSize  : UInt32;
    { TODO -oMax -c : Adding support uint512 23/07/2019 18:06:23 }
    MaxValue    : UInt64;
    procedure   Create;
    procedure   Free;
    function ToStr: string;
    procedure SetHigh(v: uint32);
    procedure SetLow(v: uint32);
 end;

        function RefBVToBV(HBv:HandleBV): _BV; cdecl;  external Triton_dll Name 'RefBVToBV';


   (*  BitsVector ========================================================================== *)

	      //! Constructor.
        function BVCreateBitsVector:HandleBV;cdecl;  external Triton_dll Name 'BVCreateBitsVector';
        //! Constructor.
        function BVCreateBitsVectorHL(high: uint32; low: uint32): HandleBV;cdecl;  external Triton_dll Name 'BVCreateBitsVectorHL';
        //! Constructor by copy.
        function BVCreateBitsVectorFrom(other: HandleBV): HandleBV;cdecl;  external Triton_dll Name 'BVCreateBitsVectorFrom';
        //! Destructor.
	      procedure  BVDelete(Handle: HandleBV);cdecl;  external Triton_dll Name 'BVDelete';
        //! Sets the highest bit position
        procedure  BVsetHigh(bv: HandleBV; v: uint32);cdecl;  external Triton_dll Name 'BVsetHigh';
        //! Sets the lower bit position
        procedure  BVsetLow(bv: HandleBV; v: uint32);cdecl;  external Triton_dll Name 'BVsetLow';

implementation

{ BitVector }

procedure BitVector.Create;
begin
    FHBV        := BVCreateBitsVector();
    FInternalBV := RefBVToBV(FHBV);

    high        := FInternalBV.high;
    low         := FInternalBV.low;
    VectorSize  := FInternalBV.VectorSize;
    MaxValue    := FInternalBV.MaxValue;
end;

procedure BitVector.Free;
begin
    BVDelete(FHBV);
    Self := default(BitVector)
end;

procedure BitVector.SetHigh(v: uint32);
begin
    BVsetHigh(FHBV,v);

    FInternalBV := RefBVToBV(FHBV);

    high        := FInternalBV.high;
    low         := FInternalBV.low;
    VectorSize  := FInternalBV.VectorSize;
    MaxValue    := FInternalBV.MaxValue;
end;

procedure BitVector.SetLow(v: uint32);
begin
    BVsetLow(FHBV, v);

    FInternalBV := RefBVToBV(FHBV);

    high        := FInternalBV.high;
    low         := FInternalBV.low;
    VectorSize  := FInternalBV.VectorSize;
    MaxValue    := FInternalBV.MaxValue;
end;

function BitVector.ToStr: string;
begin
    Result := 'bv[' + IntToStr(high)+ '..' + IntToStr(low)+ ']';
end;

end.
