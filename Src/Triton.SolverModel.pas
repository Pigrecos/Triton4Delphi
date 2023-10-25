unit Triton.SolverModel;

{$Z4}

interface
   uses System.SysUtils,Winapi.Windows,
        Triton.Define,
        Triton.SymbolicVariable;

type
 SolverModel = record
   private
      FHSolverModel : HandleSolverModel;
      FId           : usize;
      FValue        : UInt64;
      FVariable     : SymbolicVar;
      procedure GetInternalDati;
   public
      procedure   Create; overload;
      procedure   Create(variable: HandleSharedSymbolicVariable; value: uint64); overload;
      procedure   Create(other: SolverModel ); overload;
      procedure   Free;
      function    ToStr: string;

      class Operator Explicit(hSolverM: HandleSolverModel):SolverModel;
      class Operator Explicit(rSolverM: SolverModel):HandleSolverModel;

      property Id       : usize        read FId;
      property Value    : UInt64       read FValue;
      property Variable : SymbolicVar  read FVariable;

 end;

  (*  Solver Model =========================================================================== *)

        //! Constructor.
        function SMCreate:HandleSolverModel; cdecl;  external Triton_dll Name 'SMCreate';
        //! Constructor.// todo adding support uint512
        function SMCreateD(variable: HandleSharedSymbolicVariable; value: uint64): HandleSolverModel;cdecl;  external Triton_dll Name 'SMCreateD';
        //! Constructor by copy.
        function SMCreateFrom(other: HandleSolverModel): HandleSolverModel;cdecl;  external Triton_dll Name 'SMCreateFrom';
        //! Destructor.
	      procedure SMDelete(Handle: HandleSolverModel);cdecl;  external Triton_dll Name 'SMDelete';
        //! Returns the id of the variable.
        function SMgetId(Handle: HandleSolverModel): usize;cdecl;  external Triton_dll Name 'SMgetId';
        //! Returns the value of the model.// todo adding support uint512
        function SMgetValue(Handle: HandleSolverModel): uint64 ; cdecl;  external Triton_dll Name 'SMgetValue';
        //! Returns the symbolic variable.
        function SMgetVariable(Handle: HandleSolverModel): HandleSharedSymbolicVariable ; cdecl;  external Triton_dll Name 'SMgetVariable';

implementation

{ SolverModel }

procedure SolverModel.Create;
begin
     ZeroMemory(@self,SizeOf(SolverModel));
     FHSolverModel := SMCreate();

     GetInternalDati
end;

procedure SolverModel.Create(variable: HandleSharedSymbolicVariable; value: uint64);
begin
     ZeroMemory(@self,SizeOf(SolverModel));
     FHSolverModel := SMCreateD(variable,value);

     GetInternalDati;
end;

procedure SolverModel.Create(other: SolverModel);
begin
     ZeroMemory(@self,SizeOf(SolverModel));
     FHSolverModel := SMCreateFrom( HandleSolverModel(other) );

     GetInternalDati;
end;

procedure SolverModel.Free;
begin
    SMDelete(FHSolverModel);
end;

procedure SolverModel.GetInternalDati;
begin
    FId       := SMgetId(FHSolverModel);
    FValue    := SMgetValue(FHSolverModel);
    FVariable := SymbolicVar (SMgetVariable(FHSolverModel) );
end;

class operator SolverModel.Explicit(hSolverM: HandleSolverModel): SolverModel;
begin
    ZeroMemory(@Result,SizeOf(SolverModel));
    Result.FHSolverModel  := hSolverM;

    Result.GetInternalDati;
end;

class operator SolverModel.Explicit(rSolverM: SolverModel): HandleSolverModel;
begin
    Result := rSolverM.FHSolverModel;
end;

function SolverModel.ToStr: string;
begin
    Result := Format('%s =0x%x',[FVariable.ToStr,FValue] )
end;

end.
