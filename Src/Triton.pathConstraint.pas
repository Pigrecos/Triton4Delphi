unit Triton.pathConstraint;

{$Z4}
{$POINTERMATH ON}

interface
    uses System.SysUtils,Winapi.Windows,
        Triton.Define,
        Triton.AstNode;

type

pathConstraint = record
   private
     FHPath      : HandlePathConstraint;
     (*!
     * \brief The branches constraints
     * \details Vector of `<flag, source addr, dst addr, pc>`, `flag` is set to true if the branch is taken according the pc.
     * The source address is the location of the branch instruction and the destination address is the destination of the jump.
     * E.g: `"0x11223344: jne 0x55667788"`, 0x11223344 is the source address and 0x55667788 is the destination if and only if the
     * branch is taken, otherwise the destination is the next instruction address.
     *)
     FBranches              : ABranch;
     FTakenAddress          : UInt64;
     FTakenPathConstraintAst: AbstractNode;
     FisMultipleBranches    : Boolean;
     procedure GetInternalDat;
     function  getBranchConstraints:ABranch;
     function  getTakenAddress: uint64;
     function  getTakenPathConstraintAst: AbstractNode ;
     function  MultipleBranches: Boolean;
   public
      procedure   Create; overload;
      procedure   Create(other: pathConstraint ); overload;
      procedure   Free;
      procedure   addBranchConstraint(taken: Boolean;srcdAddr,dstAddr: uint64; pc: AbstractNode);

      class Operator Explicit(hPath: HandlePathConstraint):pathConstraint;
      class Operator Explicit(rPath: pathConstraint):HandlePathConstraint;

      property BranchConstraints     : ABranch         read FBranches;
      property TakenAddress          : UInt64          read FTakenAddress ;
      property TakenPathConstraintAst: AbstractNode    read FTakenPathConstraintAst ;
      property isMultipleBranches    : Boolean         read FisMultipleBranches;
end;


  (*  PathConstraint =========================================================================== *)

          //! Constructor.
          function  PCPathConstraint:HandlePathConstraint;cdecl;  external Triton_dll Name 'PCPathConstraint';

          //! Constructor by copy.
          function PCPathConstraintFrom(other: HandlePathConstraint): HandlePathConstraint; cdecl;  external Triton_dll Name 'PCPathConstraintFrom';

          //! Destructor.
          procedure PCDelete(Handle: HandlePathConstraint);cdecl;  external Triton_dll Name 'PCDelete';

          //! Adds a branch to the path constraint.
          procedure PCaddBranchConstraint(Handle: HandlePathConstraint;taken: Boolean;srcdAddr,dstAddr: uint64; pc: HandleAbstractNode);cdecl;  external Triton_dll Name 'PCaddBranchConstraint';

          //! Returns the branch constraints.
          function PCgetBranchConstraints(Handle: HandlePathConstraint; Branches: PBranch): uint32 ;cdecl;  external Triton_dll Name 'PCgetBranchConstraints';

          //! Returns the address of the taken branch.
          function PCgetTakenAddress(Handle: HandlePathConstraint): uint64 ; cdecl;  external Triton_dll Name 'PCgetTakenAddress';

          //! Returns the path constraint AST of the taken branch.
          function PCgetTakenPathConstraintAst(Handle: HandlePathConstraint): HandleAbstractNode ; cdecl;  external Triton_dll Name 'PCgetTakenPathConstraintAst';

          //! Returns true if it is not a direct jump.
          function PCisMultipleBranches(Handle: HandlePathConstraint): Boolean; cdecl;  external Triton_dll Name 'PCisMultipleBranches';

implementation

{ pathConstraint }

procedure pathConstraint.Create;
begin
    ZeroMemory(@self,SizeOf(pathConstraint));
    FHPath := PCPathConstraint;

    GetInternalDat;
end;

procedure pathConstraint.Create(other: pathConstraint);
begin
    ZeroMemory(@self,SizeOf(pathConstraint));
    FHPath := PCPathConstraintFrom( HandlePathConstraint (other ));

    GetInternalDat;
end;

procedure pathConstraint.Free;
begin
    PCDelete(FHPath);
end;

class operator pathConstraint.Explicit(  hPath: HandlePathConstraint): pathConstraint;
begin
    ZeroMemory(@Result,SizeOf(pathConstraint));
    Result.FHPath := hPath;

    Result.GetInternalDat;
end;

class operator pathConstraint.Explicit(  rPath: pathConstraint): HandlePathConstraint;
begin
    Result := rPath.FHPath;
end;

procedure pathConstraint.addBranchConstraint(taken: Boolean; srcdAddr,   dstAddr: uint64; pc: AbstractNode);
begin
   PCaddBranchConstraint(FHPath,taken,srcdAddr,dstAddr,pc);
   GetInternalDat
end;

function pathConstraint.getBranchConstraints: ABranch;
var
  n,i : Integer;
  Brs : PBranch;
  rBr : Branch;
begin
   Brs := nil;
   n := PCgetBranchConstraints(FHPath,@Brs);
   for i := 0 to n - 1 do
   begin
       rBr.taken   := Brs[i].taken;
       rBr.srcAddr := Brs[i].srcAddr;
       rBr.dstAddr := Brs[i].dstAddr;
       rBr.pc      := Brs[i].pc;

       Result := Result + [ rBr ];
   end;

end;

procedure pathConstraint.GetInternalDat;
begin
    FBranches              := getBranchConstraints;
    FTakenAddress          := getTakenAddress ;
    FTakenPathConstraintAst:= getTakenPathConstraintAst ;
    FisMultipleBranches    := MultipleBranches;
end;

function pathConstraint.getTakenAddress: uint64;
begin
    Result := PCgetTakenAddress(FHPath);
end;

function pathConstraint.getTakenPathConstraintAst: AbstractNode;
begin
    Result := PCgetTakenPathConstraintAst(FHPath);
end;

function pathConstraint.MultipleBranches: Boolean;
begin
    Result := PCisMultipleBranches(FHPath)
end;

end.
