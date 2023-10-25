unit Triton.BasicBlock;
{$Z4}
{$POINTERMATH ON}

interface
    uses
       System.SysUtils, Winapi.Windows, System.Generics.Collections,
       Triton.Define,
       Triton.Instruction;

      type

      TBasicBlock = record
        private
         FBasicBlock : HandleBasicBlock;
         function GetFirstAddress: UInt64;
         function GetLastAddress: UInt64;
         function GetSize: UInt32;
        public
         procedure Create; overload;
         procedure Create(instructions: TArray<Istruzione>); overload;
         procedure Create(other: TBasicBlock); overload;
         procedure Free;
         procedure Add(instruction : Istruzione);
         function Remove(position: UInt32): Boolean;
         function GetInstructions: TArray<Istruzione>;

         function ToStr : string;

         class Operator Explicit(hBB: HandleBasicBlock):TBasicBlock;
         class Operator Explicit(rBB: TBasicBlock):HandleBasicBlock;
         class Operator Implicit(rBB: TBasicBlock):HandleBasicBlock;

         property  Size         : UInt32 read GetSize;
         property  FirstAddress : UInt64 read GetFirstAddress;
         property  LastAddress  : UInt64 read GetLastAddress;
      end;

      //*  BasicBlock ============================================================================== */

      //! Constructor.
      function BB_BasicBlock: HandleBasicBlock; cdecl;  external Triton_dll Name 'BB_BasicBlock';

      //! Constructor.
      function BB_BasicBlockFromArray(instructions : PHandleInstruz; num_instructions: UInt32): HandleBasicBlock; cdecl;  external Triton_dll Name 'BB_BasicBlockFromArray';

      //! Constructor by copy.
      function BB_BasicBlockFrom(other: HandleBasicBlock): HandleBasicBlock; cdecl;  external Triton_dll Name 'BB_BasicBlockFrom';

      //! Destructor.
      procedure DeleteBasicBlock(Handle: HandleBasicBlock); cdecl;  external Triton_dll Name 'DeleteBasicBlock';

      //! Add an instruction to the block
      procedure BB_add(handle: HandleBasicBlock; instruction: HandleInstruz); cdecl;  external Triton_dll Name 'BB_add';

      //! Remove an instruction from the block at the given position. Returns
      //! true if success.
      function BB_remove(handle: HandleBasicBlock; position: UInt32): Boolean; cdecl;  external Triton_dll Name 'BB_remove';

      //! Gets all instructions of the block
      function BB_getInstructions(handle: HandleBasicBlock; var instructions: PHandleInstruz): UInt32; cdecl;  external Triton_dll Name 'BB_getInstructions';

      //! Returns the number of instructions in the block
      function BB_getSize(handle: HandleBasicBlock): Uint32; cdecl;  external Triton_dll Name 'BB_getSize';

      //! Returns the first instruction's address
      function BB_getFirstAddress(handle: HandleBasicBlock): UInt64; cdecl;  external Triton_dll Name 'BB_getFirstAddress';

      //! Returns the last instruction's address
      function BB_getLastAddress(handle: HandleBasicBlock): UInt64; cdecl;  external Triton_dll Name 'BB_getLastAddress';

implementation

{ TBasicBlock }

procedure TBasicBlock.Create;
begin
    ZeroMemory(@self,SizeOf(TBasicBlock));
    FBasicBlock := BB_BasicBlock;
end;

procedure TBasicBlock.Create(instructions: TArray<Istruzione>);
var
  pInstr : TArray< HandleInstruz>;
begin
   for var i := 0 to Length(instructions) -1 do
     pInstr := pInstr + [ instructions[i] ] ;

    FBasicBlock := BB_BasicBlockFromArray(@pInstr[0], Length(pInstr))
end;

procedure TBasicBlock.Create(other: TBasicBlock);
begin
    ZeroMemory(@self,SizeOf(TBasicBlock));
    FBasicBlock := BB_BasicBlockFrom(other);
end;

procedure TBasicBlock.Free;
begin
    DeleteBasicBlock(FBasicBlock);
end;

class operator TBasicBlock.Implicit(rBB: TBasicBlock): HandleBasicBlock;
begin
    Result := rBB.FBasicBlock;
end;

class operator TBasicBlock.Explicit(rBB: TBasicBlock): HandleBasicBlock;
begin
    Result := rBB.FBasicBlock;
end;

class operator TBasicBlock.Explicit(hBB: HandleBasicBlock): TBasicBlock;
begin
    ZeroMemory(@Result,SizeOf(TBasicBlock));
    Result.FBasicBlock  := hBB ;
end;

procedure TBasicBlock.Add(instruction: Istruzione);
begin
    BB_add(FBasicBlock, instruction);
end;

Function TBasicBlock.Remove(position: UInt32): Boolean;
begin
    Result := BB_remove(FBasicBlock, position);
end;

function TBasicBlock.GetInstructions: TArray<Istruzione>;
var
 numIstr : UInt32;
 pInstr  : PHandleInstruz;
begin
    pInstr :=nil;
    numIstr := BB_getInstructions(FBasicBlock, pInstr);

    for var i := 0 to numIstr - 1 do
       Result := Result +  [ Istruzione(pInstr[i]) ];

end;

function TBasicBlock.GetSize: UInt32;
begin
    Result := BB_getSize(FBasicBlock);
end;

function TBasicBlock.GetFirstAddress: UInt64;
begin
    Result := BB_getFirstAddress(FBasicBlock)
end;

function TBasicBlock.GetLastAddress: UInt64;
begin
    Result := BB_getLastAddress(FBasicBlock)
end;

function TBasicBlock.ToStr: string;
begin
    var instructions := GetInstructions;
    for var i := 0 to Length(instructions) - 1 do
       Result := Result + sLineBreak + instructions[i].ToStr;
end;

end.
