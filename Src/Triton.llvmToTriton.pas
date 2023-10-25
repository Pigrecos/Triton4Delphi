unit Triton.llvmToTriton;

{$Z4}
{$POINTERMATH ON}

interface
    uses
       System.SysUtils, Winapi.Windows, System.Generics.Collections,
       LLVM.Imports,
       LLVM.Imports.Types,
       LLVM.Imports.Core,
       LLVM.Imports.Analysis,
       LLVM.Imports.IRReader,
       LLVM.Imports.BitReader,
       LLVM.Imports.BitWriter,

       Triton.Define,
       Triton.AstNode,
       triton.AstContext,
       triton.Api;

type
  TllvmToTriton = record
   private
    FHLLvmToTriton   : HLLVMToTriton;
   public
    constructor Create(ctx: TTritonCtx);  overload;
    constructor Create(actx : AstContext);  overload;
    procedure Free;
    function convert(module: TLLVMModuleRef; fName : AnsiString= '__triton'): AbstractNode; overload;
    function convert(instruction: TLLVMValueRef): AbstractNode; overload;

    class Operator Explicit(hSR: HLLVMToTriton):TllvmToTriton;
    class Operator Explicit(rSR: TllvmToTriton):HLLvmToTriton;
    class Operator Implicit(rSR: TllvmToTriton):HLLvmToTriton;
  end;

  procedure saveModuleToFile(DumpModule: TLLVMModuleRef; Filepath: String);
  function loadModuleFromFile(ModuleNameName_II : string; Ctx : TLLVMContextRef): TLLVMModuleRef;

       (*  LLVMToTriton ======================================================================== *)

      //! Constructor.
      function LLVMToTritonCtx(ctx: HandleContext): HLLVMToTriton; cdecl;  external Triton_dll Name 'LLVMToTritonCtx';

      //! Constructor.
      function LLVMToTritonNode(actx: HandleAstContext): HLLVMToTriton;  cdecl;  external Triton_dll Name 'LLVMToTritonNode';

      //! Destructor.
      procedure LLVMToTritonDelete(Handle: HLLVMToTriton); cdecl;  external Triton_dll Name 'LLVMToTritonDelete';

      //! Converts a given function from an LLVM module to a Triton AST.
      function convertModule(Handle: HLLVMToTriton; llvmModule: TLLVMModuleRef; fname: PAnsichar): HandleAbstractNode; cdecl;  external Triton_dll Name 'convertModule';

      //! Converts an LLVM instruction function to a Triton AST.
      function convertValue(Handle: HLLVMToTriton; instruction: TLLVMValueRef): HandleAbstractNode;cdecl;  external Triton_dll Name 'convertValue';

implementation

procedure saveModuleToFile(DumpModule: TLLVMModuleRef; Filepath: String);
var
 llFile      : Text;
 strModule   : PAnsiChar;
begin
    strModule := LLVMPrintModuleToString(DumpModule);

    AssignFile(llFile,Filepath);
    try
      Rewrite(llFile);
      Writeln(llFile, string(AnsiString( strModule)));

    finally
      CloseFile(llFile);
    end;
end;

function loadModuleFromFile(ModuleNameName_II : string; Ctx : TLLVMContextRef): TLLVMModuleRef;
var
 bufLL       : TLLVMMemoryBufferRef;
 msg         : PAnsiChar;
 DumpModule  : TLLVMModuleRef;

begin
    if LLVMCreateMemoryBufferWithContentsOfFile(PAnsiChar(AnsiString(ModuleNameName_II)),bufLL, msg) then
    begin
       if  LLVMParseIRInContext(Ctx,bufLL,DumpModule,msg) then
         Result := DumpModule;
    end;
end;

{ TllvmToTriton }

constructor TllvmToTriton.Create(ctx: TTritonCtx);
begin
    ZeroMemory(@Self,SizeOf(TllvmToTriton));
    FHLLvmToTriton := LLVMToTritonCtx(ctx);
end;

constructor TllvmToTriton.Create(actx: AstContext);
begin
    ZeroMemory(@Self,SizeOf(TllvmToTriton));
    FHLLvmToTriton := LLVMToTritonNode(hAstRepresentation(actx));
end;

procedure TllvmToTriton.Free;
begin
    LLVMToTritonDelete(FHLLvmToTriton);
    Self := default(TllvmToTriton);
end;

function TllvmToTriton.convert(module: TLLVMModuleRef; fName : AnsiString): AbstractNode;
begin
    Result := convertModule(FHLLvmToTriton, module, PAnsiChar(fName))
end;

function TllvmToTriton.convert(instruction: TLLVMValueRef): AbstractNode;
begin
    Result := convertValue(FHLLvmToTriton, instruction)
end;

class operator TllvmToTriton.Explicit(rSR: TllvmToTriton): HLLvmToTriton;
begin
    Result := rSR.FHLLvmToTriton;
end;

class operator TllvmToTriton.Explicit(hSR: HLLVMToTriton): TllvmToTriton;
begin
    ZeroMemory(@Result,SizeOf(TllvmToTriton));
    Result.FHLLvmToTriton  := hSR ;
end;

class operator TllvmToTriton.Implicit(rSR: TllvmToTriton): HLLvmToTriton;
begin
    Result := rSR.FHLLvmToTriton;
end;

end.
