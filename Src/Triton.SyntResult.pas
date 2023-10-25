unit Triton.SyntResult;

{$Z4}
{$POINTERMATH ON}

interface
    uses
       System.SysUtils, Winapi.Windows, System.Generics.Collections,
       Triton.Define,
       Triton.AstNode;

type
  SynthesisResult = record
   private
    FSyntResult   : HandleSynthesisResult;
   public
    procedure Create; overload;
    procedure Create(other : SynthesisResult); overload;
    procedure Free;
    function getInput: AbstractNode;
    function getOutput: AbstractNode;
    function successful: Boolean;

    class Operator Explicit(hSR: HandleSynthesisResult):SynthesisResult;
    class Operator Explicit(rSR: SynthesisResult):HandleSynthesisResult;
    class Operator Implicit(rSR: SynthesisResult):HandleSynthesisResult;
  end;


      (*  SynthesisResult ======================================================================== *)

      //! Constructor.
      function SyntResult: HandleSynthesisResult; cdecl;  external Triton_dll Name 'SyntResult';

      //! Constructor by copy.
      function SyntResultFrom(other: HandleSynthesisResult): HandleSynthesisResult; cdecl;  external Triton_dll Name 'SyntResultFrom';

      //! Destructor.
      procedure SyntResultDelete(Handle: HandleSynthesisResult); cdecl;  external Triton_dll Name 'SyntResultDelete';

      //! Gets the input node
      function SyntResultgetInput(Handle: HandleSynthesisResult): HandleAbstractNode; cdecl;  external Triton_dll Name 'SyntResultgetInput';

      //! Gets the output node
      function SyntResultgetOutput(Handle: HandleSynthesisResult): HandleAbstractNode; cdecl;  external Triton_dll Name 'SyntResultgetOutput';

      //! Returns True the input node has been synthesized successfully.
      function SyntResultsuccessful(Handle: HandleSynthesisResult): Boolean; cdecl;  external Triton_dll Name 'SyntResultsuccessful';

implementation

{ SynthesisResult }

procedure SynthesisResult.Create;
begin
    ZeroMemory(@self,SizeOf(SynthesisResult));
    FSyntResult := SyntResult;
end;

procedure SynthesisResult.Create(other: SynthesisResult);
begin
    ZeroMemory(@self,SizeOf(SynthesisResult));
    FSyntResult := SyntResultFrom(other);
end;

procedure SynthesisResult.Free;
begin
    SyntResultDelete(FSyntResult);
    Self := default(SynthesisResult);
end;

function SynthesisResult.getInput: AbstractNode;
begin
    Result := SyntResultgetInput(FSyntResult) ;
end;

function SynthesisResult.getOutput: AbstractNode;
begin
    Result := SyntResultgetOutput(FSyntResult);
end;

function SynthesisResult.successful: Boolean;
begin
    Result := SyntResultsuccessful(FSyntResult);
end;

class operator SynthesisResult.Implicit(rSR: SynthesisResult): HandleSynthesisResult;
begin
    Result := rSR.FSyntResult;
end;

class operator SynthesisResult.Explicit(hSR: HandleSynthesisResult): SynthesisResult;
begin
    ZeroMemory(@Result,SizeOf(SynthesisResult));
    Result.FSyntResult  := hSR ;
end;

class operator SynthesisResult.Explicit(rSR: SynthesisResult): HandleSynthesisResult;
begin
    Result := rSR.FSyntResult;
end;

end.
