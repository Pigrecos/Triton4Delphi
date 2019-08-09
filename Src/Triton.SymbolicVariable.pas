unit Triton.SymbolicVariable;

{$Z4}

interface
    uses System.SysUtils, Winapi.Windows,
         Triton.Register,
         Triton.MemoryAccess,
         Triton.AstNode,
         Triton.Define;

type

  SymbolicVar = record
    private
      FHSymVar : HandleSharedSymbolicVariable;

      function  getType: variable_e ;
      function  getAlias: AnsiString ;
      function  getComment:AnsiString ;
      function  getName:AnsiString ;
      function  getId: usize ;
      function  getOrigin: uint64 ;
      function  getSize:uint32 ;

      procedure setAlias(sAlias: AnsiString);
      procedure setComment(comment: AnsiString);
    public
      procedure Create(tipo: variable_e; origin: uint64; id: usize; size: uint32;	comment: PAnsiChar= nil); overload;
      procedure Create(other: SymbolicVar);overload;
      procedure Free;
      function ToStr:string;

      class operator Explicit(hSymVar: HandleSharedSymbolicVariable): SymbolicVar;
      class operator Explicit(rSymVar: SymbolicVar): HandleSharedSymbolicVariable;

      property Tipo    : variable_e read getType;
      property Alias   : AnsiString read getAlias   write setAlias;
      property Comment : AnsiString read getComment write setComment;
      property Name    : AnsiString read getName ;
      property Id      : usize      read getId ;
      property Origin  : uint64     read getOrigin ;
      property Size    : UInt32     read getSize ;

  end;

  (*  SymbolicVariable ============================================================================== *)
        //! Constructor.
        function SVCreateSymbolicVariable(tipo: variable_e; origin: uint64; id: usize; size: uint32;	comment: PAnsiChar): HandleSharedSymbolicVariable; cdecl;  external Triton_dll Name 'SVCreateSymbolicVariable';

        //! Constructor by copy.
        function SVCreateSymbolicVariableFrom(other: HandleSharedSymbolicVariable): HandleSharedSymbolicVariable;cdecl;  external Triton_dll Name 'SVCreateSymbolicVariableFrom';

        //! Destructor.
        procedure  SVDelete(Handle: HandleSharedSymbolicVariable); cdecl;  external Triton_dll Name 'SVDelete';

        //! Returns the symbolic variable type assignment.
        function SVgetType(Handle: HandleSharedSymbolicVariable): variable_e ;cdecl;  external Triton_dll Name 'SVgetType';

        //! Returns the alias of the symbolic variable.
        procedure SVgetAlias(Handle: HandleSharedSymbolicVariable; var sAlias: PAnsiChar) ; cdecl;  external Triton_dll Name 'SVgetAlias';

        //! Returns the comment of the symbolic variable.
        procedure SVgetComment(Handle: HandleSharedSymbolicVariable; var sComment: PAnsiChar) ; cdecl;  external Triton_dll Name 'SVgetComment';

        //! Returns the name of the symbolic variable.
        procedure SVgetName(Handle: HandleSharedSymbolicVariable; var sName: PAnsiChar) ; cdecl;  external Triton_dll Name 'SVgetName';

        //! Returns the id of the symbolic variable. This id is unique.
        function SVgetId(Handle: HandleSharedSymbolicVariable): usize ; cdecl;  external Triton_dll Name 'SVgetId';

        //! Returns the source value of the symbolic variable.
        function SVgetOrigin(Handle: HandleSharedSymbolicVariable): uint64 ; cdecl;  external Triton_dll Name 'SVgetOrigin';

        //! Returns the size (in bits) of the symbolic variable.
        function SVgetSize(Handle: HandleSharedSymbolicVariable): uint32 ; cdecl;  external Triton_dll Name 'SVgetSize';

        //! Sets the alias of the symbolic variable.
        procedure SVsetAlias(Handle: HandleSharedSymbolicVariable; sAlias: PAnsiChar);cdecl;  external Triton_dll Name 'SVsetAlias';

        //! Sets the comment of the symbolic variable.
        procedure SVsetComment(Handle: HandleSharedSymbolicVariable; comment: PAnsiChar); cdecl;  external Triton_dll Name 'SVsetComment';

implementation

{ SymbolicVar }

procedure SymbolicVar.Create(tipo: variable_e; origin: uint64; id: usize; size: uint32; comment: PAnsiChar);
begin
     ZeroMemory(@self,SizeOf(SymbolicVar));
     FHSymVar := SVCreateSymbolicVariable(tipo,origin,id,size,comment)
end;

procedure SymbolicVar.Create(other: SymbolicVar);
begin
    ZeroMemory(@self,SizeOf(SymbolicVar));
    FHSymVar := SVCreateSymbolicVariableFrom( HandleSharedSymbolicVariable(other) )
end;

class operator SymbolicVar.Explicit(rSymVar: SymbolicVar): HandleSharedSymbolicVariable;
begin
    Result := rSymVar.FHSymVar;
end;

class operator SymbolicVar.Explicit(hSymVar: HandleSharedSymbolicVariable): SymbolicVar;
begin
    ZeroMemory(@Result,SizeOf(SymbolicVar));
    Result.FHSymVar :=  hSymVar;
end;

procedure SymbolicVar.Free;
begin
    SVDelete(FHSymVar);
    ZeroMemory(@self,SizeOf(SymbolicVar));
end;

function SymbolicVar.getAlias: AnsiString;
var
 p : PAnsiChar;
begin
    SVgetAlias(FHSymVar,p);
    Result := AnsiString(p)
end;

function SymbolicVar.getComment: AnsiString;
var
 p : PAnsiChar;
begin
    SVgetComment(FHSymVar,p);
    Result := AnsiString(p)
end;

function SymbolicVar.getId: usize;
begin
    Result := SVgetId(FHSymVar)
end;

function SymbolicVar.getName: AnsiString;
var
 p : PAnsiChar;
begin
    SVgetName(FHSymVar,p);
    Result := AnsiString(p)
end;

function SymbolicVar.getOrigin: uint64;
begin
    Result := SVgetOrigin(FHSymVar)
end;

function SymbolicVar.getSize: uint32;
begin
    Result := SVgetSize(FHSymVar)
end;

function SymbolicVar.getType: variable_e;
begin
    Result := SVgetType(FHSymVar)
end;

procedure SymbolicVar.setAlias(sAlias: AnsiString);
begin
    SVsetAlias(FHSymVar, PAnsiChar(sAlias));
end;

procedure SymbolicVar.setComment(comment: AnsiString);
begin
    SVsetComment(FHSymVar, PAnsiChar(comment));
end;

function SymbolicVar.ToStr: string;
begin
    if Alias = '' then  Result := string(Name)  +':'+ IntToStr(Size)
    else                Result := string(Alias) +':'+ IntToStr(Size)
end;

end.
