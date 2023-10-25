unit Triton.Snapshot;

{$Z4}

interface
    uses System.SysUtils,Winapi.Windows,
        Triton.Define,
        Triton.Api,
        Triton.BitVector;

type

 Snapshot = record
   private
      FHSnapshot   : HandleSnapshot;
   Public
      procedure   Create(Ctx: TTritonCtx;Is64Bit: Boolean);
      procedure   Free;

      // Resets the snapshot engine
      procedure resetEngine;
      // Restores a snapshot
      procedure restoreSnapshot;
      // Takes a snapshot
      procedure takeSnapshot;

      class Operator Implicit(hSnap: HandleSnapshot):Snapshot;
      class Operator Implicit(Snap: Snapshot):HandleSnapshot;

 end;

  //* Snapshot =======================================================================================*/

  // Constructor
  function CreaSnapshot(HAPI: HandleContext; Is64Bit: Boolean): HandleSnapshot; cdecl;  external Triton_dll;

  // Destructor
  procedure DeleteSnapshot(Handle: HandleSnapshot); cdecl;  external Triton_dll;

  // Resets the snapshot engine
  procedure snap_resetEngine(Handle: HandleSnapshot); cdecl;  external Triton_dll;

  // Restores a snapshot
  procedure snap_restoreSnapshot(Handle: HandleSnapshot); cdecl;  external Triton_dll ;

  // Takes a snapshot
  procedure snap_takeSnapshot(Handle: HandleSnapshot); cdecl;  external Triton_dll ;


implementation


{ Snapshot }

procedure Snapshot.Create(Ctx: TTritonCtx; Is64Bit: Boolean);
begin
    ZeroMemory(@self,SizeOf(Snapshot));
    FHSnapshot := CreaSnapshot(Ctx,Is64Bit);
end;

class operator Snapshot.Implicit(hSnap: HandleSnapshot): Snapshot;
begin
    Result.FHSnapshot := hSnap;
end;

class operator Snapshot.Implicit(Snap: Snapshot): HandleSnapshot;
begin
    Result := Snap.FHSnapshot;
end;

procedure Snapshot.Free;
begin
    DeleteSnapshot(FHSnapshot);
end;

procedure Snapshot.resetEngine;
begin
    snap_resetEngine(FHSnapshot);
end;

procedure Snapshot.restoreSnapshot;
begin
    snap_restoreSnapshot(FHSnapshot);
end;

procedure Snapshot.takeSnapshot;
begin
    snap_takeSnapshot(FHSnapshot);
end;

end.
