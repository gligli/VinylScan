unit tbbmalloc;

interface

uses
  windows;

const
  CTbbMallocDll = 'tbbmalloc.dll';

  // ScalableAllocationResult
  TBBMALLOC_OK = 0;
  TBBMALLOC_INVALID_PARAM = 1;
  TBBMALLOC_UNSUPPORTED = 2;
  TBBMALLOC_NO_MEMORY = 3;
  TBBMALLOC_NO_EFFECT = 4;

  // AllocationModeParam
  TBBMALLOC_USE_HUGE_PAGES = 0; // value turns using huge pages on and off

  // ScalableAllocationCmd
  TBBMALLOC_CLEAN_ALL_BUFFERS = 0; // Clean internal allocator buffers for all threads.
  TBBMALLOC_CLEAN_THREAD_BUFFERS = 1; // Clean internal allocator buffer for current thread only.

  // Set allocator-specific allocation modes.
  function scalable_allocation_mode(param: Integer; value: PtrInt): Integer; cdecl; external CTbbMallocDll;

  // Call allocator-specific commands.
  function scalable_allocation_command(cmd: Integer; param: Pointer): Integer; cdecl; external CTbbMallocDll;

implementation

function scalable_malloc (size: PtrInt): Pointer; cdecl; external CTbbMallocDll;
procedure scalable_free (ptr: Pointer); cdecl; external CTbbMallocDll;
function scalable_realloc (ptr: Pointer; size: PtrInt): Pointer; cdecl; external CTbbMallocDll;
function scalable_calloc (nobj: PtrInt; size: PtrInt): Pointer; cdecl; external CTbbMallocDll;
function scalable_msize (ptr: Pointer): PtrInt; cdecl; external CTbbMallocDll;

function FastGetLastError: Cardinal; assembler;
asm
  {$IFDEF WIN32}
  mov eax, fs:[$34]
  {$ENDIF WIN32}
  {$IFDEF WIN64}
  mov rax, gs:[$68]
  {$ENDIF WIN64}
end;

function TbbAllocMem(Size: PtrUInt): Pointer;
var
  LastError: Cardinal;
begin
  LastError := FastGetLastError;

  Result := scalable_calloc(1, Size);

  if LastError <> FastGetLastError then
    SetLastError(LastError);
end;

function TbbGetMem(Size: PtrUInt): Pointer;
var
  LastError: Cardinal;
begin
  LastError := FastGetLastError;

  Result := scalable_malloc(Size);

  if LastError <> FastGetLastError then
    SetLastError(LastError);
end;

function TbbReallocMem(var P: Pointer; Size: PtrUInt): Pointer;
var
  LastError: Cardinal;
begin
  LastError := FastGetLastError;

  if Assigned(P) then
  begin
    P := scalable_realloc(P, Size);
  end
  else
  begin
    P := scalable_malloc(Size)
  end;

  Result := P;

  if LastError <> FastGetLastError then
    SetLastError(LastError);
end;

function TbbFreeMem(P: Pointer): PtrUInt;
var
  LastError: Cardinal;
begin
  Result := 0;
  LastError := FastGetLastError;

  if Assigned(P) then
    scalable_free(P);

  if LastError <> FastGetLastError then
    SetLastError(LastError);
end;

function TbbFreeMemSize(P: Pointer; Size:ptruint): PtrUInt;
var
  LastError: Cardinal;
begin
  Result := 0;
  LastError := FastGetLastError;

  if Assigned(P) and (Size <> 0) then
    scalable_free(P);

  if LastError <> FastGetLastError then
    SetLastError(LastError);
end;

function TbbMemSize(P: Pointer): PtrUInt;
var
  LastError: Cardinal;
begin
  Result := 0;
  LastError := FastGetLastError;

  Result := scalable_msize(P);

  if LastError <> FastGetLastError then
    SetLastError(LastError);
end;

function TbbGetHeapStatus:THeapStatus;
begin
  fillchar(Result, sizeof(Result), 0);
end;

function TbbGetFPCHeapStatus:TFPCHeapStatus;
begin
  fillchar(Result, sizeof(Result), 0);
end;

Const
  TbbMemMgr : TMemoryManager = (
    NeedLock : false;
    GetMem : @TbbGetMem;
    FreeMem : @TbbFreeMem;
    FreememSize : @TbbFreeMemSize;
    AllocMem : @TbbAllocMem;
    ReallocMem : @TbbReallocMem;
    MemSize : @TbbMemSize;
    InitThread : nil;
    DoneThread : nil;
    RelocateHeap : nil;
    GetHeapStatus : @TbbGetHeapStatus;
    GetFPCHeapStatus: @TbbGetFPCHeapStatus;
  );

Var
  OldMemoryManager : TMemoryManager;

Initialization
  GetMemoryManager(OldMemoryManager);
  //scalable_allocation_mode(TBBMALLOC_USE_HUGE_PAGES, 1);
  SetMemoryManager(TbbMemMgr);

Finalization
  SetMemoryManager(OldMemoryManager);
end.

