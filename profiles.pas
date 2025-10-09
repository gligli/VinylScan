unit profiles;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, IniFiles, FileCtrl, FileUtil, utils;

type

  { TProfile }

  TProfile = class
  private
    FName: String;
    FRevolutionsPerSecond: Double;
    FRecordingGrooveWidth: Double;
    FRecordingGrooveThickness: Double;
    FLeadOutGrooveThickness: Double;
    FOuterSize: Double;
    FInnerSize: Double;
    FLabelOuterSize: Double;
    FConcentricGroove: Double;
    FMinConcentricGroove: Double;
    FMaxConcentricGroove: Double;
    FStylusSetDown: Double;
    FFirstMusicGroove: Double;
    FLastMusicGroove: Double;
    FAdapterSize: Double;
    FMono: Boolean;
    FCorrectAngleCount: Integer;

  public
    constructor Create(AFN: String);

    property Name: String read FName;

    property RevolutionsPerSecond: Double     read FRevolutionsPerSecond;
    property RecordingGrooveWidth: Double     read FRecordingGrooveWidth;
    property RecordingGrooveThickness: Double read FRecordingGrooveThickness;
    property LeadOutGrooveThickness: Double   read FLeadOutGrooveThickness;
    property OuterSize: Double                read FOuterSize;
    property InnerSize: Double                read FInnerSize;
    property LabelOuterSize: Double           read FLabelOuterSize;
    property ConcentricGroove: Double         read FConcentricGroove;
    property MinConcentricGroove: Double      read FMinConcentricGroove;
    property MaxConcentricGroove: Double      read FMaxConcentricGroove;
    property StylusSetDown: Double            read FStylusSetDown;
    property FirstMusicGroove: Double         read FFirstMusicGroove;
    property LastMusicGroove: Double          read FLastMusicGroove;
    property AdapterSize: Double              read FAdapterSize;
    property Mono: Boolean                    read FMono;
    property CorrectAngleCount: Integer       read FCorrectAngleCount;
  end;

  TProfileArray = array of TProfile;

  { TProfiles }

  TProfiles = class
  private
    FProfiles: TProfileArray;
    FCurrentProfileRef: TProfile;
  public
    constructor Create(ADir: String);
    destructor Destroy; override;

    property Profiles: TProfileArray read FProfiles;
    property CurrentProfileRef: TProfile read FCurrentProfileRef write FCurrentProfileRef;
  end;


implementation

{ TProfile }

constructor TProfile.Create(AFN: String);
const
  CSection = 'Profile';
var
  ini: TIniFile;

  function GetValue(AIdent: string): Double;
  begin
    Result := ini.ReadFloat(CSection, AIdent, NaN);
    Assert(not IsNan(Result), AFN + ': ' + AIdent + ' Not found!');
  end;

begin
  FName := ChangeFileExt(ExtractFileName(AFN), '');;

  ini := TIniFile.Create(AFN, [ifoFormatSettingsActive]);
  try
    ini.FormatSettings := InvariantFormatSettings;

    FRevolutionsPerSecond     := GetValue('RevolutionsPerMinute') / 60.0;
    FRecordingGrooveWidth     := GetValue('RecordingGrooveWidth');
    FRecordingGrooveThickness := GetValue('RecordingGrooveThickness');
    FLeadOutGrooveThickness   := GetValue('LeadOutGrooveThickness');
    FOuterSize                := GetValue('OuterSize');
    FInnerSize                := GetValue('InnerSize');
    FLabelOuterSize           := GetValue('LabelOuterSize');
    FConcentricGroove         := GetValue('ConcentricGroove');
    FMinConcentricGroove      := GetValue('ConcentricGrooveLowTol') * -1 + FConcentricGroove;
    FMaxConcentricGroove      := GetValue('ConcentricGrooveHighTol') + FConcentricGroove;
    FStylusSetDown            := GetValue('StylusSetDown');
    FFirstMusicGroove         := GetValue('FirstMusicGroove');
    FLastMusicGroove          := GetValue('LastMusicGroove');
    FAdapterSize              := GetValue('AdapterSize');

    FMono                     := ini.ReadBool(CSection, 'Mono', True);
    FCorrectAngleCount        := ini.ReadInteger(CSection, 'CorrectAngleCount', 36);
  finally
    ini.Free;
  end;
end;

{ TProfiles }

constructor TProfiles.Create(ADir: String);
var
  Info: TSearchRec;
begin
  ADir := IncludeTrailingPathDelimiter(ADir);

  if FindFirst(ADir + '*.ini', faNormal, Info) = 0 then
  begin
    repeat
      SetLength(FProfiles, Length(FProfiles) + 1);
      FProfiles[High(FProfiles)] := TProfile.Create(ADir + Info.Name);
    until FindNext(info) <> 0;
    FindClose(Info);
  end;

  if Length(FProfiles) > 0 then
    FCurrentProfileRef := FProfiles[0];
end;

destructor TProfiles.Destroy;
var
  iProfile: Integer;
begin
  for iProfile := 0 to High(FProfiles) do
    FProfiles[iProfile].Free;

  inherited Destroy;
end;

end.

