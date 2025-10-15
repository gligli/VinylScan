unit filter_rbj; //uRbjEqFilters;
//http://en.wikipedia.org/wiki/Digital_biquad_filter
//http://www.musicdsp.org/archive.php?classid=3#225
//http://www.musicdsp.org/showone.php?id=197
//http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt
//  http://www.musicdsp.org/archive.php?classid=3#225
//http://www.musicdsp.org/showArchiveComment.php?ArchiveID=225
  
  {
  RBJ Audio EQ Cookbook Filters
  A pascal conversion of arguru[AT]smartelectronix[DOT]com's
  c++ implementation.
  WARNING:This code is not FPU undernormalization safe.
  }
interface

uses math;

const
  kLowPass=0;//-LowPass
  kHighPass=1;//-HiPass
  kBandPassCSG=2;//-BandPass CSG
  kBandPassCZPG=3;//-BandPass CZPG
  kNotch=4;//-Notch
  kAll=5;//-AllPass
  kPeaking=6;//-Peaking
  kLowShelf=7;//-LowShelf
  kHighShelf=8;//-HiShelf

type
  TRbjEqFilter=class
  private
    b0a0,b1a0,b2a0,a1a0,a2a0:double;
    in1,in2,ou1,ou2:double;
    fSampleRate:double;
    fMaxBlockSize:integer;
    fFilterType:integer;
    fFreq,fQ,fDBGain:double;
    fQIsBandWidth:boolean;
    procedure SetQ(NewQ:double);
  public
    out1:array of double;
    constructor create(SampleRate:double;MaxBlockSize:integer);
    procedure CalcFilterCoeffs(pFilterType:integer;pFreq,pQ,pDBGain:double;pQIsBandWidth:boolean);overload;
    procedure CalcFilterCoeffs;overload;
    function Process(input:double):double; overload;
    procedure Process(Input:pdouble;sampleframes:integer); overload;
    property FilterType:integer read fFilterType write fFilterType;
    property Freq:double read fFreq write fFreq;
    property q:double read fQ write SetQ;
    property DBGain:double read fDBGain write fDBGain;
    property QIsBandWidth:boolean read fQIsBandWidth write fQIsBandWidth;
  end;

implementation

constructor TRbjEqFilter.create(SampleRate:double;MaxBlockSize:integer);
begin
  fMaxBlockSize:=MaxBlockSize;
  setLength(out1,fMaxBlockSize);

  fFilterType:=0;
  fSampleRate:=SampleRate;
  fFreq:=10;
  fQ:=0.3;
  fDBGain:=0;
  fQIsBandWidth:=true;

  in1:=0;
  in2:=0;
  ou1:=0;
  ou2:=0;
end;

procedure TRbjEqFilter.SetQ(NewQ:double);
begin
  fQ:=(1-NewQ)*0.98;
end;

procedure TRbjEqFilter.CalcFilterCoeffs(pFilterType:integer;pFreq,pQ,pDBGain:double;pQIsBandWidth:boolean);
begin
  FilterType:=pFilterType;
  Freq:=pFreq;
  Q:=pQ;
  DBGain:=pDBGain;
  QIsBandWidth:=pQIsBandWidth;

  CalcFilterCoeffs;
end;

procedure TRbjEqFilter.CalcFilterCoeffs;
var
  alpha,a0,a1,a2,b0,b1,b2:double;
  A,beta,omega,tsin,tcos:double;
begin
    if (fFreq*2) >= fSampleRate then exit;
  //peaking, LowShelf or HiShelf
  if fFilterType>=6 then
  begin
    A:=power(10.0,(DBGain/40.0));
    omega:=2*pi*fFreq/fSampleRate;
    tsin:=sin(omega);
    tcos:=cos(omega);

    if fQIsBandWidth then
      alpha:=tsin*sinh(log2(2.0)/2.0*fQ*omega/tsin)
    else
      alpha:=tsin/(2.0*fQ);

    beta:=sqrt(A)/fQ;

    // peaking
    if fFilterType=6 then
    begin
      b0:=1.0+alpha*A;
      b1:=-2.0*tcos;
      b2:=1.0-alpha*A;
      a0:=1.0+alpha/A;
      a1:=-2.0*tcos;
      a2:=1.0-alpha/A;
    end else
    // lowshelf
    if fFilterType=7 then
    begin
      b0:=(A*((A+1.0)-(A-1.0)*tcos+beta*tsin));
      b1:=(2.0*A*((A-1.0)-(A+1.0)*tcos));
      b2:=(A*((A+1.0)-(A-1.0)*tcos-beta*tsin));
      a0:=((A+1.0)+(A-1.0)*tcos+beta*tsin);
      a1:=(-2.0*((A-1.0)+(A+1.0)*tcos));
      a2:=((A+1.0)+(A-1.0)*tcos-beta*tsin);
    end;
    // hishelf
    if fFilterType=8 then
    begin
      b0:=(A*((A+1.0)+(A-1.0)*tcos+beta*tsin));
      b1:=(-2.0*A*((A-1.0)+(A+1.0)*tcos));
      b2:=(A*((A+1.0)+(A-1.0)*tcos-beta*tsin));
      a0:=((A+1.0)-(A-1.0)*tcos+beta*tsin);
      a1:=(2.0*((A-1.0)-(A+1.0)*tcos));
      a2:=((A+1.0)-(A-1.0)*tcos-beta*tsin);
    end;
  end else  //other filter types
  begin
    omega:=2*pi*fFreq/fSampleRate;
    tsin:=sin(omega);
    tcos:=cos(omega);
    if fQIsBandWidth then
      alpha:=tsin*sinh(log2(2)/2*fQ*omega/tsin)
    else
      alpha:=tsin/(2*fQ);
    //lowpass
    if fFilterType=0 then
    begin
      b0:=(1-tcos)/2;
      b1:=1-tcos;
      b2:=(1-tcos)/2;
      a0:=1+alpha;
      a1:=-2*tcos;
      a2:=1-alpha;
    end else //hipass
    if fFilterType=1 then
    begin
      b0:=(1+tcos)/2;
      b1:=-(1+tcos);
      b2:=(1+tcos)/2;
      a0:=1+alpha;
      a1:=-2*tcos;
      a2:=1-alpha;
    end else //bandpass CSG
    if fFilterType=2 then
    begin
      b0:=tsin/2;
      b1:=0;
      b2:=-tsin/2;
      a0:=1+alpha;
      a1:=-1*tcos;
      a2:=1-alpha;
    end else //bandpass CZPG
    if fFilterType=3 then
    begin
      b0:=alpha;
      b1:=0.0;
      b2:=-alpha;
      a0:=1.0+alpha;
      a1:=-2.0*tcos;
      a2:=1.0-alpha;
    end else  //notch
    if fFilterType=4 then
    begin
      b0:=1.0;
      b1:=-2.0*tcos;
      b2:=1.0;
      a0:=1.0+alpha;
      a1:=-2.0*tcos;
      a2:=1.0-alpha;
    end else   //allpass
    if fFilterType=5 then
    begin
      b0:=1.0-alpha;
      b1:=-2.0*tcos;
      b2:=1.0+alpha;
      a0:=1.0+alpha;
      a1:=-2.0*tcos;
      a2:=1.0-alpha;
    end;
  end;

  b0a0:=b0/a0;
  b1a0:=b1/a0;
  b2a0:=b2/a0;
  a1a0:=a1/a0;
  a2a0:=a2/a0;
end;


function TRbjEqFilter.Process(input:double):double;
var
  LastOut:double;
begin
  if (fFreq*2) >= fSampleRate then begin //beyond NyQuist
    result := input;
    exit;
  end;
  // filter
  LastOut:= b0a0*input + b1a0*in1 + b2a0*in2 - a1a0*ou1 - a2a0*ou2;

  // push in/out buffers
  in2:=in1;
  in1:=input;
  ou2:=ou1;
  ou1:=LastOut;

  // return output
  result:=LastOut;
end;

{
the process method is overloaded.
use Process(input:double):double;
for per sample processing
use Process(Input:pdouble;sampleframes:integer);
for block processing. The input is a pointer to
the start of an array of double which contains
the audio data.
i.e.
RBJFilter.Process(@WaveData[0],256);
}

procedure TRbjEqFilter.Process(Input:pdouble;sampleframes:integer);
var
  i:integer;
  LastOut:double;
begin
  for i:=0 to SampleFrames-1 do
  begin
    // filter
    LastOut:= b0a0*(input^)+ b1a0*in1 + b2a0*in2 - a1a0*ou1 - a2a0*ou2;
    //LastOut:=input^;
    // push in/out buffers
    in2:=in1;
    in1:=input^;
    ou2:=ou1;
    ou1:=LastOut;

    Out1[i]:=LastOut;

    inc(input);
  end;
end;

end.



end.
 
