unit ksSystemSound;

interface

{$I ksComponents.inc}

uses System.Classes, ksTypes;

const
  libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';


type

  TksSystemSound = class(TInterfacedObject, IksSystemSound)
  public
    procedure Play(ASound: TksSound);
  end;

implementation

uses Types;

  {$IFDEF IOS}

  procedure AudioServicesPlaySystemSound( inSystemSoundID: UInt32 ); cdecl; external libAudioToolbox name 'AudioServicesPlaySystemSound';
  procedure AudioServicesPlayAlertSound( inSystemSoundID: UInt32 ); cdecl; external libAudioToolbox name 'AudioServicesPlayAlertSound';

  {$ENDIF}

 
procedure TksSystemSound.Play(ASound: TksSound);
{$IFDEF IOS}
var
  AId: integer;
{$ENDIF}
begin
  //AId := -1;
  {$IFDEF IOS}
  case ASound of
    ksMailNew: AId := 1000;
    ksMailSent: AId := 1001;
    ksVoiceMail: AId := 1002;
    ksMessageReceived: AId := 1003;
    ksMessageSent: AId := 1004;
    ksBeep: AId := 1052;
  end;

  if AId = -1 then
    Exit;
    
  AudioServicesPlaySystemSound(AId);    
  {$ENDIF}
end;

end.
