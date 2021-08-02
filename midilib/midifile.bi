''
''
'' midifile -- header translated with help of SWIG FB wrapper
''
'' NOTICE: This file is part of the FreeBASIC Compiler package and can't
''         be included in other distributions without authorization.
''
''
#ifndef __midifile_bi__
#define __midifile_bi__

type WORD as uint16_t
type DWORD as uint32_t
type BOOLINT as integer

#define TRUE 1
#define FALSE 0
#define MIDI_PPQN_DEFAULT 384
#define MIDI_VERSION_DEFAULT 1
#define MAX_MIDI_TRACKS 256
#define MAX_TRACK_POLYPHONY 64

type MIDI_FILE as any

type MIDI_MSG__NESTED__MsgData__NESTED__SysEx
	pData as UByte ptr
	iSize as integer
end type

type MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__Sequencer
	pData as UByte ptr
	iSize as integer
end type



type MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__TimeSig
	iNom as integer
	iDenom as integer
end type

Type MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__KeySig
	iKey as tMIDI_KEYSIG
end type

Type MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__SMPTE
	iHours as integer
	iMins as Integer
	iSecs as integer
	iFrames as integer
	iFF as integer
end type


type MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__Tempo
	iBPM as integer
end type

type MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__Text
	pData as UByte ptr
end Type

union MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data
	iMIDIPort as integer
	iSequenceNumber as integer
	Sequencer as MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__Sequencer
	TimeSig as MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__TimeSig
	KeySig as MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__KeySig
	SMPTE as MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__SMPTE
	Tempo as MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__Tempo
	Text as MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data__NESTED__Text
end union


type MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent
	iType as tMIDI_META
	Data as MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent__NESTED__Data
end type

type MIDI_MSG__NESTED__MsgData__NESTED__PitchWheel
	iChannel as integer
	iPitch as integer
end type

type MIDI_MSG__NESTED__MsgData__NESTED__ChangePressure
	iChannel as integer
	iPressure as integer
End type

type MIDI_MSG__NESTED__MsgData__NESTED__ChangeProgram
	iChannel as integer
	iProgram as integer
end type

Type MIDI_MSG__NESTED__MsgData__NESTED__NoteParameter
	iChannel as integer
	iControl as tMIDI_CC
	iParam as integer
end type

type MIDI_MSG__NESTED__MsgData__NESTED__NoteKeyPressure
	iNote as integer
	iChannel as integer
	iPressure as integer
end type

type MIDI_MSG__NESTED__MsgData__NESTED__NoteOff
	iNote as integer
	iChannel as integer
end type

type MIDI_MSG__NESTED__MsgData__NESTED__NoteOn
	iNote as integer
	iChannel as integer
	iVolume as integer
end type

union MIDI_MSG__NESTED__MsgData
	SysEx as MIDI_MSG__NESTED__MsgData__NESTED__SysEx
	MetaEvent as MIDI_MSG__NESTED__MsgData__NESTED__MetaEvent
	PitchWheel as MIDI_MSG__NESTED__MsgData__NESTED__PitchWheel
	ChangePressure as MIDI_MSG__NESTED__MsgData__NESTED__ChangePressure
	ChangeProgram as MIDI_MSG__NESTED__MsgData__NESTED__ChangeProgram
	NoteParameter as MIDI_MSG__NESTED__MsgData__NESTED__NoteParameter
	NoteKeyPressure as MIDI_MSG__NESTED__MsgData__NESTED__NoteKeyPressure
	NoteOff as MIDI_MSG__NESTED__MsgData__NESTED__NoteOff
	NoteOn as MIDI_MSG__NESTED__MsgData__NESTED__NoteOn
end union



type MIDI_MSG
	iType as tMIDI_MSG
	dt as DWORD
	dwAbsPos as DWORD
	iMsgSize as DWORD
	bImpliedMsg as BOOLINT
	iImpliedMsg as tMIDI_MSG
	data as UByte ptr
	data_sz as DWORD
	iLastMsgType as tMIDI_MSG
	iLastMsgChnl as UByte
	MsgData as MIDI_MSG__NESTED__MsgData
end type








declare function midiFileCreate cdecl alias "midiFileCreate" (byval pFilename as zstring ptr, byval bOverwriteIfExists as BOOLINT) as MIDI_FILE ptr
declare function midiFileSetTracksDefaultChannel cdecl alias "midiFileSetTracksDefaultChannel" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iChannel as integer) as integer
declare function midiFileGetTracksDefaultChannel cdecl alias "midiFileGetTracksDefaultChannel" (byval pMF as MIDI_FILE ptr, ByVal iTrack as integer) as integer
declare function midiFileFlushTrack cdecl alias "midiFileFlushTrack" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval bFlushToEnd as BOOLINT, byval dwEndTimePos as DWORD) as BOOLINT
declare function midiFileSyncTracks cdecl alias "midiFileSyncTracks" (ByVal pMF as MIDI_FILE ptr, byval iTrack1 as integer, byval iTrack2 as integer) as BOOLINT
declare function midiFileSetPPQN cdecl alias "midiFileSetPPQN" (byval pMF as MIDI_FILE ptr, byval PPQN as integer) as integer
declare function midiFileGetPPQN cdecl alias "midiFileGetPPQN" (byval pMF as MIDI_FILE ptr) as integer
declare function midiFileSetVersion cdecl alias "midiFileSetVersion" (byval pMF as MIDI_FILE ptr, byval iVersion as integer) as integer
declare function midiFileGetVersion cdecl alias "midiFileGetVersion" (byval pMF as MIDI_FILE ptr) as integer
declare function midiFileOpen cdecl alias "midiFileOpen" (byval pFilename as zstring ptr) as MIDI_FILE ptr
declare function midiFileClose cdecl alias "midiFileClose" (byval pMF as MIDI_FILE ptr) as BOOLINT
declare function midiSongAddSMPTEOffset cdecl alias "midiSongAddSMPTEOffset" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iHours as integer, byval iMins as integer, byval iSecs as integer, byval iFrames as integer, byval iFFrames as integer) as BOOLINT
declare function midiSongAddSimpleTimeSig cdecl alias "midiSongAddSimpleTimeSig" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iNom as integer, byval iDenom as integer) as BOOLINT
declare function midiSongAddTimeSig cdecl alias "midiSongAddTimeSig" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iNom as integer, byval iDenom as integer, byval iClockInMetroTick as integer, byval iNotated32nds as integer) as BOOLINT
declare function midiSongAddKeySig cdecl alias "midiSongAddKeySig" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iKey as tMIDI_KEYSIG) as BOOLINT
declare function midiSongAddTempo cdecl alias "midiSongAddTempo" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iTempo as integer) as BOOLINT
declare function midiSongAddMIDIPort cdecl alias "midiSongAddMIDIPort" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iPort as integer) as BOOLINT
declare function midiSongAddEndSequence cdecl alias "midiSongAddEndSequence" (byval pMF as MIDI_FILE ptr, byval iTrack as integer) as BOOLINT
declare function midiTrackAddRaw cdecl alias "midiTrackAddRaw" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iDataSize as integer, byval pData as UByte ptr, byval bMovePtr as BOOLINT, byval iDeltaTime as integer) as BOOLINT
declare function midiTrackIncTime cdecl alias "midiTrackIncTime" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iDeltaTime as integer, byval bOverridePPQN as BOOLINT) as BOOLINT
declare function midiTrackAddText cdecl alias "midiTrackAddText" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iType as tMIDI_TEXT, byval pTxt as zstring ptr) as BOOLINT
declare function midiTrackAddMsg cdecl alias "midiTrackAddMsg" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iMsg as tMIDI_MSG, byval iParam1 as integer, byval iParam2 as integer) as BOOLINT
declare function midiTrackSetKeyPressure cdecl alias "midiTrackSetKeyPressure" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iNote as integer, byval iAftertouch as integer) as BOOLINT
declare function midiTrackAddControlChange cdecl alias "midiTrackAddControlChange" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iCCType as tMIDI_CC, byval iParam as integer) as BOOLINT
declare function midiTrackAddProgramChange cdecl alias "midiTrackAddProgramChange" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iInstrPatch as integer) as BOOLINT
declare function midiTrackChangeKeyPressure cdecl alias "midiTrackChangeKeyPressure" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iDeltaPressure as integer) as BOOLINT
declare function midiTrackSetPitchWheel cdecl alias "midiTrackSetPitchWheel" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iWheelPos as integer) as BOOLINT
declare function midiTrackAddNote cdecl alias "midiTrackAddNote" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iNote as integer, byval iLength as integer, byval iVol as integer, byval bAutoInc as BOOLINT, byval bOverrideLength as BOOLINT) as BOOLINT
declare function midiTrackAddRest cdecl alias "midiTrackAddRest" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval iLength as integer, byval bOverridePPQN as BOOLINT) as BOOLINT
declare function midiTrackGetEndPos cdecl alias "midiTrackGetEndPos" (byval pMF as MIDI_FILE ptr, byval iTrack as integer) as BOOLINT
declare function midiReadGetNumTracks cdecl alias "midiReadGetNumTracks" (byval pMF as MIDI_FILE ptr) as integer
declare function midiReadGetNextMessage cdecl alias "midiReadGetNextMessage" (byval pMF as MIDI_FILE ptr, byval iTrack as integer, byval pMsg as MIDI_MSG ptr) as BOOLINT
declare sub midiReadInitMessage cdecl alias "midiReadInitMessage" (byval pMsg as MIDI_MSG ptr)
declare sub midiReadFreeMessage cdecl alias "midiReadFreeMessage" (byval pMsg as MIDI_MSG ptr)

#endif
