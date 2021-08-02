''
''
'' midiutil -- header translated with help of SWIG FB wrapper
''
'' NOTICE: This file is part of the FreeBASIC Compiler package and can't
''         be included in other distributions without authorization.
''
''
#ifndef __midiutil_bi__
#define __midiutil_bi__

#define CHORD_ROOT_MASK &h000000ff
#define CHORD_TYPE_MASK &h0000ff00
#define CHORD_BASS_MASK &h00ff0000
#define CHORD_ADDITION_MASK &hff000000
#define CHORD_TYPE_MAJOR &h00000100
#define CHORD_TYPE_MINOR &h00000200
#define CHORD_TYPE_AUG &h00000300
#define CHORD_TYPE_DIM &h00000400
#define CHORD_ADD_7TH &h01000000
#define CHORD_ADD_9TH &h02000000
#define CHORD_ADD_MAJ7TH &h04000000

declare function muGetInstrumentName cdecl alias "muGetInstrumentName" (byval pName as zstring ptr, byval iInstr as integer) as BOOL
declare function muGetDrumName cdecl alias "muGetDrumName" (byval pName as zstring ptr, byval iInstr as integer) as BOOL
declare function muGetMIDIMsgName cdecl alias "muGetMIDIMsgName" (byval pName as zstring ptr, byval iMsg as tMIDI_MSG) as BOOL
declare function muGetControlName cdecl alias "muGetControlName" (byval pName as zstring ptr, byval iCC as tMIDI_CC) as BOOL
declare function muGetKeySigName cdecl alias "muGetKeySigName" (byval pName as zstring ptr, byval iKey as tMIDI_KEYSIG) as BOOL
declare function muGetTextName cdecl alias "muGetTextName" (byval pName as zstring ptr, byval iEvent as tMIDI_TEXT) as BOOL
declare function muGetMetaName cdecl alias "muGetMetaName" (byval pName as zstring ptr, byval iEvent as tMIDI_META) as BOOL
declare function muGetNoteFromName cdecl alias "muGetNoteFromName" (byval pName as zstring ptr) as integer
declare function muGetNameFromNote cdecl alias "muGetNameFromNote" (byval pStr as zstring ptr, byval iNote as integer) as zstring ptr
declare function muGetFreqFromNote cdecl alias "muGetFreqFromNote" (byval iNote as integer) as single
declare function muGetNoteFromFreq cdecl alias "muGetNoteFromFreq" (byval fFreq as single) as integer
declare function muGuessChord cdecl alias "muGuessChord" (byval pNoteStatus as integer ptr, byval channel as integer, byval lowRange as integer, byval highRange as integer) as integer
declare function muGetChordName cdecl alias "muGetChordName" (byval str as zstring ptr, byval chord as integer) as zstring ptr

#endif
